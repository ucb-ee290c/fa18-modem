package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._


trait EqualizerParams[T <: Data] extends PacketBundleParams[T] {
  //val mu: Double                  // Unused. In place for pilot-based equalization later.
  //val pilots: Seq[Int]            // Unused. List of subcarrier indices containing channel pilots.
  val carrierMask: Seq[Boolean]   // Mask of subcarriers containing data or a pilot, which should be equalized.
  val nSubcarriers: Int           // Total number of subcarriers coming from the FFT.
}

case class FixedEqualizerParams(
  width: Int = 16,
  binaryPoint: Int = 13,
  //mu: Double = 0.25,
  //pilots: Seq[Int] = Seq(5, 21, 43, 59),
  // Default to non-fft-shifted output from fft block, 802.11a mask
  carrierMask: Seq[Boolean] = Seq.fill(1)(false) ++ Seq.fill(26)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(6)(false) ++ Seq.fill(27)(true),
  nSubcarriers: Int = 64
) extends EqualizerParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(width.W, binaryPoint.BP)).cloneType
}

/**
 * Equalizer IO bundle
 */
class EqualizerIO[T <: Data](params: EqualizerParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params.nSubcarriers, params.protoIQ)))
  val out = Decoupled(PacketBundle(params.nSubcarriers, params.protoIQ))

  override def cloneType: this.type = EqualizerIO(params).asInstanceOf[this.type]
}
object EqualizerIO {
  def apply[T <: Data](params: EqualizerParams[T]): EqualizerIO[T] = new EqualizerIO[T](params)
}

/**
 * ChannelInverter IO bundle
 */
class ChannelInverterIO[T <: Data](params: EqualizerParams[T]) extends Bundle {
  val in = Flipped(Valid(IQBundle(params.protoIQ)))
  val out = Valid(IQBundle(params.protoIQ))

  override def cloneType: this.type = ChannelInverterIO(params).asInstanceOf[this.type]
}
object ChannelInverterIO {
  def apply[T <: Data](params: EqualizerParams[T]): ChannelInverterIO[T] = new ChannelInverterIO[T](params)
}

/**
 * ChannelInverter module
 * Input: An IQ sample v representing a channel tap
 * Output: 1/v, the correction factor for the channel tap
 *         Equivalently, |v|^1 * exp(-angle(v))
 * Valid interface since this module is pipelined to reduce latency when inverting multiple channel taps.
 */
class ChannelInverter[T <: Data : Real : BinaryRepresentation](params: EqualizerParams[T]) extends Module {
  val io = IO(ChannelInverterIO(params))

  // number of cordic stages
  val minNumber = math.pow(2.0, -(params.protoIQ.real.getWidth-2))
  private var n = 0
  while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
    n += 1
  }
  val nCordicStages = n
  val cordicParams = new CordicParams[T]{
    val protoXY=params.protoIQ.real
    val protoZ=params.protoIQ.real
    val correctGain=true
    val stagesPerCycle=4
    val nStages=nCordicStages
    }
  val toPolar = Module(new IterativeCordic(cordicParams))
  val toCartesian = Module(new IterativeCordic(cordicParams))
  val cordicDelay = toPolar.cycles

  val dividerWidth = params.protoIQ.real.getWidth * 2  // Want this to be bigger to handle divisor < dividend case
  val dividerConversionDelay = 2
  val divider = Module(new PipelinedDivider(n=dividerWidth, conversionDelay=dividerConversionDelay))
  val dividerDelay = dividerWidth + 1 + dividerConversionDelay

  val delay = cordicDelay * 2 + dividerDelay

  /**
   * Operations are:
   *   v = (i,q) -> |v|exp(<v) = mag(i,q) * phase(i,q) = (m, p)
   *   m' = 1/mag(i,q)
   *   p' = -phase(i,q)
   *   (m', p') -> (i', q') = v_inverse
   */
  toPolar.io.in.valid := io.in.valid
  toPolar.io.in.bits.vectoring := true.B
  toPolar.io.in.bits.x := io.in.bits.iq.real
  toPolar.io.in.bits.y := io.in.bits.iq.imag
  toPolar.io.in.bits.z := Real[T].zero
  toPolar.io.out.ready := true.B

  val phaseDelay = ShiftRegister(in=toPolar.io.out.bits.z, n=dividerDelay)

  var nShift = 0.BP
  params.protoIQ.real match {
    case f: FixedPoint => nShift = f.binaryPoint
    case t => ???
  }
  divider.io.in.valid      := toPolar.io.out.valid
  divider.io.in.bits.num   := Real[T].fromDouble(1.0).asUInt() << nShift.get - 1 //TODO: make this not hardcoded
  divider.io.in.bits.denom := toPolar.io.out.bits.x.asUInt()

  toCartesian.io.in.valid  := divider.io.out.valid
  params.protoIQ.real match {
    case f: FixedPoint => toCartesian.io.in.bits.x := divider.io.out.bits.asFixedPoint(f.binaryPoint)
    case t => ???
  }
   //TODO: make this not hardcoded
  toCartesian.io.in.bits.y := Real[T].zero
  toCartesian.io.in.bits.z := -phaseDelay
  toCartesian.io.in.bits.vectoring := false.B
  toCartesian.io.out.ready := true.B

  io.out.valid := toCartesian.io.out.valid
  io.out.bits.iq.real := toCartesian.io.out.bits.x
  io.out.bits.iq.imag := toCartesian.io.out.bits.y
}
object ChannelInverter {
  def apply[T <: Data : Real : BinaryRepresentation](in: Valid[IQBundle[T]], params: EqualizerParams[T]): Valid[IQBundle[T]] = {
    val ci = Module(new ChannelInverter(params))
    ci.io.in <> in
    ci.io.out
  }
}

/**
 * Channel Equalizer Module
 *
 * Parameters specified in EqualizerParams above.
 * Implements zero-forcing equalization based on the IEEE 802.11a LTF preamble.
 */
class Equalizer[T <: Data : Real : BinaryRepresentation](params: EqualizerParams[T]) extends Module {
  // Calculate useful stuff based on params
  val nLTFCarriers = params.carrierMask.map(c => if (c) 1 else 0).reduce(_ + _)
  val ltfIdxs = VecInit(params.carrierMask zip (0 until params.carrierMask.length) filter {case (b, i) => b} map {case (b, i) => i.U})
  val ltfWires = Seq.fill(params.nSubcarriers)(Wire(params.protoIQ))
  ltfWires zip (IEEE80211.ltfFreq.toScalaVector) foreach {
    case (w, x) => w := DspComplex.wire(Real[T].fromDouble(x.real), Real[T].fromDouble(x.imag))
  }
  val ltfTable = VecInit(ltfWires)
  // IO
  val io = IO(EqualizerIO(params))
  // State machine values
  val sLTS1 :: sLTS2 :: sInvert :: sCorrect :: sError :: Nil = Enum(5)
  val state = RegInit(sLTS1)
  val nextState = Wire(sLTS1.cloneType)
  state := nextState
  nextState := state
  // Counters for inversion state
  // val invertInCounter = Reg(UInt(log2Ceil(params.nSubcarriers).W))
  val invertInCounter = Reg(UInt(32.W))
  // val invertOutCounter = Reg(UInt(log2Ceil(params.nSubcarriers).W))
  val invertOutCounter = Reg(UInt(32.W))
  invertInCounter := 0.U; invertOutCounter := 0.U
  // Register for tracking start of packets
  val pktStartReg = Reg(Bool())
  pktStartReg := false.B
  // Storage for channel weights
  val dspComplexOne = Wire(params.protoIQ)
  dspComplexOne.real := Real[T].fromDouble(1.0)
  dspComplexOne.imag := Real[T].fromDouble(0.0)
  val correction = RegInit(VecInit(
    Seq.fill(params.nSubcarriers)(dspComplexOne)
  ))
  correction := correction // Stays the same except during occasional updates

  // Safe defaults for io
  io.out.valid := false.B
  io.in.ready := false.B
  (0 until params.nSubcarriers) foreach {i => io.out.bits.iq(i) := correction(i) * io.in.bits.iq(i)}
  io.out.bits.pktStart := pktStartReg
  io.out.bits.pktEnd := false.B

  // Body
  val dataBuf = Reg(Vec(params.nSubcarriers, params.protoIQ))
  dataBuf := dataBuf
  switch(state) {
    is(sLTS1) {
      // printf("LTS1 STATE\n")
      io.in.ready := true.B
      nextState := Mux(io.in.fire(),
                       Mux(io.in.bits.pktStart, sLTS2, sError), // This better be the start of a new packet
                       sLTS1)
      dataBuf := io.in.bits.iq
    }
    is(sLTS2) {
      // printf("LTS2 STATE\n")
      io.in.ready := true.B
      nextState := Mux(io.in.fire(), sInvert, sLTS2)
      // Make sure we have the correct sign (assumes LTF is either 0, 1, or -1)
      // Could be extended to arbitrary symbols by providing the inverse LTF constants in ltfTable and multiplying.
      val ltsAverage = (0 until params.nSubcarriers).map(i => Mux(ltfTable(i).real > Real[T].zero,
                                                                  (dataBuf(i) + io.in.bits.iq(i)).div2(1),
                                                                  -(dataBuf(i) + io.in.bits.iq(i)).div2(1)))
      when(io.in.fire()) {  // Can't use a Mux here because ltsAverage and dataBuf are different types
        dataBuf := ltsAverage
      }.otherwise {
        dataBuf := dataBuf
      }
      invertInCounter := 0.U
      invertOutCounter := 0.U
    }
    is(sInvert) {
      // printf("INVERT STATE\n")
      io.in.ready := false.B
      invertInCounter := invertInCounter + 1.U

      val ciBundle = Wire(Valid(IQBundle(params.protoIQ)))
      ciBundle.bits.iq := dataBuf(ltfIdxs(invertInCounter))
      ciBundle.valid := invertInCounter < nLTFCarriers.U && state === sInvert

      val inverter = ChannelInverter(ciBundle, params)
      invertOutCounter := invertOutCounter + inverter.valid
      correction(ltfIdxs(invertOutCounter)) := inverter.bits.iq
      pktStartReg := true.B
      nextState := Mux((invertOutCounter >= (nLTFCarriers - 1).U) && inverter.valid, sCorrect, sInvert)
    }
    is(sCorrect) {
      // printf("CORRECTION STATE\n")
      io.in.ready := true.B
      nextState := Mux(io.in.fire() && io.in.bits.pktEnd, sLTS1, sCorrect)
      // Should probably check for io.out.ready and handle it? Right now the sample will just be dropped.
      io.out.valid := io.in.fire()
      io.out.bits.pktEnd := io.in.bits.pktEnd
    }
    is(sError) {
      io.in.ready := true.B  // this has to be true to bring in IQ until the end of the packet
      printf("ERROR STATE!\n")
      nextState := Mux(io.in.fire() && io.in.bits.pktEnd, sLTS1, sError)
    }
  }
}
