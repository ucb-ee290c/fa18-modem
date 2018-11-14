package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._


trait EqualizerParams[T <: Data] {
  val protoIQ: DspComplex[T]
  val mu: Double
  val pilots: Seq[Int]
  val carrierMask: Seq[Boolean]
  val nSubcarriers: Int
  // val dataCarriers: Seq[Int]
}

case class FixedEqualizerParams(
  width: Int,
  mu: Double = 0.25,
  pilots: Seq[Int] = Seq(5, 21, 43, 59),
  // Default to non-fft-shifted output from fft block, 802.11a mask
  carrierMask: Seq[Boolean] = Seq.fill(1)(false) ++ Seq.fill(27)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(5)(false) ++ Seq.fill(27)(true),
  nSubcarriers: Int = 64
) extends EqualizerParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(width.W, (width-3).BP)).cloneType
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

  // printf("cordic1 valid %d\n", toPolar.io.out.valid)
  // printf("polar angle %d\n", toPolar.io.out.bits.z.asUInt())

  val phaseDelay = ShiftRegister(in=toPolar.io.out.bits.z, n=dividerDelay)

  divider.io.in.valid      := toPolar.io.out.valid
  divider.io.in.bits.num   := Real[T].fromDouble(1.0).asUInt() << params.protoIQ.real.getWidth - 4 //TODO: make this not hardcoded
  divider.io.in.bits.denom := toPolar.io.out.bits.x.asUInt()

  // printf("divider denom %d\n", divider.io.in.bits.denom)
  // printf("divider out %d\n", divider.io.out.bits)
  // printf("division valid %d\n", divider.io.out.valid)

  // printf("cordic2 angle %d\n", phaseDelay.asUInt())

  toCartesian.io.in.valid  := divider.io.out.valid
  toCartesian.io.in.bits.x := divider.io.out.bits.asFixedPoint((params.protoIQ.real.getWidth - 3).BP) //TODO: make this not hardcoded
  toCartesian.io.in.bits.y := Real[T].zero
  toCartesian.io.in.bits.z := -phaseDelay
  toCartesian.io.in.bits.vectoring := false.B
  toCartesian.io.out.ready := true.B

  // printf("cordic2 valid %d\n", toCartesian.io.out.valid)
  // printf("cordic2 x %d\n", toCartesian.io.out.bits.x.asUInt())
  // printf("cordic2 y %d\n", toCartesian.io.out.bits.y.asUInt())

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
  val invertInCounter = Reg(UInt(log2Ceil(params.nSubcarriers).W))
  val invertOutCounter = Reg(UInt(log2Ceil(params.nSubcarriers).W))
  invertInCounter := 0.U; invertOutCounter := 0.U
  // Register for tracking start of packets
  val pktStartReg = Reg(Bool())
  pktStartReg := false.B
  // Storage for channel weights
  val correction = RegInit(VecInit(
    Seq.fill(params.nSubcarriers)(DspComplex.wire(Real[T].fromDouble(1.0),
                                                  Real[T].fromDouble(0.0))
                                  )
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
  dataBuf := io.in.bits.iq
  switch(state) {
    is(sLTS1) {
      printf("LTS1 STATE\n")
      io.in.ready := true.B
      nextState := Mux(io.in.fire(),
                       Mux(io.in.bits.pktStart, sLTS2, sError), // This better be the start of a new packet
                       sLTS1)
      dataBuf := io.in.bits.iq
    }
    is(sLTS2) {
      printf("LTS2 STATE\n")
      io.in.ready := true.B
      nextState := Mux(io.in.fire(), sInvert, sLTS2)
      // Make sure we have the correct sign by multiplying by the table entries (assumes LTF is either 0, 1, or -1)
      val ltsAverage = (0 until params.nSubcarriers).map(i => Mux(ltfTable(i).real > Real[T].zero,
                                                                  dataBuf(i).div2(1) + io.in.bits.iq(i).div2(1),
                                                                  -(dataBuf(i).div2(1) + io.in.bits.iq(i).div2(1))))
      // dataBuf := Mux(!io.in.fire(), VecInit(ltsAverage), dataBuf)
      when(!io.in.fire()) {
        dataBuf := ltsAverage
      }.otherwise {
        dataBuf := dataBuf
      }
      invertInCounter := 0.U
      invertOutCounter := 0.U
    }
    is(sInvert) {
      printf("INVERT STATE\n")
      io.in.ready := false.B
      nextState := Mux(invertOutCounter < nLTFCarriers.U, sInvert, sCorrect)
      invertInCounter := invertInCounter + 1.U
      val ciBundle = Wire(Valid(IQBundle(params.protoIQ)))
      ciBundle.bits.iq := dataBuf(ltfIdxs(invertInCounter))
      ciBundle.valid := invertInCounter < nLTFCarriers.U

      printf("inverter in %d\n", dataBuf(ltfIdxs(invertInCounter)).asUInt())

      val inverter = ChannelInverter(ciBundle, params)
      invertOutCounter := invertOutCounter + inverter.valid
      printf("invert out counter %d\n", invertOutCounter)
      correction(invertOutCounter) := inverter.bits.iq
      pktStartReg := true.B
    }
    is(sCorrect) {
      printf("CORRECTION STATE\n")
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
