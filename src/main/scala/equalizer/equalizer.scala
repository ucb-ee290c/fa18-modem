package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._


trait EqualizerParams[T <: Data] {
  val protoIQ: DspComplex[T]
  val mu: Double
  val pilots: Seq[Int]
  val nSubcarriers: Int
  // val dataCarriers: Seq[Int]
}

case class FixedEqualizerParams(
  width: Int,
  mu: Double = 0.25,
  pilots: Seq[Int] = Seq(5, 21, 43, 59),
  // Default to non-fft-shifted output from fft block, 802.11a mask
  carrierMask: Seq[Boolean] = Seq.fill(1)(false) ++ Seq.fill(27)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(5)(false) ++ Seq.fill(27)(true)
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
  def apply[T <: Data](params: ChannelInverterParams[T]): ChannelInverterIO[T] = new ChannelInverterIO[T](params)
}

 /**
  * ChannelInverter module
  */
class ChannelInverter[T <: Data : Real : BinaryRepresentation](params: EqualizerParams[T]) extends Module {
  val io = IO(ChannelInverterIO(params))

  val cordicParams = new FixedCordicParams(xyWidth=params.protoIQ.width, zWidth=params.protoIQ.width + 4, correctGain=true, stagesPerCycle=4)
  val toPolar = Module(new PipelinedCordic(cordicParams))
  val toCartesian = Module(new PipelinedCordic(cordicParams))
  val cordicDelay = toPolar.cycles

  val dividerWidth = params.protoIQ.width
  val dividerConversionDelay = 2
  val divider = Module(new PipelinedDivider(n=dividerStages, conversionDelay=dividerConversionDelay))
  val dividerDelay = dividerWidth + 1 + dividerConversionDelay

  val delay = cordicDelay * 2 + dividerDelay

  toPolar.io.in.valid := io.in.valid
  toPolar.io.in.bits.vectoring := true.B
  toPolar.io.in.bits.x := io.in.bits.iq.real
  toPolar.io.in.bits.y := io.in.bits.iq.imag

  divider.io.in.valid      := toPolar.io.out.valid
  divider.io.in.bits.num   := Real[T].fromDouble(1.0).asUInt()
  divider.io.in.bits.denom := toPolar.io.out.x

  toCartesian.io.in.valid  := divider.out.valid
  toCartesian.io.in.bits.x := divider.out.bits
  toCartesian.io.in.bits.y := Real[T].zero
  toCartesian.io.in.bits.z := -toPolar.io.out.bits.z
  toCartesian.io.in.bits.vectoring := false.B

  io.out.valid := toCartesian.out.valid
  io.out.bits.iq.real := toCartesian.io.out.x
  io.out.bits.iq.imag := toCartesian.io.out.y
}
object ChannelInverter {
  def apply[T <: Data](in: IQBundle[T], params: EqualizerParams[T]): ChannelInverter[T] = {
    val ci = Module(new ChannelInverter(params))
    ci.io.in <> in
    ci.io.out
  }
}

class Equalizer[T <: Data : Real : BinaryRepresentation](params: EqualizerParams[T]) extends Module {
  // Calculate useful stuff based on params
  val nLTFCarriers = params.carrierMask.map(c => if (c) 1 else 0).reduce(_ + _)
  val ltfIdxs = params.carrierMask zip (0 until params.carrierMask.length) filter {case (b, i) => b} map {case (b, i) => i}
  // IO
  val io = IO(EqualizerIO(params))
  // State machine values
  val sLTS1 :: sLTS2 :: sInvert :: sCorrect :: sError :: Nil = Enum(5)
  val state = RegInit(sLTS1)
  val nextState = Wire(sLTS1.cloneType)
  state := nextState
  // Counters for inversion state
  val invertInCounter = Wire(UInt(log2Ceil(params.nSubcarriers).W))
  val invertOutCounter = Wire(UInt(log2Ceil(params.nSubcarriers).W))
  invertInCounter := 0.U; invertOutCounter := 0.U
  // Register for tracking start of packets
  val pktStartReg = Reg(Bool())
  pktStartReg := false.B
  // Storage for channel weights
  val correction = RegInit(VecInit(
    Seq.fill(params.nSubcarriers)(DspComplex[T].wire(ConvertableTo[T].fromDouble(1.0),
                                                     ConvertableTo[T].fromDouble(0.0))
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
      io.in.ready := true.B
      nextState := Mux(io.in.fire(),
                       Mux(io.in.bits.pktStart, sLTS2, sError), // This better be the start of a new packet
                       sLTS1)
      dataBuf := io.in.bits.iq
    }
    is(sLTS2) {
      io.in.ready := true.B
      nextState := Mux(io.in.fire(), sInvert, sLTS2)
      val ltsAverage = (0 until params.nSubcarriers).map(i => ((dataBuf(i) * io.in.bits.iq(i)) >> 2) * IEEE80211.ltfFreq(i))
      dataBuf := Mux(!io.in.fire(), ltsAverage, dataBuf)
      invertInCounter := 0.U
      invertOutCounter := 0.U
    }
    is(sInvert) {
      io.in.ready := false.B
      nextState := Mux(invertOutCounter < params.nLTFCarriers.U, sInvert, sCorrect)
      invertInCounter := invertInCounter + 1.U
      ciBundle = Valid(IQBundle(dataBuf(ltfIdxs(invertInCounter))))
      ciBundle.valid := invertInCounter < nLTFCarriers.U
      val inverter = ChannelInverter(ciBundle, params)
      invertOutCounter := invertOutCounter + inverter.valid
      correction(invertOutCounter) := inverter.iq
      pktStartReg := true.B
    }
    is(sCorrect) {
      io.in.ready := true.B
      nextState := Mux(io.in.fire() && io.in.bits.pktEnd, sLTS1, sCorrect)
      // Should probably check for io.out.ready and handle it? Right now the sample will just be dropped.
      io.out.valid := io.in.fire()
      io.out.bits.pktEnd := io.in.bits.pktEnd
    }
    is(sError) {
      io.in.ready := true.B  // this has to be true to bring in IQ until the end of the packet
      printf("ERROR STATE!")
      nextState := Mux(io.in.fire() && io.in.bits.pktEnd, sLTS1, sError)
    }
  }
}
