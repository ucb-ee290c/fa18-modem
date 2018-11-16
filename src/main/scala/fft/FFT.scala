package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

import dsptools.numbers._

import breeze.numerics.{cos, sin}
import breeze.signal.{fourierTr}
import breeze.linalg.{DenseVector}
import breeze.math.Complex
import scala.math._

/**
 * Base class for FFT parameters
 *
 * These are type generic
 */
trait FFTParams[T <: Data] extends PacketBundleParams[T] {
  val numPoints    : Int
  val protoTwiddle : DspComplex[T]
  val pipeline     : Boolean
  val fftType      : String
  lazy val width = numPoints

  final val allowedFftTypes = Seq("direct", "sdf")
}
object FFTParams {
  def apply[T <: Data](old_params: FFTParams[T], new_num_points: Int): FFTParams[T] = new FFTParams[T] {
    val protoIQ      = old_params.protoIQ
    val protoTwiddle = old_params.protoTwiddle
    val numPoints    = new_num_points
    val pipeline     = old_params.pipeline
    val fftType      = old_params.fftType
  }
}

/**
 * FFT parameters object for fixed-point FFTs
 */
case class FixedFFTParams(
  // width of Input and Output
  dataWidth: Int,
  // width of twiddle constants
  twiddleWidth: Int,
  maxVal: Int,
  numPoints: Int = 4,
  pipeline: Boolean = false,
  fftType: String = "direct"
) extends FFTParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, (dataWidth-2-log2Ceil(maxVal)).BP))
  val protoTwiddle = DspComplex(FixedPoint(twiddleWidth.W, (twiddleWidth-2).BP))
}


/**
 * Bundle type as IO for PacketBundles
 */

// serial input, serial output
class SISOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(SerialPacketBundle(params)))
  val out = Decoupled(SerialPacketBundle(params))

  override def cloneType: this.type = SISOIO(params).asInstanceOf[this.type]
}
object SISOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): SISOIO[T] = new SISOIO(params)
}

// serial input, deserial output
class SIDOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(SerialPacketBundle(params)))
  val out = Decoupled(DeserialPacketBundle(params))

  override def cloneType: this.type = SIDOIO(params).asInstanceOf[this.type]
}
object SIDOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): SIDOIO[T] = new SIDOIO(params)
}

// deserial input, serial output
class DISOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(DeserialPacketBundle(params)))
  val out = Decoupled(SerialPacketBundle(params))

  override def cloneType: this.type = DISOIO(params).asInstanceOf[this.type]
}
object DISOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): DISOIO[T] = new DISOIO(params)
}

// deserial input, deserial output
class DIDOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(DeserialPacketBundle(params)))
  val out = Decoupled(DeserialPacketBundle(params))

  override def cloneType: this.type = DIDOIO(params).asInstanceOf[this.type]
}
object DIDOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): DIDOIO[T] = new DIDOIO(params)
}

class FFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(SIDOIO(params))
  require(params.allowedFftTypes.contains(params.fftType), s"""FFT type must be one of the following: ${params.allowedFftTypes.mkString(", ")}""")

  params.fftType match {
    case "direct" => {
      val deser = Module(new Deserializer(SerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft = Module(new DirectFFT(params))
      deser.io.in <> io.in
      fft.io.in   <> deser.io.out
      io.out      <> fft.io.out
    }
    case "sdf" => {
      val deser = Module(new Deserializer(SerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft = Module(new SDFFFT(params))
      fft.io.in   <> io.in
      deser.io.in <> fft.io.out
      io.out      <> deser.io.out
    }
  }
}

class DirectFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(DIDOIO(params))
  val fft_stage = {
    if (params.numPoints != 2 && FFTUtil.is_prime(params.numPoints)) { Module(new RaderFFT(params)) }
    else                                                             { Module(new FFTStage(params)) }
  }
  fft_stage.io.in.bits  := io.in.bits
  fft_stage.io.in.valid := io.in.fire()

  if (params.pipeline) {
    // FIXME
    // val out_fifo = Module(new Queue(SerialPacketBundle(params), params.numPoints))
    io.in.ready  := io.out.ready
    io.out.bits  := fft_stage.io.out.bits
    io.out.valid := fft_stage.io.out.valid
  } else {
    io.in.ready  := io.out.ready
    io.out.bits  := fft_stage.io.out.bits
    io.out.valid := fft_stage.io.out.valid
  }
}

class IFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val scale : Boolean = true) extends Module {
  val io = IO(DISOIO(params))

  require(params.allowedFftTypes.contains(params.fftType), s"""FFT type must be one of the following: ${params.allowedFftTypes.mkString(", ")}""")

  val fft_in = Wire(io.in.cloneType)
  val fft_out = Wire(io.out.cloneType)

  // Bulk connect, but iq will be overridden in a following block of code
  fft_in  <> io.in
  fft_out <> io.out

  val scalar = ConvertableTo[T].fromDouble(1.0 / params.numPoints.toDouble)

  io.in.bits.iq.zip(fft_in.bits.iq).foreach {
    case (io_in, fft_inp) => {
      fft_inp.real := io_in.imag
      fft_inp.imag := io_in.real
    }
  }

  if (scale) {
    io.out.bits.iq.real := fft_out.bits.iq.imag * scalar
    io.out.bits.iq.imag := fft_out.bits.iq.real * scalar
  } else {
    io.out.bits.iq := fft_out.bits.iq
  }

  params.fftType match {
    case "direct" => {
      val ser = Module(new Serializer(SerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft = Module(new DirectFFT(params))
      fft_in    <> fft.io.in
      ser.io.in <> fft.io.out
      fft_out   <> ser.io.out
    }
    case "sdf" => {
      val ser = Module(new Serializer(SerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft = Module(new SDFFFT(params))
      ser.io.in <> fft_in
      fft.io.in <> ser.io.out
      fft_out   <> fft.io.out
    }
  }
}


class FFTStageIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Valid(DeserialPacketBundle(params)))
  val out = Valid(DeserialPacketBundle(params))

  override def cloneType: this.type = FFTStageIO(params).asInstanceOf[this.type]
}
object FFTStageIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): FFTStageIO[T] =
    new FFTStageIO(params)
}
class FFTStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  require(isPow2(params.numPoints), "number of points must be a power of 2")
  val io = IO(FFTStageIO(params))

  val numPointsDiv2 = params.numPoints / 2
  // twiddling
  val twiddles_seq = Seq.fill(numPointsDiv2)(Wire(params.protoTwiddle.cloneType))
  (0 until numPointsDiv2).map(n => {
    twiddles_seq(n).real := Real[T].fromDouble( cos(2 * Pi / params.numPoints * n))
    twiddles_seq(n).imag := Real[T].fromDouble(-sin(2 * Pi / params.numPoints * n))
  })

  val butterfly_inputs = Wire(Vec(params.numPoints, params.protoIQ.cloneType))

  if (params.numPoints == 2) {
    butterfly_inputs := io.in.bits.iq

    io.out.bits.pktStart := io.in.bits.pktStart
    io.out.bits.pktEnd   := io.in.bits.pktEnd
    io.out.valid         := io.in.valid
  }
  else {
    val new_params = FFTParams(params, numPointsDiv2)
    val fft_even = Module(new FFTStage(new_params))
    val fft_odd  = Module(new FFTStage(new_params))
    fft_even.io.in.bits.iq := io.in.bits.iq.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    fft_odd.io.in.bits.iq  := io.in.bits.iq.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)

    if (params.pipeline) {
      // Register the outputs of sub-fft stages
      butterfly_inputs := RegNext(fft_even.io.out.bits.iq) ++ RegNext(fft_odd.io.out.bits.iq)
    } else {
      butterfly_inputs := fft_even.io.out.bits.iq ++ fft_odd.io.out.bits.iq
    }

    fft_even.io.in.bits.pktStart := io.in.bits.pktStart
    fft_odd.io.in.bits.pktStart  := io.in.bits.pktStart
    fft_even.io.in.bits.pktEnd   := io.in.bits.pktEnd
    fft_odd.io.in.bits.pktEnd    := io.in.bits.pktEnd
    fft_even.io.in.valid         := io.in.valid
    fft_odd.io.in.valid          := io.in.valid

    if (params.pipeline) {
      // Register the outputs of sub-fft stages
      io.out.bits.pktStart := RegNext(fft_even.io.out.bits.pktStart)
      io.out.bits.pktEnd   := RegNext(fft_even.io.out.bits.pktEnd)
      io.out.valid         := RegNext(fft_even.io.out.valid, init=false.B)
    } else {
      io.out.bits.pktStart := fft_even.io.out.bits.pktStart
      io.out.bits.pktEnd   := fft_even.io.out.bits.pktEnd
      io.out.valid         := fft_even.io.out.valid
    }
  }

  (0 until numPointsDiv2).map(n => {
    val butterfly_outputs = Butterfly[T](Seq(butterfly_inputs(n), butterfly_inputs(n + numPointsDiv2)), twiddles_seq(n))
    io.out.bits.iq(n)                 := butterfly_outputs(0)
    io.out.bits.iq(n + numPointsDiv2) := butterfly_outputs(1)
  })
}

class IFFTStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val scale : Boolean = true) extends Module {
  require(isPow2(params.numPoints), "number of points must be a power of 2")
  val io = IO(FFTStageIO(params))

  val fft = Module(new FFTStage(params))

  val scalar = ConvertableTo[T].fromDouble(1.0 / params.numPoints.toDouble)

  // Connect valid, pktStart, and pktEnd signals, but iq will be overridden in the following block of code
  fft.io.in <> io.in
  io.out <> fft.io.out

  io.in.bits.iq.zip(io.out.bits.iq).zipWithIndex.foreach {
    case ((inp, out), index) => {
      fft.io.in.bits.iq(index).real := inp.imag
      fft.io.in.bits.iq(index).imag := inp.real

      if (scale) {
        out.real := fft.io.out.bits.iq(index).imag * scalar
        out.imag := fft.io.out.bits.iq(index).real * scalar
      } else {
        out.real := fft.io.out.bits.iq(index).imag
        out.imag := fft.io.out.bits.iq(index).real
      }
    }
  }
}

class RaderFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  require(FFTUtil.is_prime(params.numPoints), "number of points must be prime")
  val io = IO(FFTStageIO(params))

  val sub_fft_size = if (isPow2(params.numPoints - 1)) params.numPoints - 1 else scala.math.pow(2, log2Ceil(2 * params.numPoints - 3)).toInt
  val pad_length = sub_fft_size - (params.numPoints - 1)
  val g = FFTUtil.primitive_root(params.numPoints)
  val g_inv = FFTUtil.mult_inv(g, params.numPoints)
  val inv_idx_map = (0 until params.numPoints - 1).map(scala.math.pow(g_inv, _).toInt % params.numPoints)
  val idx_map = (0 until params.numPoints - 1).map(scala.math.pow(g, _).toInt % params.numPoints)

  val pad_length_quot = pad_length / (params.numPoints - 1)
  val pad_length_rem = pad_length % (params.numPoints - 1)
  val twiddles = inv_idx_map.map(x => Complex(cos(2 * Pi / params.numPoints * x), -sin(2 * Pi / params.numPoints * x)) / sub_fft_size).toArray
  val twiddles_extended = (0 until pad_length_quot).foldRight(twiddles)((_, list) => list ++ twiddles) ++ twiddles.slice(0, pad_length_rem)
  val twiddles_fft = fourierTr(DenseVector(twiddles_extended)).toScalaVector

  val sub_fft_params = FFTParams(params, sub_fft_size)

  val sub_fft = Module(new FFTStage(sub_fft_params))

  sub_fft.io.in.bits.iq.zipWithIndex.foreach {
    case (sub_inp, index) => {
      if (index == 0) {
        sub_inp := io.in.bits.iq(idx_map(index).U)
      }
      else if (index <= pad_length) {
        sub_inp.real := Ring[T].zero
        sub_inp.imag := Ring[T].zero
      }
      else {
        sub_inp := io.in.bits.iq(idx_map(index - pad_length).U)
      }
    }
  }

  sub_fft.io.in.bits.pktStart := io.in.bits.pktStart
  sub_fft.io.in.bits.pktEnd   := io.in.bits.pktEnd
  sub_fft.io.in.valid         := io.in.valid

  io.out.bits.iq(0.U) := io.in.bits.iq(0.U) + sub_fft.io.out.bits.iq(0.U)

  val sub_ifft = Module(new IFFTStage(sub_fft_params, false))
  sub_ifft.io.in.bits.iq.zip(sub_fft.io.out.bits.iq).zip(twiddles_fft).foreach {
    case ((sub_ifft_in, sub_fft_out), twiddle) => {
      val twiddle_wire = Wire(params.protoIQ.cloneType)
      twiddle_wire.real := Real[T].fromDouble(twiddle.real)
      twiddle_wire.imag := Real[T].fromDouble(twiddle.imag)
      sub_ifft_in := sub_fft_out * twiddle_wire
    }
  }

  sub_ifft.io.in.bits.pktStart := sub_fft.io.out.bits.pktStart
  sub_ifft.io.in.bits.pktEnd   := sub_fft.io.out.bits.pktEnd
  sub_ifft.io.in.valid         := sub_fft.io.out.valid

  (0 until params.numPoints - 1).map(n => {
    io.out.bits.iq(inv_idx_map(n).U) := io.in.bits.iq(0.U) + sub_ifft.io.out.bits.iq(n.U)
  })

  io.out.bits.pktStart := sub_ifft.io.out.bits.pktStart
  io.out.bits.pktEnd   := sub_ifft.io.out.bits.pktEnd
  io.out.valid         := sub_ifft.io.out.valid
}

// single radix-2 butterfly
object Butterfly {
  def apply[T <: Data : Real](in: Seq[DspComplex[T]]): Seq[DspComplex[T]] = 
  {
    require(in.length == 2, "Butterfly requires two data inputs")
    Seq(in(0) + in(1), in(0) - in(1))
  }
  def apply[T <: Data : Real](in: Seq[DspComplex[T]], twiddle: DspComplex[T]): Seq[DspComplex[T]] =
  {
    require(in.length == 2, "Butterfly requires two data inputs")
    val product = in(1) * twiddle
    Seq(in(0) + product, in(0) - product)
  }
}

class SDFStageIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Input(SerialPacketBundle(params))
  val out = Output(SerialPacketBundle(params))
  val twiddles_rom = Input(Vec(params.numPoints / 2, params.protoTwiddle.cloneType))
  val cntr = Input(UInt(log2Up(params.numPoints).W))

  override def cloneType: this.type = SDFStageIO(params).asInstanceOf[this.type]
}
object SDFStageIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): SDFStageIO[T] =
    new SDFStageIO(params)
}
class SDFStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val delayLog2: Int, val rom_shift: Int = 0, val dit: Boolean = true) extends Module {
  require(isPow2(params.numPoints), "number of points must be a power of 2")
  require(delayLog2 >= 0, "delay (log-2) must be non-negative")

  val io = IO(SDFStageIO(params))

  val delay = scala.math.pow(2, delayLog2).toInt

  val twiddle = io.twiddles_rom((io.cntr - delay.U) << rom_shift.U)
  val use_twiddle = io.cntr > delay.U
  val inp = Wire(params.protoIQ.cloneType)
  val out = Wire(params.protoIQ.cloneType)

  // Apply twiddle factor at the input or output, depending on whether it's decimation-in-time or decimation-in-frequency
  if (dit) {
    // Issue: using `inp := Mux(use_twiddle, io.in.iq * twiddle, io.in.iq` causes the following error:
    // can't create Mux with non-equivalent types dsptools.numbers.DspComplex@________ and dsptools.numbers.DspComplex@________
    when (use_twiddle) {
      inp := io.in.iq * twiddle
    } .otherwise {
      inp := io.in.iq
    }
    io.out.iq := out
  } else {
    inp := io.in.iq
    when (use_twiddle) {
      io.out.iq := out * twiddle
    } .otherwise {
      io.out.iq := out
    }
  }

  val butterfly_outputs = Seq.fill(2)(Wire(params.protoIQ.cloneType))

  val load_input = io.cntr < delay.U
  val shift_in = Mux(load_input, inp, butterfly_outputs(1))
  val shift_out = ShiftRegister(shift_in, delay)

  Butterfly[T](Seq(shift_out, inp)).zip(butterfly_outputs).foreach {
    case (out_val, out_wire) => out_wire := out_val
  }

  out := Mux(load_input, shift_out, butterfly_outputs(0))

  io.out.pktStart := ShiftRegister(io.in.pktStart, delay)
  io.out.pktEnd   := ShiftRegister(io.in.pktEnd  , delay)
}

class FFTUnscramblerIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(SerialPacketBundle(params)))
  val out = Decoupled(SerialPacketBundle(params))

  override def cloneType: this.type = new FFTUnscramblerIO(params).asInstanceOf[this.type]
}
class FFTUnscrambler[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(new FFTUnscramblerIO(params))

  val serdes_params = SerDesParams(params.protoIQ.cloneType, params.numPoints)

  val des = Module(new Deserializer(serdes_params))
  val ser = Module(new Serializer(serdes_params))

  des.io.in <> io.in

  ser.io.in.valid         := des.io.out.valid
  des.io.out.ready        := ser.io.in.ready
  ser.io.in.bits.pktStart := des.io.out.bits.pktStart
  ser.io.in.bits.pktEnd   := des.io.out.bits.pktEnd

  (0 until params.numPoints).foreach {
    case (index) => {
      val index_bits = (0 until log2Up(params.numPoints - 1)).map(scala.math.pow(2, _).toInt).map(i => (index % (2 * i)) / i).reverse
      val reversed_index = index_bits.foldRight(0)(_ + 2 * _)
      ser.io.in.bits.iq(reversed_index.U) := des.io.out.bits.iq(index.U)
    }
  }

  io.out <> ser.io.out
}

class SDFFFTIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(SerialPacketBundle(params)))
  val out = Decoupled(SerialPacketBundle(params))

  override def cloneType: this.type = SDFFFTIO(params).asInstanceOf[this.type]
}
object SDFFFTIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): SDFFFTIO[T] =
    new SDFFFTIO(params)
}
class SDFFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val dit: Boolean = true) extends Module {
  require(isPow2(params.numPoints), "number of points must be a power of 2")

  val io = IO(SDFFFTIO(params))

  val numPointsDiv2 = params.numPoints / 2

  val twiddles_rom = Wire(Vec(numPointsDiv2, params.protoTwiddle.cloneType))
  (0 until numPointsDiv2).map(n => {
    twiddles_rom(n).real := Real[T].fromDouble( cos(2 * Pi / params.numPoints * n))
    twiddles_rom(n).imag := Real[T].fromDouble(-sin(2 * Pi / params.numPoints * n))
  })

  val unscrambler = Module(new FFTUnscrambler(params))

  unscrambler.io.in <> io.in

  val numStages = log2Up(params.numPoints)

  val delayLog2s = (0 until numStages)
  val delays = delayLog2s.map(d => scala.math.pow(2, d).toInt)
  val cumulative_delays = delays.scanLeft(0)(_ + _)

  val sIdle :: sComp :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  val cntr = RegInit(0.U(log2Up(params.numPoints).W))
  val cntr_next = Wire(cntr.cloneType)

  val out_fifo = Module(new Queue(SerialPacketBundle(params), params.numPoints))

  val sdf_stages = (0 until numStages).map(i => {
    val stage = Module(new SDFStage(params, delayLog2s(i), numStages - 1 - i, true))
    stage.io.twiddles_rom := twiddles_rom
    stage.io.cntr := (cntr - cumulative_delays(i).U)(i, 0)
    stage
  })

  sdf_stages.map(_.io).foldLeft(RegNext(unscrambler.io.out.bits))((stg_in, stg_io) => {
    stg_io.in := stg_in
    stg_io.out
  })

  out_fifo.io.enq.bits := sdf_stages.last.io.out
  out_fifo.io.enq.valid := ShiftRegister(unscrambler.io.out.fire(), cumulative_delays.last + 1)
  unscrambler.io.out.ready := out_fifo.io.enq.ready

  io.out <> out_fifo.io.deq

  cntr_next  := cntr
  state_next := state

  switch (state) {
    is (sIdle) {
      when (unscrambler.io.out.fire()) { state_next := sComp }
    }
    is (sComp) {
      cntr_next := cntr + 1.U
      when (cntr === (params.numPoints - 2).U) { state_next := sDone }
    }
    is (sDone) {
      when (unscrambler.io.out.fire()) { state_next := sComp }
      .elsewhen (io.out.fire())        { state_next := sIdle }
    }
  }

  when (state_next === sComp && state =/= sComp) {
    // Reset counter
    cntr_next := 0.U
  }

  cntr  := cntr_next
  state := state_next
}

// DFT
object DFT {
  def apply[T <: Data : Real](in: Seq[DspComplex[T]]): Seq[DspComplex[T]] = 
  {
    require(in.length == 2, "2-point DFT only for no defined twiddle type")
    Seq(in(0) + in(1), in(0) - in(1))
  }
  def apply[T <: Data : Real](in: Seq[DspComplex[T]], genTwiddle: DspComplex[T]): Seq[DspComplex[T]] = 
  {
    in.length match {
      case 2 => apply(in)
      case 3 => {
        val w = Complex(cos(2 * Pi / 3), -sin(2 * Pi / 3))
        val w_wire = Wire(genTwiddle.cloneType)
        w_wire := DspComplex.wire(Real[T].fromDouble(w.real), Real[T].fromDouble(w.imag))
        val product = (in(1) - in(2)) * w_wire
        Seq(in.reduce(_ + _), in(0) + product, in(0) - product)
      }
      case _ => {
        val twiddles_seq = Seq.fill(in.length)(Wire(genTwiddle.cloneType))
        (0 until in.length).map(n => {
          twiddles_seq(n).real := Real[T].fromDouble( cos(2 * Pi / in.length * n))
          twiddles_seq(n).imag := Real[T].fromDouble(-sin(2 * Pi / in.length * n))
        })
        Seq.tabulate(in.length)(k => {
          in.zipWithIndex.map {
            case (inp, n) => inp * twiddles_seq((k * n) % in.length)
          }.reduce(_ + _)
        })
      }
    }
  }
}
