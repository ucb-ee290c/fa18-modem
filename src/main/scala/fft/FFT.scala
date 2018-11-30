package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._
import breeze.numerics.{cos, sin}
import breeze.signal.fourierTr
import breeze.linalg.DenseVector
import breeze.math.Complex
import scala.math._

/**
 * Base class for FFT parameters
 *
 * These are type generic
 */
trait FFTParams[T <: Data] extends PacketBundleParams[T] {
  val numPoints    : Int           // number of points in FFT
  val protoTwiddle : DspComplex[T] // twiddle data type
  val fftType      : String        // type of FFT to use
  val decimType    : String        // if SDF FFT is being used, whether to use DIT, DIF, or whatever is more "optimal"
  val pipeline     : Boolean       // if direct FFT is being used, whether to pipeline the FFT stages
  lazy val width = numPoints       // overrides the `width` parameter of PacketBundleParams

  // Allowed values for some parameters
  final val allowedFftTypes   = Seq("direct", "sdf")
  final val allowedDecimTypes = Seq("dit", "dif", "opt")

  // Common require functions used in FFT blocks
  def checkNumPointsPow2() {
    require(isPow2(numPoints), "number of points must be a power of 2")
  }
  def checkFftType() {
    require(allowedFftTypes.contains(fftType), s"""FFT type must be one of the following: ${allowedFftTypes.mkString(", ")}""")
  }
  def checkDecimType() {
    require(allowedDecimTypes.contains(decimType), s"""Decimation type must be one of the following: ${allowedDecimTypes.mkString(", ")}""")
  }
}
object FFTParams {
  // Override number of points
  def apply[T <: Data](old_params: FFTParams[T], newNumPoints: Int): FFTParams[T] = new FFTParams[T] {
    val protoIQ      = old_params.protoIQ
    val protoTwiddle = old_params.protoTwiddle
    val numPoints    = newNumPoints
    val pipeline     = old_params.pipeline
    val fftType      = old_params.fftType
    val decimType    = old_params.decimType
  }
  // Override decimation type
  def apply[T <: Data](old_params: FFTParams[T], newDecimType: String): FFTParams[T] = new FFTParams[T] {
    val protoIQ      = old_params.protoIQ
    val protoTwiddle = old_params.protoTwiddle
    val numPoints    = old_params.numPoints
    val pipeline     = old_params.pipeline
    val fftType      = old_params.fftType
    val decimType    = newDecimType
  }
}

/**
 * FFT parameters case class for fixed-point FFTs
 */
case class FixedFFTParams(
  dataWidth   : Int, // width of input and output
  binPoint    : Int, // Binary point of input and output
  twiddleWidth: Int, // width of twiddle constants
  numPoints   : Int,
  pipeline    : Boolean = true,
  fftType     : String = "sdf",
  decimType   : String = "opt"
) extends FFTParams[FixedPoint] {
  val protoIQ      = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
  val protoTwiddle = DspComplex(FixedPoint(twiddleWidth.W, (twiddleWidth-2).BP)) // to allow for 1, -1, j, and -j to be expressed.
}

/**
 * Bundle type as IO for various blocks
 *
 * Many blocks use serial/parallel input and serial/parallel output streams
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
  val out = Decoupled(PacketBundle(params.numPoints, params.protoIQ))

  override def cloneType: this.type = SIDOIO(params).asInstanceOf[this.type]
}
object SIDOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): SIDOIO[T] = new SIDOIO(params)
}

// deserial input, serial output
class DISOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params.numPoints, params.protoIQ)))
  val out = Decoupled(SerialPacketBundle(params))

  override def cloneType: this.type = DISOIO(params).asInstanceOf[this.type]
}
object DISOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): DISOIO[T] = new DISOIO(params)
}

// deserial input, deserial output
class DIDOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params.numPoints, params.protoIQ)))
  val out = Decoupled(PacketBundle(params.numPoints, params.protoIQ))

  override def cloneType: this.type = DIDOIO(params).asInstanceOf[this.type]
}
object DIDOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): DIDOIO[T] = new DIDOIO(params)
}


/**
 * Top level FFT
 *
 * Instantiates the correct type of FFT based on parameter value
 */
class FFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(SIDOIO(params))
  params.checkFftType()
  params.checkDecimType()

  params.fftType match {
    case "direct" => {
      // instantiate Deserializer to go from serial-input FFT to parallel-input DirectFFT
      val deser = Module(new Deserializer(SerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft   = Module(new DirectFFT(params))
      deser.io.in <> io.in
      fft.io.in   <> deser.io.out
      io.out      <> fft.io.out
    }
    case "sdf" => {
      val fft = Module(new SDFFFTDeserOut(params))
      fft.io.in  <> io.in
      fft.io.out <> io.out
    }
  }
}

/**
 * Top level IFFT
 *
 * Instantiates the correct type of FFT based on parameter value
 */
class IFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(DISOIO(params))
  params.checkFftType()
  params.checkDecimType()

  val fft_in  = Wire(io.in.cloneType)
  val fft_out = Wire(io.out.cloneType)

  // Bulk connect, but iq will be overridden in a following block of code
  fft_in  <> io.in
  fft_out <> io.out

  io.in.bits.iq.zip(fft_in.bits.iq).foreach {
    case (io_in, fft_inp) => {
      // Swap real and imaginary components
      fft_inp.real := io_in.imag
      fft_inp.imag := io_in.real
    }
  }

  val scalar = ConvertableTo[T].fromDouble(1.0 / params.numPoints.toDouble) // normalization factor
  // Swap real and imaginary components and normalize
  io.out.bits.iq.real := fft_out.bits.iq.imag * scalar
  io.out.bits.iq.imag := fft_out.bits.iq.real * scalar

  params.fftType match {
    case "direct" => {
      // instantiate Serializer to go from parallel-output DirectFFT to serial-output FFT
      val ser = Module(new Serializer(SerDesParams(params.protoIQ.cloneType, params.numPoints)))
      val fft = Module(new DirectFFT(params))
      fft_in    <> fft.io.in
      ser.io.in <> fft.io.out
      fft_out   <> ser.io.out
    }
    case "sdf" => {
      val fft = Module(new SDFFFTDeserIn(params))
      fft.io.in <> fft_in
      fft_out   <> fft.io.out
    }
  }
}

/**************************************************
 * Direct FFT (top level and sub-blocks)
 **************************************************/

/**
 * Top level Direct FFT block
 *
 * FIXME: Rader FFT WIP, but not currently used
 */
class DirectFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(DIDOIO(params))
  val fft_stage = {
    if (params.numPoints != 2 && FFTUtil.is_prime(params.numPoints)) { Module(new RaderFFT(params)) }
    else                                                             { Module(new DirectStage(params)) }
  }
  fft_stage.io.in.bits  := io.in.bits
  fft_stage.io.in.valid := io.in.fire()
  io.in.ready  := io.out.ready
  io.out.bits  := fft_stage.io.out.bits
  io.out.valid := fft_stage.io.out.valid
}

/**
 * Bundle type as IO for direct FFT stage
 */
class DirectStageIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Valid(PacketBundle(params.numPoints, params.protoIQ)))
  val out = Valid(PacketBundle(params.numPoints, params.protoIQ))

  override def cloneType: this.type = DirectStageIO(params).asInstanceOf[this.type]
}
object DirectStageIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): DirectStageIO[T] = new DirectStageIO(params)
}

/**
 * "Stage" for Direct FFT
 *
 * Recursively instantiates smaller stages/DFTs based on the Cooley-Tukey algorithm decimation-in-time
 */
class DirectStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  val io = IO(DirectStageIO(params))

  val numPointsDiv2 = params.numPoints / 2
  // generate twiddle constants
  val twiddles_seq = Seq.fill(numPointsDiv2)(Wire(params.protoTwiddle.cloneType))
  (0 until numPointsDiv2).map(n => {
    twiddles_seq(n).real := Real[T].fromDouble( cos(2 * Pi / params.numPoints * n))
    twiddles_seq(n).imag := Real[T].fromDouble(-sin(2 * Pi / params.numPoints * n))
  })

  val butterfly_inputs = Wire(Vec(params.numPoints, params.protoIQ.cloneType))
  if (params.numPoints == 2) {
    butterfly_inputs     := io.in.bits.iq
    io.out.bits.pktStart := io.in.bits.pktStart
    io.out.bits.pktEnd   := io.in.bits.pktEnd
    io.out.valid         := io.in.valid
  } else {
    // Create sub-FFTs
    val new_params = FFTParams(params, numPointsDiv2)
    val sub_stg_outputs = (0 until 2).map {
      case i => {
        val stage = Module(new DirectStage(new_params))
        stage.io.in.bits.iq       := io.in.bits.iq.zipWithIndex.filter(_._2 % 2 == i).map(_._1)
        stage.io.in.bits.pktStart := io.in.bits.pktStart
        stage.io.in.bits.pktEnd   := io.in.bits.pktEnd
        stage.io.in.valid         := io.in.valid

        if (i == 0) {
          if (params.pipeline) {
            // Register the outputs of sub-fft stages
            io.out.bits.pktStart := RegNext(stage.io.out.bits.pktStart)
            io.out.bits.pktEnd   := RegNext(stage.io.out.bits.pktEnd)
            io.out.valid         := RegNext(stage.io.out.valid, init=false.B)
          } else {
            io.out.bits.pktStart := stage.io.out.bits.pktStart
            io.out.bits.pktEnd   := stage.io.out.bits.pktEnd
            io.out.valid         := stage.io.out.valid
          }
        }

        if (params.pipeline) {
          // Register the outputs of sub-fft stages
          RegNext(stage.io.out.bits.iq)
        } else {
          stage.io.out.bits.iq
        }
      }
    }
    butterfly_inputs := sub_stg_outputs(0) ++ sub_stg_outputs(1)
  }

  (0 until numPointsDiv2).map(n => {
    val butterfly_outputs = Butterfly[T](Seq(butterfly_inputs(n), butterfly_inputs(n + numPointsDiv2) * twiddles_seq(n)))
    io.out.bits.iq(n)                 := butterfly_outputs(0)
    io.out.bits.iq(n + numPointsDiv2) := butterfly_outputs(1)
  })
}

/**************************************************
 * Single-Path Delay Feedback FFT (top level and sub-blocks)
 **************************************************/

/**
 * Top level SDF FFT block
 *
 * For serial-input serial-output, arbitrarily choose DIT for "optimal" decimation type setting
 */
class SDFFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  val io = IO(SISOIO(params))
  val serdes_params = SerDesParams(params.protoIQ.cloneType, params.numPoints)

  if (params.decimType == "dif") {
    val ser               = Module(new Serializer(serdes_params))
    val sdf_fft_deser_out = Module(new SDFFFTDeserOut(params))
    io.in     <> sdf_fft_deser_out.io.in
    ser.io.in <> sdf_fft_deser_out.io.out
    io.out    <> ser.io.out
  } else {
    val des              = Module(new Deserializer(serdes_params))
    val sdf_fft_deser_in = Module(new SDFFFTDeserIn(params))
    io.in      <> des.io.in
    des.io.out <> sdf_fft_deser_in.io.in
    io.out     <> sdf_fft_deser_in.io.out
  }
}

/**
 * Top level SDF FFT block for parallel/deserialized input
 *
 * For parallel-input serial-output, "optimal" setting is DIT (unscrambler is at the input)
 */
class SDFFFTDeserIn[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val dit: Boolean = true) extends Module {
  params.checkNumPointsPow2()
  val io = IO(DISOIO(params))

  val updated_params = if (params.decimType == "opt") FFTParams(params, "dit") else params
  val serdes_params  = SerDesParams(params.protoIQ.cloneType, params.numPoints)
  val inp_ser     = Module(new Serializer(serdes_params))
  val unscrambler = Module(new FFTUnscrambler(updated_params))
  val sdf_chain   = Module(new SDFChain(updated_params))

  val out_if = if (updated_params.decimType == "dit") {
    // Data flow: Unscrambler -> Serializer -> SDF Chain
    unscrambler.io.in <> io.in
    inp_ser.io.in     <> unscrambler.io.out
    sdf_chain.io.out
  } else {
    // Data flow: Serializer -> SDF Chain -> Deserializer -> Unscrambler -> Serializer
    val ser = Module(new Serializer(serdes_params))
    val des = Module(new Deserializer(serdes_params))
    inp_ser.io.in <> io.in
    des.io.in     <> sdf_chain.io.out
    des.io.out    <> unscrambler.io.in
    ser.io.in     <> unscrambler.io.out
    ser.io.out
  }
  inp_ser.io.out <> sdf_chain.io.in
  io.out         <> out_if
}

/**
 * Top level SDF FFT block for parallel/deserialized output
 *
 * For serial-input parallel-output, "optimal" setting is DIF (unscrambler is at the output)
 */
class SDFFFTDeserOut[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  val io = IO(SIDOIO(params))

  val updated_params = if (params.decimType == "opt") FFTParams(params, "dif") else params
  val serdes_params  = SerDesParams(params.protoIQ.cloneType, params.numPoints)
  val out_des     = Module(new Deserializer(serdes_params))
  val unscrambler = Module(new FFTUnscrambler(updated_params))
  val sdf_chain   = Module(new SDFChain(updated_params))

  val inp_if = if (updated_params.decimType == "dif") {
    // Data flow: SDF Chain -> Deserializer -> Unscrambler
    unscrambler.io.in  <> out_des.io.out
    unscrambler.io.out <> io.out
    sdf_chain.io.in
  } else {
    // Data flow: Deserializer -> Unscrambler -> Serializer -> SDF Chain -> Deserializer
    val ser = Module(new Serializer(serdes_params))
    val des = Module(new Deserializer(serdes_params))
    out_des.io.out <> io.out
    ser.io.out     <> sdf_chain.io.in
    ser.io.in      <> unscrambler.io.out
    des.io.out     <> unscrambler.io.in
    des.io.in
  }
  out_des.io.in <> sdf_chain.io.out
  inp_if        <> io.in
}

/**
 * SDF FFT Unscrambler
 *
 * Reorders parallel data by bit reversion
 */
class FFTUnscrambler[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(DIDOIO(params))

  // Bulk connect, but iq field will be re-connected in the following block of code
  io.out <> io.in

  (0 until params.numPoints).foreach(i => {
    val index = i.U(log2Up(params.numPoints).W)
    val reversed_index = Reverse(index)
    io.out.bits.iq(reversed_index) := io.in.bits.iq(index)
  })
}

/**
 * SDF FFT Chain
 *
 * Instantiates and connects SDF FFT stages in series and provides necessary control signals for each stage
 */
class SDFChain[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  // At this point, "opt" decimType should already have been resolved to "dit" or "dif"
  require(Seq("dit", "dif").contains(params.decimType), s"""Decimation type must either be dit or dif""")
  val io = IO(SISOIO(params))

  // Calculation of constants
  val numPointsDiv2     = params.numPoints / 2                                                                // FFT size / 2
  val numStages         = log2Up(params.numPoints)                                                            // required number of SDF stages for given FFT size
  val delayLog2s        = if (params.decimType == "dit") (0 until numStages) else (0 until numStages).reverse // log2(delay) of each nth stage
  val delays            = delayLog2s.map(d => scala.math.pow(2, d).toInt)                                     // actual delay of each nth stage
  val cumulative_delays = delays.scanLeft(0)(_ + _)                                                           // Cumulative delay up to (and including) nth stage

  // Generate ROM of twiddle factors
  val twiddles_rom = Wire(Vec(numPointsDiv2, params.protoTwiddle.cloneType))
  (0 until numPointsDiv2).map(n => {
    twiddles_rom(n).real := Real[T].fromDouble( cos(2 * Pi / params.numPoints * n))
    twiddles_rom(n).imag := Real[T].fromDouble(-sin(2 * Pi / params.numPoints * n))
  })

  // FSM states for control logic
  val sIdle :: sComp :: sDone :: Nil = Enum(3)
  val state      = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  // Counter for control logic
  val cntr      = RegInit(0.U(log2Up(params.numPoints).W))
  val cntr_next = Wire(cntr.cloneType)

  // Instantiate and connect control signals of stages
  val sdf_stages = delayLog2s.zip(delays).zip(cumulative_delays).map {
    case ((delayLog2, delay), cumulative_delay) => {
      val stage = Module(new SDFStage(params, delay=delay, rom_shift=numStages - 1 - delayLog2))
      stage.io.twiddles_rom := twiddles_rom
      stage.io.cntr         := (cntr - cumulative_delay.U)(delayLog2, 0)
      stage.io.en           := io.in.fire()
      stage
    }
  }

  // Connect datapath of stages in series
  sdf_stages.map(_.io).foldLeft(RegNext(io.in.bits))((stg_in, stg_io) => {
    stg_io.in := stg_in
    stg_io.out
  })

  // Output interface connections
  // TODO: Do we need a Queue?
  io.out.bits  := sdf_stages.last.io.out
  io.out.valid := ShiftRegister(io.in.fire(), cumulative_delays.last + 1, resetData=false.B, en=true.B)
  io.in.ready  := io.out.ready

  // Controller FSM
  cntr_next  := cntr
  state_next := state

  switch (state) {
    is (sIdle) {
      when (io.in.fire()) { state_next := sComp }
    }
    is (sComp) {
      when (io.in.fire()) {
        cntr_next := cntr + 1.U
        when (cntr === (params.numPoints - 2).U) { state_next := sDone }
      }
    }
    is (sDone) {
      when      (io.in.fire())  { state_next := sComp }
      .elsewhen (io.out.fire()) { state_next := sIdle }
    }
  }

  when (state_next === sComp && state =/= sComp) {
    // Reset counter
    cntr_next := 0.U
  }

  cntr  := cntr_next
  state := state_next
}

/**
 * Bundle type as IO for direct FFT stage
 */
class SDFStageIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  // datapath
  val in  = Input(SerialPacketBundle(params))
  val out = Output(SerialPacketBundle(params))
  // control
  val twiddles_rom = Input(Vec(params.numPoints / 2, params.protoTwiddle.cloneType))
  val cntr         = Input(UInt(log2Up(params.numPoints).W))
  val en           = Input(Bool())

  override def cloneType: this.type = SDFStageIO(params).asInstanceOf[this.type]
}
object SDFStageIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): SDFStageIO[T] = new SDFStageIO(params)
}

/**
 * Stage for SDF FFT
 *
 * Recursively instantiates smaller stages/DFTs based on the Cooley-Tukey algorithm decimation-in-time
 */
class SDFStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val delay: Int, val rom_shift: Int = 0) extends Module {
  params.checkNumPointsPow2()
  require(isPow2(delay) && delay >= 1, "delay must be a power of 2 greater than or equal to 1")
  require(Seq("dit", "dif").contains(params.decimType), s"""Decimation type must either be dit or dif""")

  val io = IO(SDFStageIO(params))

  val inp = Wire(params.protoIQ.cloneType)
  val out = Wire(params.protoIQ.cloneType)

  // Apply twiddle factor at the input or output, depending on whether it's DIT or DIF
  if (params.decimType == "dit") {
    // Issue: using `inp := Mux(use_twiddle, io.in.iq * twiddle, io.in.iq` causes the following error:
    // can't create Mux with non-equivalent types dsptools.numbers.DspComplex@________ and dsptools.numbers.DspComplex@________
    when (io.cntr > delay.U) {
      inp := io.in.iq * io.twiddles_rom((io.cntr - delay.U) << rom_shift.U)
    } .otherwise {
      inp := io.in.iq
    }
    io.out.iq := out
  } else {
    inp := io.in.iq
    when (io.cntr < delay.U && io.cntr =/= 0.U) {
      io.out.iq := out * io.twiddles_rom(io.cntr << rom_shift.U)
    } .otherwise {
      io.out.iq := out
    }
  }

  val butterfly_outputs = Seq.fill(2)(Wire(params.protoIQ.cloneType))

  val load_input = io.cntr < delay.U
  val shift_in   = Mux(load_input, inp, butterfly_outputs(1))
  val shift_out  = ShiftRegister(shift_in, delay, en=io.en)

  Butterfly[T](Seq(shift_out, inp)).zip(butterfly_outputs).foreach { case (out_val, out_wire) => out_wire := out_val }

  out := Mux(load_input, shift_out, butterfly_outputs(0))

  io.out.pktStart := ShiftRegister(io.in.pktStart, delay, en=io.en)
  io.out.pktEnd   := ShiftRegister(io.in.pktEnd  , delay, en=io.en)
}

// Radix-n butterfly
object Butterfly {
  def apply[T <: Data : Real](in: Seq[DspComplex[T]]): Seq[DspComplex[T]] = {
    require(in.length == 2, "2-point DFT only for no defined twiddle type")
    Seq(in(0) + in(1), in(0) - in(1))
  }
  def apply[T <: Data : Real](in: Seq[DspComplex[T]], genTwiddle: DspComplex[T]): Seq[DspComplex[T]] = {
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

/**
 * "Stage" for Direct IFFT
 *
 * Used in Rader FFT. Essentially the same as IFFT except different IO's and scaling is optional
 */
class DirectInverseStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val scale : Boolean = true) extends Module {
  params.checkNumPointsPow2()
  val io = IO(DirectStageIO(params))

  val fft = Module(new DirectStage(params))
  // Connect valid, pktStart, and pktEnd signals, but iq will be overridden in the following block of code
  io.in  <> fft.io.in
  io.out <> fft.io.out

  io.in.bits.iq.zip(io.out.bits.iq).zipWithIndex.foreach {
    case ((inp, out), index) => {
      fft.io.in.bits.iq(index).real := inp.imag
      fft.io.in.bits.iq(index).imag := inp.real

      if (scale) {
        val scalar = ConvertableTo[T].fromDouble(1.0 / params.numPoints.toDouble)
        out.real := fft.io.out.bits.iq(index).imag * scalar
        out.imag := fft.io.out.bits.iq(index).real * scalar
      } else {
        out.real := fft.io.out.bits.iq(index).imag
        out.imag := fft.io.out.bits.iq(index).real
      }
    }
  }
}

/**
 * Rader FFT
 */
class RaderFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  require(FFTUtil.is_prime(params.numPoints), "number of points must be prime")
  val io = IO(DirectStageIO(params))

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

  val sub_fft = Module(new DirectStage(sub_fft_params))

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

  val sub_ifft = Module(new DirectInverseStage(sub_fft_params, false))
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
