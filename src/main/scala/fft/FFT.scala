package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._
import breeze.numerics.{cos, sin}
import scala.math.Pi

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
  val sdfRadix     : Int           // if SDF FFT is being used, the radix
  val pipeline     : Boolean       // if direct FFT is being used, whether to pipeline the FFT stages
  lazy val width = numPoints       // overrides the `width` parameter of PacketBundleParams

  // Allowed values for some parameters
  final val allowedFftTypes      = Seq("direct", "sdf")
  final val allowedDecimTypes    = Seq("dit", "dif", "opt")
  final val allowedSDFRadices    = Seq(2, 4)

  // Common require functions used in FFT blocks
  def checkNumPointsPow2() {
    require(isPow2(numPoints), "number of points must be a power of 2")
  }
  def checkNumPointsPow() {
    require(FFTUtil.is_power(numPoints), "number of points must be a power of some number")
  }
  def checkFftType() {
    require(allowedFftTypes.contains(fftType), s"""FFT type must be one of the following: ${allowedFftTypes.mkString(", ")}""")
  }
  def checkDecimType() {
    require(allowedDecimTypes.contains(decimType), s"""Decimation type must be one of the following: ${allowedDecimTypes.mkString(", ")}""")
  }
  def checkSDFRadix() {
    require(allowedSDFRadices.contains(sdfRadix), s"""Radix must be one of the following: ${allowedSDFRadices.mkString(", ")}""")
  }
  def checkNumPointsPowOfSDFRadix() {
    require(FFTUtil.is_power_of(numPoints, sdfRadix), "number of points must be a power of the SDF radix")
  }
  def getPowerInfo(): (Int, Int) = {
    (FFTUtil.factorize(numPoints)._1.head, FFTUtil.factorize(numPoints)._2.head)
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
    val sdfRadix     = old_params.sdfRadix
  }
  // Override decimation type
  def apply[T <: Data](old_params: FFTParams[T], newDecimType: String): FFTParams[T] = new FFTParams[T] {
    val protoIQ      = old_params.protoIQ
    val protoTwiddle = old_params.protoTwiddle
    val numPoints    = old_params.numPoints
    val pipeline     = old_params.pipeline
    val fftType      = old_params.fftType
    val decimType    = newDecimType
    val sdfRadix     = old_params.sdfRadix
  }
}

/**
 * FFT parameters case class for fixed-point FFTs
 */
case class FixedFFTParams(
  dataWidth   : Int, // width of input and output
  binPoint    : Int, // binary point of input and output
  twiddleWidth: Int, // width of twiddle constants
  numPoints   : Int,
  pipeline    : Boolean = true,
  fftType     : String = "sdf",
  decimType   : String = "opt",
  sdfRadix    : Int = 2
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
  val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
  val out = Decoupled(PacketBundle(1, params.protoIQ))

  override def cloneType: this.type = SISOIO(params).asInstanceOf[this.type]
}
object SISOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): SISOIO[T] = new SISOIO(params)
}

// serial input, deserial output
class SIDOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
  val out = Decoupled(PacketBundle(params))

  override def cloneType: this.type = SIDOIO(params).asInstanceOf[this.type]
}
object SIDOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): SIDOIO[T] = new SIDOIO(params)
}

// deserial input, serial output
class DISOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params)))
  val out = Decoupled(PacketBundle(1, params.protoIQ))

  override def cloneType: this.type = DISOIO(params).asInstanceOf[this.type]
}
object DISOIO {
  def apply[T <: Data : Ring](params: PacketBundleParams[T]): DISOIO[T] = new DISOIO(params)
}

// deserial input, deserial output
class DIDOIO[T <: Data : Ring](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params)))
  val out = Decoupled(PacketBundle(params))

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
      // instantiate PacketDeserializer to go from serial-input FFT to parallel-input DirectFFT
      val deser = Module(new PacketDeserializer(PacketSerDesParams(params.protoIQ.cloneType, params.numPoints)))
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
  io.out.bits.iq(0).real := fft_out.bits.iq(0).imag * scalar
  io.out.bits.iq(0).imag := fft_out.bits.iq(0).real * scalar

  params.fftType match {
    case "direct" => {
      // instantiate PacketSerializer to go from parallel-output DirectFFT to serial-output FFT
      val ser = Module(new PacketSerializer(PacketSerDesParams(params.protoIQ.cloneType, params.numPoints)))
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

// Radix-n butterfly
object Butterfly {
  def apply[T <: Data : Real](in: Seq[DspComplex[T]]): Seq[DspComplex[T]] = {
    require(in.length == 2, "2-point DFT only for no defined twiddle type")
    Seq(in(0) + in(1), in(0) - in(1))
  }
  def apply[T <: Data : Real](in: Seq[DspComplex[T]], genTwiddle: DspComplex[T]): Seq[DspComplex[T]] = {
    in.length match {
      case 2 => apply(in)
      case _ => {
        val twiddlesSeq = (0 until in.length).map(n => {
          val twiddle_wire = Wire(genTwiddle.cloneType)
          twiddle_wire.real := Real[T].fromDouble( cos(2 * Pi / in.length * n))
          twiddle_wire.imag := Real[T].fromDouble(-sin(2 * Pi / in.length * n))
          twiddle_wire
        })
        Seq.tabulate(in.length)(k => {
          in.head + in.zipWithIndex.tail.map {
            case (inp, n) => inp * twiddlesSeq((k * n) % in.length)
          }.reduce(_ + _)
        })
      }
    }
  }
}
