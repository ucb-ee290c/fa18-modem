package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{Decoupled, Valid, isPow2, log2Ceil}

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
trait FFTParams[T <: Data] extends IQBundleParams[T] {
  val numPoints: Int
  val protoTwiddle: DspComplex[T]
  val pipeline: Boolean
}
object FFTParams {
  def apply[T <: Data](old_params: FFTParams[T], new_num_points: Int): FFTParams[T] = new FFTParams[T] {
    val protoIQ = old_params.protoIQ
    val protoTwiddle = old_params.protoTwiddle
    val numPoints = new_num_points
    val pipeline = old_params.pipeline
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
  pipeline: Boolean = false
) extends FFTParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, (dataWidth-2-log2Ceil(maxVal)).BP))
  val protoTwiddle = DspComplex(FixedPoint(twiddleWidth.W, (twiddleWidth-2).BP))
}

/**
 * Bundle type as IO for FFT modules
 */
class FFTIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ.cloneType)))
  val out = Decoupled(PacketBundle(params.numPoints, params.protoIQ.cloneType))

  override def cloneType: this.type = FFTIO(params).asInstanceOf[this.type]
}
object FFTIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): FFTIO[T] =
    new FFTIO(params)
}

class FFTDeserIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params.numPoints, params.protoIQ.cloneType)))
  val out = Decoupled(PacketBundle(params.numPoints, params.protoIQ.cloneType))

  override def cloneType: this.type = FFTDeserIO(params).asInstanceOf[this.type]
}
object FFTDeserIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): FFTDeserIO[T] =
    new FFTDeserIO(params)
}

class FFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(FFTIO(params))
  val deser = Module(new Deserializer(DeserializerParams(params.protoIQ.cloneType, params.numPoints)))
  val fft = Module(new FFTDeser(params))

  deser.io.in <> io.in
  fft.io.in <> deser.io.out
  io.out <> fft.io.out
}

class FFTDeser[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(FFTDeserIO(params))
  io.in.ready := true.B
  val fft_stage = {
    if (params.numPoints != 2 && FFTUtil.is_prime(params.numPoints)) {
      Module(new RaderFFT(params))
    }
    else {
      Module(new FFTStage(params))
    }
  }
  fft_stage.io.in.bits  := io.in.bits
  fft_stage.io.in.valid := io.in.fire()

  io.out.bits  := fft_stage.io.out.bits
  io.out.valid := fft_stage.io.out.valid
}

class IFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(FFTDeserIO(params))
  io.in.ready := true.B
  val ifft_stage = Module(new IFFTStage(params))

  ifft_stage.io.in.bits  := io.in.bits
  ifft_stage.io.in.valid := io.in.fire()

  io.out.bits  := ifft_stage.io.out.bits
  io.out.valid := ifft_stage.io.out.valid
}


class FFTStageIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Valid(PacketBundle(params.numPoints, params.protoIQ.cloneType)))
  val out = Valid(PacketBundle(params.numPoints, params.protoIQ.cloneType))

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
  val twiddles_vec = Wire(Vec(numPointsDiv2, params.protoTwiddle.cloneType))
  (0 until numPointsDiv2).map(n => {
    twiddles_vec(n).real := Real[T].fromDouble( cos(2 * Pi / params.numPoints * n))
    twiddles_vec(n).imag := Real[T].fromDouble(-sin(2 * Pi / params.numPoints * n))
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
    val butterfly_outputs = Butterfly[T](Seq(butterfly_inputs(n), butterfly_inputs(n + numPointsDiv2)), twiddles_vec(n))
    io.out.bits.iq(n)                 := butterfly_outputs(0)
    io.out.bits.iq(n + numPointsDiv2) := butterfly_outputs(1)
  })
}

class IFFTStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val scale : Boolean = true) extends Module {
  require(isPow2(params.numPoints), "number of points must be a power of 2")
  val io = IO(FFTStageIO(params))

  val fft = Module(new FFTStage(params))

  val scalar = ConvertableTo[T].fromDouble(1.0 / params.numPoints.toDouble)

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

  fft.io.in.bits.pktStart := io.in.bits.pktStart
  fft.io.in.bits.pktEnd   := io.in.bits.pktEnd
  fft.io.in.valid         := io.in.valid

  io.out.bits.pktStart := fft.io.out.bits.pktStart
  io.out.bits.pktEnd   := fft.io.out.bits.pktEnd
  io.out.valid         := fft.io.out.valid
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
  def apply[T <: Data : Real](in: Seq[DspComplex[T]], twiddle: DspComplex[T]): Seq[DspComplex[T]] =
  {
    require(in.length == 2, "Butterfly requires two data inputs")
    val product = in(1) * twiddle
    Seq(in(0) + product, in(0) - product)
  }
}
