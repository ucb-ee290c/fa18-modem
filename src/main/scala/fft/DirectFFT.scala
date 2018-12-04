package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
import breeze.numerics.{cos, sin}
import scala.math.{Pi, pow}

/**************************************************
 * Direct FFT (top level and sub-blocks)
 **************************************************/

/**
 * Top level Direct FFT block
 */
class DirectFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(DIDOIO(params))
  val fft_stage = if (FFTUtil.is_power(params.numPoints)) {
    Module(new CooleyTukeyStage(params))
  } else {
    Module(new PFAStage(params))
  }
  fft_stage.io.in := io.in.bits.iq
  io.in.ready     := io.out.ready
  io.out.bits.iq  := fft_stage.io.out

  val delay = FFTUtil.factorize(params.numPoints)._2.reduce(_ + _) - 1
  if (params.pipeline && delay > 0) {
    io.out.bits.pktStart := ShiftRegister(io.in.bits.pktStart, delay)
    io.out.bits.pktEnd   := ShiftRegister(io.in.bits.pktEnd  , delay)
    io.out.valid         := ShiftRegister(io.in.valid        , delay, false.B, true.B)
  } else {
    io.out.bits.pktStart := io.in.bits.pktStart
    io.out.bits.pktEnd   := io.in.bits.pktEnd
    io.out.valid         := io.in.valid
  }
}

/**
 * Bundle type as IO for direct FFT stage
 */
class DirectStageIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in  = Input(PacketBundle(params).iq)
  val out = Output(PacketBundle(params).iq)

  override def cloneType: this.type = DirectStageIO(params).asInstanceOf[this.type]
}
object DirectStageIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): DirectStageIO[T] = new DirectStageIO(params)
}

/**
 * "Stage" for Direct FFT Cooley-Tukey algorithm (CTA) based implementation
 *
 * Recursively instantiates smaller stages/DFTs based on the CTA decimation-in-time.
 * This implementation is only for FFT sizes that are a power of a number.
 * Relatively prime numbers are handled via the PFA stage.
 */
class CooleyTukeyStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow()
  val io = IO(DirectStageIO(params))

  val (base, numStages) = params.getPowerInfo()
  val numPointsDivBase = params.numPoints / base

  // generate twiddle constants
  val numTwiddles = (base - 1) * (numPointsDivBase - 1) + 1
  val twiddlesSeq = (0 until numTwiddles).map(n => {
    val twiddle_wire = Wire(params.protoTwiddle.cloneType)
    twiddle_wire.real := Real[T].fromDouble( cos(2 * Pi / params.numPoints * n))
    twiddle_wire.imag := Real[T].fromDouble(-sin(2 * Pi / params.numPoints * n))
    twiddle_wire
  })

  if (params.numPoints == base) { // Base case --> use butterfly
    io.out.zip(Butterfly[T](io.in.seq, params.protoTwiddle)).foreach {
      case (out_wire, out_val) => out_wire := out_val 
    }
  } else {
    // Create sub-FFTs
    val new_params = FFTParams(params, numPointsDivBase)
    val sub_stg_outputs = (0 until base).map {
      case i => {
        val stage = Module(new CooleyTukeyStage(new_params))

        // Interleave input
        stage.io.in := io.in.zipWithIndex.filter(_._2 % base == i).map(_._1)

        // Optionally insert pipeline
        if (params.pipeline) RegNext(stage.io.out) else stage.io.out
      }
    }

    (0 until numPointsDivBase).map(n => {
      val butterfly_inputs = Seq(sub_stg_outputs.head(n)) ++ sub_stg_outputs.zipWithIndex.tail.map {
        case (stg_output, idx) => stg_output(n) * twiddlesSeq(idx * n) // Multiply by twiddle factors
      }
      // Insert butterflies
      val butterfly_outputs = Butterfly[T](butterfly_inputs, params.protoTwiddle)
      // Interleave output
      butterfly_outputs.zipWithIndex.foreach {
        case (out_val, idx) => io.out(n + numPointsDivBase * idx) := out_val
      }
    })
  }
}

/**
 * "Stage" for Direct FFT prime-factor algorithm (PFA) based implementation
 *
 * Recursively instantiates smaller stages/DFTs based on the PFA, dividing into relatively prime stages.
 * The leaf stages are CTA implementations.
 */
class PFAStage[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  require(!FFTUtil.is_power(params.numPoints), "number of points must not be a power of some number")
  val io = IO(DirectStageIO(params))

  val factorized = FFTUtil.factorize(params.numPoints) // tuple consisting of prime factors and powers

  val N1 = pow(factorized._1.head, factorized._2.head).toInt
  val N2 = params.numPoints / N1

  val invN1 = FFTUtil.mult_inv(N1 % N2, N2)
  val invN2 = FFTUtil.mult_inv(N2 % N1, N1)

  val first_stage_params = FFTParams(params, N1)
  val rest_stage_params  = FFTParams(params, N2)

  // First stage is CTA-based sub-FFT
  val first_stage_outputs = (0 until N2).map(n2 => {
    val stage = Module(new CooleyTukeyStage(first_stage_params))
    // Good's mapping
    stage.io.in.zipWithIndex.map {
      case (stage_in, n1) => {
        val inp_idx = (N1 * n2 + N2 * n1) % params.numPoints
        stage_in := io.in(inp_idx)
      }
    }
    stage.io.out
  })

  // Rest of the stages is either a CTA if power of a number, or PFA (which recursively instantiates CTAs)
  val rest_stage_outputs = (0 until N1).map(k1 => {
    val stage = if (FFTUtil.is_power(N2)) {
      Module(new CooleyTukeyStage(rest_stage_params))
    } else {
      Module(new PFAStage(rest_stage_params))
    }
    stage.io.in.zipWithIndex.map {
      case (rest_stage_in, idx) => {
        if (params.pipeline) {
          // Register the outputs of sub-FFT stages
          rest_stage_in := RegNext(first_stage_outputs(idx)(k1))
        } else {
          rest_stage_in := first_stage_outputs(idx)(k1)
        }
      }
    }

    // CRT mapping
    stage.io.out.zipWithIndex.map {
      case (stage_out, k2) => {
        val out_idx = (N1 * invN1 * k2 + N2 * invN2 * k1) % params.numPoints
        io.out(out_idx) := stage_out
      }
    }
    stage.io.out
  })
}