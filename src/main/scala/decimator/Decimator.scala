// package cordic

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

/**
 * Base class for CORDIC parameters
 *
 * These are type generic
 */
trait DecimatorParams[T <: Data] {
  val protoIn: DspComplex[T]
  val protoout: DspComplex[T]
  val nDecimation: Int
}

class DecimateByN(val params: DecimatorParams[T]) extends Module {
  requireIsChiselType(params.protoIn)
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(params.protoIn))
    val out = Decoupled(params.protoOut)
  })
  val count = Counter(params.nDecimation)
  io.in.ready := true.B

  when(io.in.valid){
    when(count.inc()){
      io.out.bits := io.in.bits
      io.out.valid := io.in.valid
    }
  }
}
