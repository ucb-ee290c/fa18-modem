package modem

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
trait DecimatorParams[T <: Data] extends PacketBundleParams[T] {
  val nDecimation: Int
}

class DecimateByN[T<:Data:Real](val params: DecimatorParams[T]) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(SerialPacketBundle(params)))
    val out = Decoupled(SerialPacketBundle(params))
  })
  val count = Counter(params.nDecimation)
  io.in.ready := true.B

  io.out.bits.pktStart := io.in.bits.pktStart
  io.out.bits.pktEnd := io.in.bits.pktEnd

  io.out.bits.iq := io.in.bits.iq
  when(io.in.fire()){
    when(count.inc()){

      io.out.valid := io.in.valid
    }.otherwise{
      io.out.valid := false.B
    }
  }
}
