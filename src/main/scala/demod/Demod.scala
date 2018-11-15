package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._

trait DemodulationParams[T<: Data, U<: Data] extends PacketBundleParams[T] with BitsBundleParams[U] {
  val tdummy: T
  val udummy: U
}

class Demodulator[T <: Data:Real:BinaryRepresentation, U <: Data](params: DemodulationParams[T, U]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(PacketBundle(params)))
    val out = Decoupled(PacketBundle(params))
  })
}