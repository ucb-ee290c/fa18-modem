package modem

import chisel3._
import chisel3.util._

trait ViterbiParams[U<: Data] extends BitsBundleParams[U] {
  val udummy: U
}

class ViterbiDecoder[U <: Data](params: ViterbiParams[U]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(BitsBundle(params)))
    val out = Decoupled(BitsBundle(params))
  })
}