package modem

import chisel3._
import dsptools.numbers._

trait ViterbiParams[U<: Data] {
  val udummy: U
}

class ViterbiDecoder[U <: Data](params: ViterbiParams[U]) extends Module {
  val foo = params
}