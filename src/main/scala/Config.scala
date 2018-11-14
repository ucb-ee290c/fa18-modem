package modem

import chisel3._
import dsptools.numbers._

trait TXParams[T<:Data] {
    val width: Int
}

trait RXParams[T<:Data] {
    val width: Int
}