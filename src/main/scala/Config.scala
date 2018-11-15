package modem

import chisel3._

trait TXParams[T<:Data] {
    val width: Int
}

trait RXParams[T<:Data] {
    val width: Int
}