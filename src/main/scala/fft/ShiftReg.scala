package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{Decoupled, log2Ceil, log2Floor}

import dsptools.numbers._

import scala.math._

/**
 * Base class for ShiftReg parameters
 *
 * These are type generic
 */
trait ShiftRegParams[T <: Data] {
  val protoData: T
  val length: Int
}

/**
 * Bundle type as IO for ShiftReg modules
 */
class ShiftRegIO[T <: Data : Ring](params: ShiftRegParams[T]) extends Bundle {
  val in  = Input(params.protoData.cloneType)
  val out = Output(Vec(params.length, params.protoData.cloneType))

  val ovrd = Input(Vec(params.length, params.protoData.cloneType))
  val ovrd_en = Input(Bool())

  override def cloneType: this.type = ShiftRegIO(params).asInstanceOf[this.type]
}

object ShiftRegIO {
  def apply[T <: Data : Ring](params: ShiftRegParams[T]): ShiftRegIO[T] =
    new ShiftRegIO(params)
}

class ShiftReg[T <: Data : Real](val params: ShiftRegParams[T]) extends Module {
  val io = IO(ShiftRegIO(params))

  val shifted = Reg(io.out.cloneType)

  shifted.zip(io.ovrd).foldRight(io.in) {
    case ((reg, ovrd), inp) => {
      reg := Mux(io.ovrd_en, ovrd, inp)
      reg
    }
  }
  io.out := shifted
}
