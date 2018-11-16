package Coding

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

class CodingIO[T <: Data](params: CodingParams[T]) extends Bundle {
  val in    = Flipped(Decoupled(params.protoInOut.cloneType))
  val out   = Decoupled(Vec(params.O, params.protoInOut.cloneType))

  override def cloneType: this.type = CodingIO(params).asInstanceOf[this.type]
}
object CodingIO {
  def apply[T <: Data](params: CodingParams[T]): CodingIO[T] = new CodingIO(params)
}

class Encoding[T <: Data](params: CodingParams[T]) extends Module {
  require(params.m > 1)
  require(params.k > 0)
  require(params.n > 0)

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val state = RegInit(sStartRecv)

  val io = IO(CodingIO(params))
  val convCodingModule  = Module(new ConvCoding[T](params))
  val puncturingModule  = Module(new Puncturing[T](params))

  when(io.in.fire() && state === sStartRecv){
    convCodingModule.io.inReady := 1.U
  }.otherwise{
    convCodingModule.io.inReady := 0.U
  }

  convCodingModule.io.in := io.in.bits
  convCodingModule.io.stateIn := state

  puncturingModule.io.in := convCodingModule.io.out
  puncturingModule.io.stateIn := convCodingModule.io.stateOut
  puncturingModule.io.inReady := convCodingModule.io.outReady

  when(io.out.fire() && puncturingModule.io.stateOut === sDone) {
    state       := sStartRecv
  }

  // connect registers to output
  io.out.bits   := puncturingModule.io.out
  io.in.ready   := state === sStartRecv   // io.out.ready is fired from FIFO sitting b/w interleaver and
  io.out.valid  := state === sDone
}
