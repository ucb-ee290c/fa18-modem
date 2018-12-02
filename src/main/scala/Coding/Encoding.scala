package modem

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: Convolutional encoder + puncturing block
class EncodingIO[T <: Data](params: CodingParams[T]) extends Bundle {
  val in          = Flipped(Decoupled(UInt(1.W)))
  val out         = Decoupled(BitsBundle(params))
  val mac         = MACctrl(params)
  val pktStartIn  = Input(Bool())       //TODO: I need to come up with smarter way to process pktStart and pktEnd
  val pktEndIn    = Input(Bool())
  override def cloneType: this.type = EncodingIO(params).asInstanceOf[this.type]
}
object EncodingIO {
  def apply[T <: Data](params: CodingParams[T]): EncodingIO[T] = new EncodingIO(params)
}

class Encoding[T <: Data](params: CodingParams[T]) extends Module {
  require(params.m >= 1)
  require(params.k >= 1)
  require(params.n >= 2)

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val state       = RegInit(sStartRecv)

  val io = IO(EncodingIO(params))
  val convCodingModule  = Module(new ConvCoding[T](params))
  val puncturingModule  = Module(new Puncturing[T](params))

  io.in                           <> convCodingModule.io.in
  convCodingModule.io.inReady     := io.out.ready

  puncturingModule.io.in          := convCodingModule.io.out
  puncturingModule.io.isHead      := io.mac.isHead
  puncturingModule.io.inReady     := convCodingModule.io.outReady
  puncturingModule.io.puncMatrix  := io.mac.puncMatrix

  when(io.out.fire()) {
    state       := sStartRecv
  }

  // connect registers to output
  io.out.bits.pktStart          := true.B
  io.out.bits.pktEnd            := false.B
  io.out.bits.bits              := puncturingModule.io.out.bits
  puncturingModule.io.out.ready := io.out.ready
  io.out.valid                  := puncturingModule.io.out.valid
  io.in.ready   := state === sStartRecv   // io.out.ready is fired from FIFO sitting b/w interleaver and

}
