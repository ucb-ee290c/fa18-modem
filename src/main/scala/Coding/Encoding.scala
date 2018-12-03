package modem

import chisel3._
import chisel3.util._
import dsptools.numbers.Real
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: Convolutional encoder + puncturing block
class EncodingIO[T <: Data, U <: Data](params: CodingParams[T, U]) extends Bundle {
  val in          = Flipped(Decoupled(UInt(1.W)))
  val out         = Decoupled(BitsBundle(params))
  val mac         = MACctrl(params)

  val pktStartIn  = Input(Bool())       //TODO: I need to come up with smarter way to process pktStart and pktEnd
  val pktEndIn    = Input(Bool())
  val modCtrl     = Output(UInt(2.W))
  override def cloneType: this.type = EncodingIO(params).asInstanceOf[this.type]
}
object EncodingIO {
  def apply[T <: Data, U <: Data](params: CodingParams[T, U]): EncodingIO[T, U] = new EncodingIO(params)
}

class Encoding[T <: Data, U <: Data](params: CodingParams[T, U]) extends Module {
  require(params.m >= 1)
  require(params.k >= 1)
  require(params.n >= 2)

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val state       = RegInit(sStartRecv)

  val io = IO(EncodingIO(params))
  val convCodingModule  = Module(new ConvCoding[T, U](params))
  val puncturingModule  = Module(new Puncturing[T, U](params))

  io.in                           <> convCodingModule.io.in
  convCodingModule.io.inReady     := io.out.ready
  convCodingModule.io.isHeadIn    := io.mac.isHead
  convCodingModule.io.pktStrIn    := io.pktStartIn
  convCodingModule.io.pktEndIn    := io.pktEndIn

  puncturingModule.io.in          := convCodingModule.io.out
  puncturingModule.io.inReady     := convCodingModule.io.outReady
  puncturingModule.io.isHead      := convCodingModule.io.isHeadOut
  puncturingModule.io.puncMatrix  := io.mac.puncMatrix
  puncturingModule.io.pktStrIn    := convCodingModule.io.pktStrOut
  puncturingModule.io.pktEndIn    := convCodingModule.io.pktEndOut


  when(io.out.fire()) {
    state       := sStartRecv
  }

  // connect registers to output
  io.out.bits.pktStart          := puncturingModule.io.pktStrOut
  io.out.bits.pktEnd            := puncturingModule.io.pktEndOut
  io.out.bits.bits              := puncturingModule.io.out.bits
  puncturingModule.io.out.ready := io.out.ready
  io.out.valid                  := puncturingModule.io.out.valid
  io.in.ready   := state === sStartRecv   // io.out.ready is fired from FIFO sitting b/w interleaver and
  io.modCtrl                    := puncturingModule.io.modCtrl
}
