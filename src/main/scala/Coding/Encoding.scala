package modem

import chisel3._
import chisel3.util._
import dsptools.numbers.Real
// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: Convolutional encoder + puncturing block

class Encoding[T <: Data, U <: Data](params: CodingParams[T, U]) extends Module {
  val io = IO(new Bundle {
    val in          = Flipped(Decoupled(UInt(1.W)))
    val out         = Decoupled(BitsBundle(params))
    val mac         = MACctrl(params)

    val pktStartIn  = Input(Bool())       //TODO: I need to come up with smarter way to process pktStart and pktEnd
    val pktEndIn    = Input(Bool())
    val modCtrl     = Output(UInt(2.W))
  })
  val pktLatch          = RegInit(false.B)

  when((io.pktStartIn === true.B) && (pktLatch === false.B)){
    pktLatch := true.B
  }.elsewhen( (io.pktEndIn === true.B) && (pktLatch === true.B)){             // lower pktLatch
    pktLatch := false.B
  }

  // TODO: come up with a smart way to process "pktEnd" signal
  val convCodingModule  = Module(new ConvCoding[T, U](params))
  val puncturingModule  = Module(new Puncturing[T, U](params))

  convCodingModule.io.in.bits     := io.in.bits
  convCodingModule.io.in.valid    := pktLatch || io.pktStartIn
  convCodingModule.io.inReady     := io.out.ready
  convCodingModule.io.isHeadIn    := io.mac.isHead
  convCodingModule.io.pktStrIn    := io.pktStartIn
  convCodingModule.io.pktEndIn    := io.pktEndIn
  io.in.ready                     := convCodingModule.io.in.ready

  puncturingModule.io.in          := convCodingModule.io.out
  puncturingModule.io.inReady     := convCodingModule.io.outReady
  puncturingModule.io.isHead      := convCodingModule.io.isHeadOut
  puncturingModule.io.puncMatrix  := io.mac.puncMatrix
  puncturingModule.io.pktStrIn    := convCodingModule.io.pktStrOut
  puncturingModule.io.pktEndIn    := convCodingModule.io.pktEndOut
  puncturingModule.io.pktLatIn    := convCodingModule.io.pktLatOut

  // connect registers to output
  io.out.bits.pktStart          := puncturingModule.io.pktStrOut
  io.out.bits.pktEnd            := puncturingModule.io.pktEndOut
  io.out.bits.bits              := puncturingModule.io.out.bits
  puncturingModule.io.out.ready := io.out.ready
  io.out.valid                  := puncturingModule.io.out.valid
  io.modCtrl                    := puncturingModule.io.modCtrl
}
