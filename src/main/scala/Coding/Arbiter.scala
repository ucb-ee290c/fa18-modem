package modem

import chisel3._
import chisel3.util._
//import cordic.CordicParams
import dsptools.numbers._
import modem.{BranchMetric, CodingParams, Trellis}
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: Arbiter identifies whether the incoming packet contains header information or payload
class Arbiter[T <: Data: Real](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle{
    val inHead      = Input(Vec((params.n * params.H), SInt(2.W)))  // from De-Puncturing
    val lenCnt      = Input(Bool())                                 // from De-Puncturing
    val hdrPktLatch = Input(Bool())                                 // from De-Puncturing

    val isHead      = Output(Bool())                                // to De-Puncturing & HeadExtractor
    val hdrEnd      = Output(Bool())                                // to De-Puncturing
  })
  val isHeadReg     = RegInit(false.B)
  val hdrCounter    = RegInit(0.U(6.W))
  val hdrEndReg     = RegInit(false.B)

  when(io.hdrPktLatch === true.B && isHeadReg === false.B && io.lenCnt === true.B && hdrCounter === 0.U){
    isHeadReg   := true.B
    hdrCounter  := 1.U
  }.elsewhen(io.hdrPktLatch === true.B && isHeadReg === true.B){
    hdrCounter := hdrCounter + 1.U
    when(hdrCounter === (params.H-1).U){
      hdrCounter  := 0.U
      isHeadReg   := false.B
    }
  }
  when(hdrCounter === (params.H-2).U){
    hdrEndReg   := true.B
  }.otherwise{
    hdrEndReg   := false.B
  }

  io.isHead         := isHeadReg
  io.hdrEnd         := hdrEndReg
}


