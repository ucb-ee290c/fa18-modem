package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
import modem.{BranchMetric, CodingParams, Trellis}
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: Arbiter identifies whether the incoming packet contains header information or payload
class Arbiter[T <: Data: Real, U <: Data: Real](params: CodingParams[T, U]) extends Module {
  val io = IO(new Bundle{
//    val inHead      = Input(Vec((params.n * params.H), params.protoBits))  // from De-Puncturing
    val lenCnt      = Input(Bool())                                 // from De-Puncturing
    val hdrPktLatch = Input(Bool())                                 // from De-Puncturing

    val isHead      = Output(Bool())                                // to De-Puncturing & HeadExtractor
    val hdrEnd      = Output(Bool())                                // to De-Puncturing
  })
  val isHeadReg     = RegInit(false.B)
  val hdrCounter    = RegInit(0.U(6.W))
  val hdrEndReg     = RegInit(false.B)

  // when it thinks the next OFDM symbol contains header information
  when(io.hdrPktLatch === true.B && isHeadReg === false.B && io.lenCnt === true.B && hdrCounter === 0.U){
    isHeadReg   := true.B
    hdrCounter  := 1.U

  // currently receiving input is header information. isHead will be ON over 62 (default) clock cycles
  }.elsewhen(io.hdrPktLatch === true.B && isHeadReg === true.B){
    hdrCounter := hdrCounter + 1.U
    when(hdrCounter === (params.FFTPoint-1).U){
      hdrCounter  := 0.U
      isHeadReg   := false.B
    }
  }

  // on 61 clock cycle, raise hdrEndReg so that it resets control registers before it starts decoding the actual payload
  when(hdrCounter === (params.FFTPoint-2).U){        // PDSU is available 62 clk cycles after the first header bit is received
    hdrEndReg   := true.B
  }.otherwise{
    hdrEndReg   := false.B
  }

  io.isHead         := isHeadReg
  io.hdrEnd         := hdrEndReg
}


