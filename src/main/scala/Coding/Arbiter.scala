package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
import modem.{BranchMetric, CodingParams, Trellis}
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
class Arbiter[T <: Data: Real](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val headInfo  = Flipped(Decoupled(DecodeHeadBundle()))
    val dataInfo  = Flipped(Decoupled(DecodeDataBundle()))
    val isHead    = Output(Bool())
  })
  val isHeadReg   = RegInit(true.B)

  when(io.headInfo.valid === true.B && isHeadReg === true.B){
    isHeadReg := false.B      // data starts
  }.elsewhen(io.dataInfo.valid === true.B && isHeadReg === false.B){
    isHeadReg := true.B       // reached end of data blocks. next block is header information
  }

  io.isHead := isHeadReg
}


