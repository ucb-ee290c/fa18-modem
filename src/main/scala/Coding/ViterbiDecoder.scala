package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: Viterbi decoder + de-puncturing + header extractor
class ViterbiDecoder[T <: Data: Real, U <: Data: Real](params: CodingParams[T, U]) extends Module {
  require(params.m > 1)
  require(params.k > 0)
  require(params.n > 0)

  val io = IO(new Bundle {
    val in            = Flipped(Decoupled(BitsBundle(params)))
    val out           = Decoupled(Vec(params.D, UInt(params.k.W)))

    val out_isHead    = Output(Bool())                  // for testing purpose
    val out_lenCnt    = Output(Bool())                  // for testing purpose
    val out_pktLatch  = Output(Bool())                  // for testing purpose
    val out_header_rate = Output(Vec(4, UInt(1.W)))     // for testing purpose
    val out_header_len  = Output(UInt(12.W))            // for testing purpose
    val outHead       = Output(Vec(params.n * params.H, params.protoBits.cloneType)) // for testing purpose
    val out_hdrEnd    = Output(Bool())                  // for testing purpose
    val out_bufData   = Output(Vec(params.n, params.protoBits.cloneType))   // for testing purpose
    val out_en1       = Output(Bool())                  // for testing purpose
    val out_en2       = Output(Bool())                  // for testing purpose
    val out_pm        = Output(Vec(params.nStates, params.pmBitType.cloneType)) // storing Path Metric
    val out_sp        = Output(Vec(params.nStates, UInt(params.m.W)))           // storing Survival Path
  })

  val arbiterModule               = Module(new Arbiter[T, U](params))
  val HeaderExtModule             = Module(new HeaderExtractor[T, U](params))
  val DePuncturingModule          = Module(new DePuncturing[T, U](params))
  val pathMetricModule            = Module(new PathMetric[T, U](params))
  val tracebackModule             = Module(new Traceback[T, U](params))

  // Arbiter module
  arbiterModule.io.lenCnt         := DePuncturingModule.io.lenCnt
  arbiterModule.io.hdrPktLatch    := DePuncturingModule.io.hdrPktLatch
  io.out_pktLatch := DePuncturingModule.io.hdrPktLatch    // for testing purpose
  io.out_lenCnt   := DePuncturingModule.io.lenCnt         // for testing purpose

  // Header-Extractor connection
  HeaderExtModule.io.in           := DePuncturingModule.io.outHead
  HeaderExtModule.io.isHead       := arbiterModule.io.isHead
  io.outHead                      := DePuncturingModule.io.outHead  // for testing purpose
  io.out_isHead   := arbiterModule.io.isHead                        // for testing purpose

  // De-Puncturing connection
  io.in                           <> DePuncturingModule.io.in
  DePuncturingModule.io.isHead    := arbiterModule.io.isHead
  DePuncturingModule.io.hdrEnd    := arbiterModule.io.hdrEnd
  DePuncturingModule.io.headInfo  <> HeaderExtModule.io.headInfo
  io.out_header_len := HeaderExtModule.io.headInfo.bits.dataLen   // for testing purpose
  io.out_header_rate := HeaderExtModule.io.headInfo.bits.rate     // for testing purpose

  pathMetricModule.io.in          <> DePuncturingModule.io.outData
  pathMetricModule.io.hdrEnd      := arbiterModule.io.hdrEnd
  pathMetricModule.io.inEnable    := DePuncturingModule.io.outEnable
  io.out_bufData  := DePuncturingModule.io.outData    // for testing purpose
  io.out_en1 := DePuncturingModule.io.outEnable       // for testing purpose
  io.out_pm <> pathMetricModule.io.outPM              // for testing purpose
  io.out_sp <> pathMetricModule.io.outSP              // for testing purpose
  io.out_hdrEnd := arbiterModule.io.hdrEnd            // for testing purpose

  tracebackModule.io.inPM         := pathMetricModule.io.outPM
  tracebackModule.io.inSP         := pathMetricModule.io.outSP
  tracebackModule.io.enable       := pathMetricModule.io.outEnable
  tracebackModule.io.headInfo.valid := HeaderExtModule.io.headInfo.valid
  tracebackModule.io.headInfo.bits  := HeaderExtModule.io.headInfo.bits
  io.out                          <> tracebackModule.io.out   // for testing purpose
  io.out_en2  := pathMetricModule.io.outEnable                // for testing purpose
}
