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
    val in_hard   = Input(Vec(params.n * params.H, SInt(2.W)))
    val in_soft   = Flipped(Decoupled(BitsBundle(params)))

    val out       = Decoupled(Vec(params.D, UInt(params.k.W)))

    val out_isHead = Output(Bool())                   // for testing purpose
    val out_lenCnt = Output(Bool())                   // for testing purpose
    val out_pktLatch = Output(Bool())                 // for testing purpose
    val out_header_rate = Output(Vec(4, UInt(1.W)))   // for testing purpose
    val out_header_len = Output(UInt(12.W))           // for testing purpose
    val out_hdrEnd    = Output(Bool())                // for testing purpose
    val out_bufData   = Output(Vec(params.n, SInt(2.W)))   // for testing purpose
    val out_en1       = Output(Bool())                // for testing purpose
    val out_en2       = Output(Bool())                // for testing purpose
//    val out_dp    = Output(Vec(params.n, SInt(2.W)))
    val out_pm    = Output(Vec(params.nStates, UInt(params.pmBits.W)))  // storing Path Metric
    val out_sp    = Output(Vec(params.nStates, UInt(params.m.W)))       // storing Survival Path
  })

  val arbiterModule               = Module(new Arbiter[T, U](params))
  val HeaderExtModule             = Module(new HeaderExtractor[T, U](params))
  val DePuncturingModule          = Module(new DePuncturing[T, U](params))
  val pathMetricModule            = Module(new PathMetric[T, U](params))
  val tracebackModule             = Module(new Traceback[T, U](params))

  // Arbiter
  arbiterModule.io.inHead         := DePuncturingModule.io.outHead
  arbiterModule.io.lenCnt         := DePuncturingModule.io.lenCnt
  arbiterModule.io.hdrPktLatch    := DePuncturingModule.io.hdrPktLatch
  io.out_pktLatch := DePuncturingModule.io.hdrPktLatch
  io.out_lenCnt   := DePuncturingModule.io.lenCnt

  // Header-Extractor connection
  HeaderExtModule.io.in           := DePuncturingModule.io.outHead
  HeaderExtModule.io.isHead       := arbiterModule.io.isHead
  io.out_isHead   := arbiterModule.io.isHead

  // De-Puncturing connection
  io.in_soft                      <> DePuncturingModule.io.in           // for soft-input decoding
//  io.in_hard                      <> DePuncturingModule.io.in_hard
  DePuncturingModule.io.isHead    := arbiterModule.io.isHead
  DePuncturingModule.io.hdrEnd    := arbiterModule.io.hdrEnd
  DePuncturingModule.io.headInfo  <> HeaderExtModule.io.headInfo
  io.out_header_len := HeaderExtModule.io.headInfo.bits.dataLen
  io.out_header_rate := HeaderExtModule.io.headInfo.bits.rate

//  io.out_dp                       <> DePuncturingModule.io.outData

  pathMetricModule.io.in          <> DePuncturingModule.io.outData
  pathMetricModule.io.hdrEnd      := arbiterModule.io.hdrEnd
  pathMetricModule.io.inEnable    := DePuncturingModule.io.outEnable
  io.out_bufData  := DePuncturingModule.io.outData
  io.out_en1 := DePuncturingModule.io.outEnable
  io.out_pm <> pathMetricModule.io.outPM
  io.out_sp <> pathMetricModule.io.outSP
  io.out_hdrEnd := arbiterModule.io.hdrEnd

  tracebackModule.io.inPM         := pathMetricModule.io.outPM
  tracebackModule.io.inSP         := pathMetricModule.io.outSP
  tracebackModule.io.enable       := pathMetricModule.io.outEnable
  io.out                          <> tracebackModule.io.out
  io.out_en2  := pathMetricModule.io.outEnable
}
