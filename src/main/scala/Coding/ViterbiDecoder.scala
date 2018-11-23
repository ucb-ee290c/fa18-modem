package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

class ViterbiDecoder[T <: Data: Real](params: CodingParams[T]) extends Module {
  require(params.m > 1)
  require(params.k > 0)
  require(params.n > 0)

  val io = IO(new Bundle {
    //val in        = Input(Vec(params.O, SInt(2.W)))
    val in        = Flipped(Decoupled(BitsBundle(bitsWidth = params.O, protoBits = SInt(2.W))))
    //val inReady   = Input(UInt(1.W))
    val out     = Decoupled(Vec(params.D, UInt(params.k.W)))
    val out_dp  = Output(Vec(params.n, SInt(2.W)))
    val out_pm  = Output(Vec(params.nStates, UInt(params.pmBits.W)))  // storing Path Metric
    val out_sp  = Output(Vec(params.nStates, UInt(params.m.W)))       // storing Survival Path
  })

  val DePuncturingModule  = Module(new DePuncturing[T](params))
  val pathMetricModule    = Module(new PathMetric[T](params))
  val tracebackModule     = Module(new Traceback[T](params))

  DePuncturingModule.io.in_hard :=  io.in.bits.bits
  DePuncturingModule.io.inReady := io.in.ready
  DePuncturingModule.io.stateIn := 0.U
  io.out_dp := DePuncturingModule.io.out_hard

  pathMetricModule.io.in      <> DePuncturingModule.io.out_hard
  pathMetricModule.io.inReady := io.in.ready
  io.out_pm <> pathMetricModule.io.outPM
  io.out_sp <> pathMetricModule.io.outSP

  tracebackModule.io.inPM     := pathMetricModule.io.outPM
  tracebackModule.io.inSP     := pathMetricModule.io.outSP
  tracebackModule.io.inReady  := io.in.ready
  io.out  <> tracebackModule.io.out
}
