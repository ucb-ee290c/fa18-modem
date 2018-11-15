//package Coding
//
//import chisel3._
//import chisel3.util._
////import freechips.rocketchip.diplomacy.LazyModule
////import freechips.rocketchip.subsystem.BaseSubsystem
//
//class ViterbiDecoder[T <: Data](params: CodingParams[T]) extends Module {
//  require(params.m > 1)
//  require(params.k > 0)
//  require(params.n > 0)
//
//  val io = IO(new Bundle {
//    val in        = Input(Vec(params.n, UInt(1.W)))
//    val inReady   = Input(UInt(1.W))
//    val outPM     = Output(Vec(params.nStates, UInt(params.pmBits.W)))  // storing Path Metric
//    val outSP     = Output(Vec(params.nStates, UInt(params.m.W)))       // storing Survival Path
//  })
//
//  val pathMetricModule = Module(new PathMetric[T](params))
//
//  (0 until params.n).map(i => {pathMetricModule.io.in(i) := io.in(i)})
//  pathMetricModule.io.inReady := io.inReady
//
//  // ******** Memory Configuration ********
//  val sramModule  = Module(new SRAM[T](params))
//  val startReading := true.B
//  sramModule.io.en    := startReading
//  sramModule.io.push  := true.B
//  sramModule.io.pop   := false.B
////  sramModule.io.dataIn := survivalPath
//
//}
//
