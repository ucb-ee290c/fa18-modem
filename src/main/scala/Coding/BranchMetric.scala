package modem

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

class BranchMetric[T <: Data](params: CodingParams[T]) extends Module {
  require(params.m > 1)
  require(params.k > 0)
  require(params.n > 0)

  val io = IO(new Bundle {
//    val in        = Input(Vec(params.n, UInt(1.W)))
    val in        = Input(Vec(params.n, SInt(2.W)))
    val out       = Output(Vec(params.nStates, Vec(params.numInputs, Vec(params.n, UInt(params.n.W)))))
    val out_dec   = Output(Vec(params.nStates, Vec(params.numInputs, UInt((log2Ceil(params.n)+1).W))))
  })

  val trellisObj  = new Trellis[T](params)

  // below is for HARD-DECISION
  // currently not supporting for punctured input sequence
  // TODO: whenever bit is punctured, ignore the branch metric calculation
  for (currentStates <- 0 until params.nStates) {
    for (currentInputs <- 0 until params.numInputs) {
      for (r <- 0 until params.n) {
        //        io.out(currentStates)(currentInputs)(r) := io.in(r) ^ trellisObj.output_table(currentStates)(currentInputs)(r).U  // this is obsolete
        if(params.softDecision == false) {        // hard decision
          when(io.in(r) === 0.S) {
            io.out(currentStates)(currentInputs)(r) := 0.U
          }.otherwise {
            io.out(currentStates)(currentInputs)(r) := Mux(io.in(r) === (2 * trellisObj.output_table(currentStates)(currentInputs)(r) - 1).S, 0.U, (1).U)
          }
        }else{
//          io.out(currentStates)(currentInputs)(r) := -1*trellisObj.output_table(currentStates)(currentInputs)(r)*io.in(r)
        }
      }
      io.out_dec(currentStates)(currentInputs)  := io.out(currentStates)(currentInputs).reduce(_ + _)
    }
  }
}
