package modem

import chisel3._
import chisel3.util._
import dsptools.numbers
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: This module calculates branch metric for every n-bit reception
class BranchMetric_backup[T <: Data:Real](params: CodingParams[T]) extends Module {
  require(params.m >= 1)
  require(params.k >= 1)
  require(params.n >= 2)

  val io = IO(new Bundle {
    val in        = Input(Vec(params.n, params.protoBits.cloneType))
    val out       = Output(Vec(params.nStates, Vec(params.numInputs, Vec(params.n, params.protoBits.cloneType))))
//    val out_frac  = Output(Vec(params.nStates, Vec(params.numInputs, Vec(params.n, params.protoBits.cloneType))))
//    val out_dec   = Output(Vec(params.nStates, Vec(params.numInputs, UInt((log2Ceil(params.n)+1).W))))
    val out_dec   = Output(Vec(params.nStates, Vec(params.numInputs, params.protoBits.cloneType)))
  })

  val trellisObj  = new Trellis[T](params)

  for (currentStates <- 0 until params.nStates) {
    for (currentInputs <- 0 until params.numInputs) {
      for (r <- 0 until params.n) {
        if(params.softDecision == false) { // hard decision
          when(io.in(r) === ConvertableTo[T].fromInt(0)) {
            io.out(currentStates)(currentInputs)(r) := ConvertableTo[T].fromInt(0)
          }.otherwise {
            io.out(currentStates)(currentInputs)(r) := Mux(io.in(r) === ConvertableTo[T].fromInt(2 * trellisObj.output_table(currentStates)(currentInputs)(r) - 1), ConvertableTo[T].fromInt(0), ConvertableTo[T].fromInt(1))
          }
        } else {
          io.out(currentStates)(currentInputs)(r) := ConvertableTo[T].fromInt(trellisObj.output_table(currentStates)(currentInputs)(r))*io.in(r)*(-1)
        }
      }
      io.out_dec(currentStates)(currentInputs)  := io.out(currentStates)(currentInputs).reduce(_ + _)
    }
  }
}
