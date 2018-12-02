package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: This module calculates branch metric for every n-bit reception
class BranchMetric[T <: Data: Real, U <: Data: Real](params: CodingParams[T, U]) extends Module {
  require(params.m >= 1)
  require(params.k >= 1)
  require(params.n >= 2)

  val io = IO(new Bundle {
    val in        = Input(Vec(params.n, params.protoBits.cloneType))
    val out       = Output(Vec(params.nStates, Vec(params.numInputs, Vec(params.n, params.BMout.cloneType))))
    val out_dec   = Output(Vec(params.nStates, Vec(params.numInputs, params.BMoutdec.cloneType)))
  })

  val trellisObj  = new Trellis[T, U](params)

  for (currentStates <- 0 until params.nStates) {
    for (currentInputs <- 0 until params.numInputs) {
      for (r <- 0 until params.n) {
        if(params.softDecision == false) { // hard decision. Calculate Hamming distance
          when(io.in(r) === ConvertableTo[T].fromInt(0)) {    // for the punctured value, skip hamming distance calculation
            io.out(currentStates)(currentInputs)(r) := ConvertableTo[U].fromInt(0)
          }.otherwise {
            io.out(currentStates)(currentInputs)(r) := Mux(io.in(r) === ConvertableTo[T].fromInt(2 * trellisObj.output_table(currentStates)(currentInputs)(r) - 1), ConvertableTo[U].fromInt(0), ConvertableTo[U].fromInt(1))
          }
        } else {    // Soft-Input decoding. Using L1 Norm to calculate Euclidean distance
          io.out(currentStates)(currentInputs)(r) := ConvertableTo[T].fromInt(trellisObj.output_table(currentStates)(currentInputs)(r))*io.in(r)*(-1)
        }
      }
      io.out_dec(currentStates)(currentInputs)  := io.out(currentStates)(currentInputs).reduce(_ + _)
    }
  }
}