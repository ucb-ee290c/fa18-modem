package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// calculates brach-metric and path-metric, then find the survival path
class PathMetric[T <: Data: Real](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val in          = Input(Vec(params.n, SInt(2.W)))
    val hdrEnd      = Input(Bool())
    val inEnable    = Input(Bool())
    val outPM       = Output(Vec(params.nStates, UInt(params.pmBits.W)))  // storing Path Metric
    val outSP       = Output(Vec(params.nStates, UInt(params.m.W)))       // storing Survival Path
    val outEnable   = Output(Bool())
  })
  val startDecode         = RegInit(0.U(1.W))
  val branchMetricModule: BranchMetric[T] = Module(new BranchMetric[T](params))
  (0 until params.n).map(i => {branchMetricModule.io.in(i) := io.in(i)})

  val survivalPath        = RegInit(VecInit(Seq.fill(params.nStates)(0.U(params.m.W))))
  val pmRegs              = RegInit(VecInit(Seq.fill(params.nStates)(0.U(params.pmBits.W))))
  val initVal             = 16.U(params.pmBits.W)

  val numRows             = math.pow(2.0, (params.m-1).asInstanceOf[Double]).asInstanceOf[Int]
  val tmpSP               = Wire(Vec(params.nStates, Vec(params.numInputs, UInt(params.m.W))))

  val enReg               = RegInit(false.B)

  for (currentInput <- 0 until params.numInputs){
    for (currentStates <- 0 until params.nStates){
      tmpSP(currentStates/2+currentInput*numRows)(currentStates%2) := currentStates.U
    }
  }

  // when Arbiter raised io.hdrEnd, reset pmRegs and raise startDecode
  when (io.hdrEnd === true.B){
    when(params.tailBitingEn.asBool() === false.B) {          // for zero-flushing
      pmRegs(0) := 0.U
      (1 until params.nStates).map(i => { pmRegs(i) := initVal})
    }
    startDecode := 1.U
  }

  when(io.inEnable === true.B){
    when (startDecode === 1.U){
      // temporary matrix for Path Metric calculation
      val tmpPM   = Wire(Vec(params.nStates, Vec(params.numInputs, UInt(params.pmBits.W))))
      // temporary matrix for Branch Metric calculation
      val tmpBM   = Wire(Vec(params.nStates, Vec(params.numInputs, UInt((log2Ceil(params.n)+1).W))))
      // temporary matrix for ACS (mainly for accumulation)
      val tmpAcc  = Wire(Vec(params.nStates, Vec(params.numInputs, UInt(params.pmBits.W))))

      val minPM   = Wire(Vec(params.nStates, UInt(params.pmBits.W)))
      val tmpPMMin  = Wire(Vec(params.nStates - 1, UInt(params.m.W)))

      for (currentInput <- 0 until params.numInputs){
        for (currentStates <- 0 until params.nStates){
          tmpPM(currentStates/2+currentInput*numRows)(currentStates%2)      := pmRegs(currentStates)
          tmpBM(currentStates/2 + currentInput*numRows)(currentStates % 2)  := branchMetricModule.io.out_dec(currentStates)(currentInput)
          tmpAcc(currentStates)(currentInput) := tmpPM(currentStates)(currentInput) + tmpBM(currentStates)(currentInput)
        }
      }

      for (nRow <- 0 until params.nStates){
        when(tmpAcc(nRow)(0) < tmpAcc(nRow)(1)){
//          pmRegs(nRow)        := tmpAcc(nRow)(0)
          minPM(nRow)         := tmpAcc(nRow)(0)
          survivalPath(nRow)  := tmpSP(nRow)(0)
        }.otherwise{
//          pmRegs(nRow)        := tmpAcc(nRow)(1)
          minPM(nRow)        := tmpAcc(nRow)(1)
          survivalPath(nRow)  := tmpSP(nRow)(1)
        }
      }

      tmpPMMin(0)           := Mux(minPM(0) < minPM(1), minPM(0), minPM(1))
      for (i <- 1 until params.nStates - 1) {
        tmpPMMin(i)         := Mux(tmpPMMin(i - 1) < minPM(i + 1), tmpPMMin(i - 1), minPM(i + 1))
      }

      for (nRow <- 0 until params.nStates){
        when(tmpPMMin(params.nStates - 2) >= 16.U){
          pmRegs(nRow)  := minPM(nRow) - 16.U
        }.otherwise{
          pmRegs(nRow)  := minPM(nRow)
        }
      }
    }
  }

  enReg         := io.inEnable
  io.outEnable  := enReg
  io.outPM      := pmRegs
  io.outSP      := survivalPath
}

