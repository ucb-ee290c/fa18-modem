package Coding

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

class PathMetric[T <: Data: Real](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val in        = Input(Vec(params.n, UInt(1.W)))
//    val in2       = Input(Vec(params.n, T))
    val inReady   = Input(UInt(1.W))
    val outPM       = Output(Vec(params.nStates, UInt(params.pmBits.W)))  // storing Path Metric
    val outSP       = Output(Vec(params.nStates, UInt(params.m.W)))       // storing Survival Path
  })
  val inReadyReg          = RegInit(0.U(1.W))
  inReadyReg := io.inReady
  val startDecode         = RegInit(0.U(1.W))
  val trellisObj          = new Trellis[T](params)
  val nextStateTable      = trellisObj.nextstate_table
  val branchMetricModule  = Module(new BranchMetric[T](params))
  (0 until params.n).map(i => {branchMetricModule.io.in(i) := io.in(i)})

  val numRows             = math.pow(2.0, (params.m-1).asInstanceOf[Double]).asInstanceOf[Int]
  val tmpSP               = Wire(Vec(params.nStates, Vec(params.numInputs, UInt(params.m.W))))
  for (currentInput <- 0 until params.numInputs){
    for (currentStates <- 0 until params.nStates){
      tmpSP(currentStates/2+currentInput*numRows)(currentStates%2) := currentStates.U
    }
  }

  val survivalPath        = RegInit(VecInit(Seq.fill(params.nStates)(0.U(params.m.W))))
  val pmRegs              = RegInit(VecInit(Seq.fill(params.nStates)(0.U(params.pmBits.W))))
  val initVal             = 100.U(params.pmBits.W)
  // when pktStart or reset is raised
  when ((inReadyReg =/= io.inReady) && (io.inReady === 1.U)){
    when(params.tailBitingEn.asBool() === true.B) {

    }.otherwise{            // for zero-flushing
      pmRegs(0) := 0.U
      (1 until params.nStates).map(i => { pmRegs(i) := initVal})
    }
    startDecode := 1.U
  }

  when (startDecode === 1.U){
    // temporary matrix for Path Metric calculation
    // TODO: How to find the maximum # of bits for PM ?
    val tmpPM               = Wire(Vec(params.nStates, Vec(params.numInputs, UInt(params.pmBits.W))))
    for (currentInput <- 0 until params.numInputs){
      for (currentStates <- 0 until params.nStates){
        tmpPM(currentStates/2+currentInput*numRows)(currentStates%2) := pmRegs(currentStates)
      }
    }

    // temporary matrix for Branch Metric calculation
    val tmpBM               = Wire(Vec(params.nStates, Vec(params.numInputs, UInt((log2Ceil(params.n)+1).W))))
    for (currentInput <- 0 until params.numInputs){
      for (currentStates <- 0 until params.nStates){
        tmpBM(currentStates/2 + currentInput*numRows)(currentStates % 2) := branchMetricModule.io.out_dec(currentStates)(currentInput)
      }
    }

    // temporary matrix for ACS (mainly for accumulation)
    val tmpAcc = Wire(Vec(params.nStates, Vec(params.numInputs, UInt(params.pmBits.W))))
    for (nCol <- 0 until params.numInputs){
      for (nRow <- 0 until params.nStates){
        tmpAcc(nRow)(nCol) := tmpPM(nRow)(nCol) + tmpBM(nRow)(nCol)
      }
    }

    for (nRow <- 0 until params.nStates){
        when(tmpAcc(nRow)(0) < tmpAcc(nRow)(1)){
          pmRegs(nRow)        := tmpAcc(nRow)(0)
          survivalPath(nRow)  := tmpSP(nRow)(0)
        }.otherwise{
          pmRegs(nRow)        := tmpAcc(nRow)(1)
          survivalPath(nRow)  := tmpSP(nRow)(1)
        }
    }
  }
  io.outPM := pmRegs
  io.outSP := survivalPath

  // ******** Memory Configuration ********
//  val sramModule  = Module(new SRAM[T](params))
//  val startReading := true.B
//  sramModule.io.en    := startReading
//  sramModule.io.push  := true.B
//  sramModule.io.pop   := false.B
//  sramModule.io.dataIn := survivalPath

}

