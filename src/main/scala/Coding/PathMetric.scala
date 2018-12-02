package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// calculates brach-metric and path-metric, then find the survival path
class PathMetric[T <: Data: Real, U <: Data: Real](params: CodingParams[T, U]) extends Module {
  val io = IO(new Bundle {
    val in          = Input(Vec(params.n, params.protoBits.cloneType))
    val hdrEnd      = Input(Bool())
    val inEnable    = Input(Bool())
    val outPM       = Output(Vec(params.nStates, params.pmBitType.cloneType))  // storing Path Metric
    val outSP       = Output(Vec(params.nStates, UInt(params.m.W)))       // storing Survival Path
    val outEnable   = Output(Bool())
  })
  val H                   = params.H          // # of bits in header block
  val m                   = params.m          // # of memory for convolutional coding
  val N                   = params.nStates    // # of possible states
  val pmBits              = params.pmBits     // max # of bits for path metric register
  val startDecode         = RegInit(0.U(1.W))
  val branchMetricModule: BranchMetric[T, U] = Module(new BranchMetric[T, U](params))
  (0 until params.n).map(i => {branchMetricModule.io.in(i) := io.in(i)})

  val survivalPath        = RegInit(VecInit(Seq.fill(N)(0.U(m.W))))
  val pmRegs              = Reg(Vec(N, params.pmBitType.cloneType))     //TODO: to enable tail-biting decoding, I need RegInit
  val initVal             = ConvertableTo[U].fromInt(16)

  val numRows             = math.pow(2.0, (params.m-1).asInstanceOf[Double]).asInstanceOf[Int]
  val tmpSP               = Wire(Vec(N, Vec(params.numInputs, UInt(params.m.W))))

  val enReg               = RegInit(false.B)

  for (currentInput <- 0 until params.numInputs){
    for (currentStates <- 0 until params.nStates){
      tmpSP(currentStates/2+currentInput*numRows)(currentStates%2) := currentStates.U
    }
  }

  // when Arbiter raised io.hdrEnd, reset pmRegs and raise startDecode
  when (io.hdrEnd === true.B){
    when(params.tailBitingEn.asBool() === false.B) {          // for zero-flushing
      pmRegs(0) := ConvertableTo[U].fromInt(0)
      (1 until N).map(i => { pmRegs(i) := initVal})
    }
    startDecode := 1.U
  }

  when(io.inEnable === true.B){
    when (startDecode === 1.U){
      // temporary matrix for Path Metric calculation
      val tmpPM       = Wire(Vec(N, Vec(params.numInputs, params.pmBitType.cloneType)))
      // temporary matrix for Branch Metric calculation
      val tmpBM       = Wire(Vec(N, Vec(params.numInputs, params.BMoutdec.cloneType)))
      // temporary matrix for ACS (mainly for accumulation)
      val tmpAcc      = Wire(Vec(N, Vec(params.numInputs, params.pmBitType.cloneType)))

      val minPM       = Wire(Vec(params.nStates, params.pmBitType.cloneType))

      for (currentInput <- 0 until params.numInputs){
        for (currentStates <- 0 until params.nStates){
          tmpPM(currentStates/2+currentInput*numRows)(currentStates%2)      := pmRegs(currentStates)
          tmpBM(currentStates/2 + currentInput*numRows)(currentStates % 2)  := branchMetricModule.io.out_dec(currentStates)(currentInput)
          tmpAcc(currentStates)(currentInput) := tmpPM(currentStates)(currentInput) + tmpBM(currentStates)(currentInput)
        }
      }

      for (nRow <- 0 until params.nStates){
        when(tmpAcc(nRow)(0) < tmpAcc(nRow)(1)){
          minPM(nRow)         := tmpAcc(nRow)(0)
          survivalPath(nRow)  := tmpSP(nRow)(0)
        }.otherwise{
          minPM(nRow)        := tmpAcc(nRow)(1)
          survivalPath(nRow)  := tmpSP(nRow)(1)
        }
      }

      val tmpPMMin = minPM.reduceLeft((x, y) => {
        val comp = x < y
        (Mux(comp, x, y))
      })

      for (nRow <- 0 until params.nStates){
        when(tmpPMMin >= initVal){
          pmRegs(nRow)  := minPM(nRow) - initVal
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

