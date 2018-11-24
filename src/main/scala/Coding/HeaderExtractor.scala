package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
class HeaderExtractor[T <: Data: Real](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val in        = Input(Vec((params.n * params.H), SInt(2.W)))
    val inReady   = Input(UInt(1.W))
    val isHead    = Input(Bool())
    val headInfo  = Decoupled(DecodeHeadBundle())
  })
  // ************************************* Path Metric Calculation *************************************
  val H                   = params.H
  val L                   = params.L
  val D                   = params.D
  val m                   = params.m
  val N                   = params.nStates
  val inReadyReg          = RegInit(0.U(1.W))
  inReadyReg := io.inReady
  val startDecode         = RegInit(0.U(1.W))
  val trellisObj          = new Trellis[T](params)
  val nextStateTable      = trellisObj.nextstate_table
//  val branchMetricModule  = Vec(H, Module(new BranchMetric[T](params)).io)
  val branchMetricModule  = VecInit(Seq.fill(H)(Module(new BranchMetric[T](params)).io))
//  val branchMetricModule  = Seq.fill(H)(Module(new BranchMetric[T](params)).io)
  for(i <- 0 until (params.n*H)){
    branchMetricModule(i/params.n).in(i % (params.n)) := io.in(i)
  }

  val numRows             = math.pow(2.0, (params.m-1).asInstanceOf[Double]).asInstanceOf[Int]
  val tmpSP               = Wire(Vec(H, Vec(N, Vec(params.numInputs, UInt(params.m.W)))))
  for (hdr <- 0 until H){
    for (currentInput <- 0 until params.numInputs){
      for (currentStates <- 0 until N){
        tmpSP(hdr)(currentStates/2+currentInput*numRows)(currentStates%2) := currentStates.U
      }
    }
  }

  val survivalPath        = RegInit(VecInit(Seq.fill(H)(VecInit(Seq.fill(N)(0.U(params.m.W))))))
  val pmRegs              = RegInit(VecInit(Seq.fill(H+1)(VecInit(Seq.fill(N)(0.U(params.pmBits.W))))))
  val SPcalcCompleted     = RegInit(false.B)

  // when pktStart or reset is raised
  when ((inReadyReg =/= io.inReady) && (io.inReady === 1.U)){
    when(params.tailBitingEn.asBool() === true.B) { // tail-biting

    }.otherwise{            // for zero-flushing
      pmRegs(0)(0) := 0.U
      (1 until N).map(i => { pmRegs(0)(i) := 100.U})
    }
    startDecode := 1.U
  }

  printf(p"**************** startDecode = ${startDecode} **************** \n")
  when (startDecode === 1.U){
    // temporary matrix for Path Metric calculation
    // TODO: How to find the maximum # of bits for PM ?
    val tmpPM   = Wire(Vec(H+1, Vec(N, Vec(params.numInputs, UInt(params.pmBits.W)))))
    // temporary matrix for Branch Metric calculation
    val tmpBM   = Wire(Vec(H+1, Vec(N, Vec(params.numInputs, UInt((log2Ceil(params.n)+1).W)))))
    // temporary matrix for ACS (mainly for accumulation)
    val tmpAcc  = Wire(Vec(H+1, Vec(N, Vec(params.numInputs, UInt(params.pmBits.W)))))
    val pmWire  = Wire(Vec(H+1, Vec(N, UInt(params.pmBits.W))))

    // for hdr = 0
    for (currentInput <- 0 until params.numInputs) {
      for (currentStates <- 0 until N) {
        if (currentStates == 0) {
          tmpPM(0)(currentStates/2 + currentInput*numRows)(currentStates % 2) := 0.U
          pmRegs(0)(currentStates) := 0.U
          pmWire(0)(currentStates) := 0.U
        } else {
          tmpPM(0)(currentStates/2 + currentInput*numRows)(currentStates % 2) := 100.U
          pmRegs(0)(currentStates) := 100.U
          pmWire(0)(currentStates) := 100.U
        }
        tmpBM(0)(currentStates/2 + currentInput*numRows)(currentStates % 2) := 0.U
        tmpAcc(0)(currentStates)(currentInput) := tmpPM(0)(currentStates)(currentInput) + tmpBM(0)(currentStates)(currentInput)
      }
    }

    for(hdr <- 1 until H+1) {
      for (currentInput <- 0 until params.numInputs){
        for (currentStates <- 0 until N){
          tmpPM(hdr)(currentStates/2 + currentInput*numRows)(currentStates%2) := pmWire(hdr-1)(currentStates)
          tmpBM(hdr)(currentStates/2 + currentInput*numRows)(currentStates%2) := branchMetricModule(hdr-1).out_dec(currentStates)(currentInput)
          tmpAcc(hdr)(currentStates)(currentInput) := tmpPM(hdr)(currentStates)(currentInput) + tmpBM(hdr)(currentStates)(currentInput)
        }
      }

      if(hdr < H+1) {
        for (nRow <- 0 until N){
          when(tmpAcc(hdr)(nRow)(0) < tmpAcc(hdr)(nRow)(1)){
            pmRegs(hdr)(nRow)         := tmpAcc(hdr)(nRow)(0)
            pmWire(hdr)(nRow)         := tmpAcc(hdr)(nRow)(0)
            survivalPath(hdr-1)(nRow) := tmpSP(hdr-1)(nRow)(0)
          }.otherwise{
            pmRegs(hdr)(nRow)         := tmpAcc(hdr)(nRow)(1)
            pmWire(hdr)(nRow)         := tmpAcc(hdr)(nRow)(1)
            survivalPath(hdr-1)(nRow) := tmpSP(hdr-1)(nRow)(1)
          }
        }
      }
    }
    SPcalcCompleted := true.B           // need to reset this later
  }
  for(i <- 0 until H){
    for(j <- 0 until N){
      printf(p"************ i = ${i}, j = ${j}, survivalPath = ${survivalPath(i)(j)} \n")
    }
  }

  // ************************************* Traceback *************************************
  // declare variables for decoding process
  val outValid    = RegInit(false.B)
  val tmpPMMin          = Wire(Vec(N - 1, UInt(m.W)))
  val tmpPMMinIndex     = Wire(Vec(N - 1, UInt(m.W)))
  val tmpPMMinReg       = RegInit(VecInit(Seq.fill(N - 1)(0.U(m.W))))
  val tmpPMMinIndexReg  = RegInit(VecInit(Seq.fill(N - 1)(0.U(m.W))))
  val tmpSPReg          = RegInit(VecInit(Seq.fill(N)(0.U(m.W))))
  val trackValid        = RegInit(VecInit(Seq.fill(3)(0.U(1.W))))
  val decodeReg         = Reg(Vec(H, UInt(params.k.W)))
  val memReg            = RegInit(VecInit(Seq.fill(N * (H-1))(0.U(m.W))))
  val lengthInfoWire    = Wire(Vec(12, UInt(12.W)))

  // find minimum in PM
  tmpPMMin(0)           := Mux(pmRegs(H)(0) < pmRegs(H)(1), pmRegs(H)(0), pmRegs(H)(1))
  tmpPMMinIndex(0)      := Mux(pmRegs(H)(0) < pmRegs(H)(1), 0.U, 1.U)
  for (i <- 1 until N - 1) {
    tmpPMMin(i)         := Mux(tmpPMMin(i-1) < pmRegs(H)(i+1), tmpPMMin(i-1), pmRegs(H)(i+1))
    tmpPMMinIndex(i)    := Mux(tmpPMMin(i-1) < pmRegs(H)(i+1), tmpPMMinIndex(i-1), (i+1).U)
  }

  // when the decoding just started, it starts decoding after it receives D+L bits
  when((io.isHead === 1.U) && (SPcalcCompleted === true.B)) {
    tmpPMMinReg         := tmpPMMin
    tmpPMMinIndexReg    := tmpPMMinIndex
    tmpSPReg            := survivalPath(H-1)
    trackValid(0)       := 1.U                    // trackValid is used to raise/lower io.out.valid signal
    SPcalcCompleted     := false.B                // decodeStart indicates whether this is the first time decoding
//    decodeReg(H-1)      := pmRegs(H)(tmpPMMinIndex(N-2))    // grab the minimum PM
    decodeReg(H-1)      := tmpPMMinIndex(N-2)     // grab the minimum PM
  }
  when(trackValid(0) === 1.U){
    (2 to 1 by -1).map(i => {trackValid(i) := trackValid(i-1)})
  }
  printf(p"**************** trackValid(2) = ${trackValid(2)} **************** \n")
  // Start decoding
  when(io.isHead === 1.U) {
    val tmpSPforTB  = Wire(Vec(H-1, UInt(m.W)))
    tmpSPforTB(H-2) := tmpSPReg(tmpPMMinIndexReg(N - 2)) // grab the minimum PM
    decodeReg(H-2)  := tmpSPforTB(H-2) >> (m-1)
    printf(p"back tracking = ${tmpSPforTB(H-2)} \n ")
    for (i <- (0 until H-2).reverse) {
      tmpSPforTB(i) := survivalPath(i+1)(tmpSPforTB(i+1))
      printf(p"back tracking = ${tmpSPforTB(i)} \n ")
      decodeReg(i) := tmpSPforTB(i) >> (m-1) // get MSB
    }
  }

  when(trackValid(2) === 1.U) {
    outValid      := true.B
    (0 to 2).map(i => {trackValid(i)  := 0.U})
  }
  when(io.headInfo.fire()){
    outValid      := false.B
  }
  for(i <- (0 until H).reverse){
    printf(p"**************** i = ${i}, decodeReg = ${decodeReg(i)} **************** \n")
  }
  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := decodeReg(i)             })
  (0 until 12).map(i  => { lengthInfoWire(i)         := decodeReg(5 + i) << (i)  })
  io.headInfo.bits.dataLen    := lengthInfoWire.reduce(_ + _)
  io.headInfo.valid           := outValid

//  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := 0.U             })
//  (0 until 12).map(i  => { lengthInfoWire(i)  := 0.U             })
//  io.headInfo.bits.dataLen    := 4.U
//  io.headInfo.valid           := true.B
}

