package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: 48-bit minimum latency Viterbi-Decoder to extract information from header block
class HeaderExtractor[T <: Data: Real](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val in        = Input(Vec((params.n * params.H), SInt(2.W)))      // from De-Puncturing
    val isHead    = Input(Bool())                                     // from arbiter
    val headInfo  = Decoupled(DecodeHeadBundle())                     // to De-Puncturing
  })
  // ***************************************************************************************************
  // ************************************* Path Metric Calculation *************************************
  // ***************************************************************************************************
  val H                   = params.H          // # of bits in header block
  val m                   = params.m          // # of memory for convolutional coding
  val N                   = params.nStates    // # of possible states
  val pmBits              = params.pmBits
  val survivalPath        = SyncReadMem(H, Vec(N, UInt(m.W)))
  val pmRegs              = RegInit(VecInit(Seq.fill(N)(0.U(pmBits.W))))
  val addrReg             = RegInit(0.U(log2Ceil(H).W))
  val addrWire            = Wire(UInt(log2Ceil(H).W))
  val counter             = RegInit(0.U(log2Ceil(H*2).W))
  val SPcalcCompleted     = RegInit(false.B)    // flag for SP & PM calculation
  val initVal             = 16.U(pmBits.W)
  val numRows             = math.pow(2.0, (m-1).asInstanceOf[Double]).asInstanceOf[Int]
  val tmpSP               = Wire(Vec(N, Vec(params.numInputs, UInt(m.W))))
  val decodeReg           = Reg(Vec(H, UInt(params.k.W)))
  for (currentInput <- 0 until params.numInputs){
    for (currentStates <- 0 until N){
      tmpSP(currentStates/2+currentInput*numRows)(currentStates%2) := currentStates.U
    }
  }

  val branchMetricModule: BranchMetric[T] = Module(new BranchMetric[T](params))
  (0 until params.n).map(i => {branchMetricModule.io.in(i) := io.in(counter + i.U)})

  addrWire  := addrReg
  addrReg   := addrWire

  // when Arbiter raised io.hdrEnd, reset pmRegs and raise startDecode
  when (io.isHead === false.B){
    when(params.tailBitingEn.asBool() === false.B) {          // for zero-flushing
      pmRegs(0) := 0.U
      (1 until N).map(i => { pmRegs(i) := initVal})
    }
  }

  when(io.isHead === true.B && SPcalcCompleted === false.B){
    // temporary matrix for Path Metric calculation
    val tmpPM   = Wire(Vec(N, Vec(params.numInputs, UInt(pmBits.W))))
    // temporary matrix for Branch Metric calculation
    val tmpBM   = Wire(Vec(N, Vec(params.numInputs, UInt((log2Ceil(params.n)+1).W))))
    // temporary matrix for ACS (mainly for accumulation)
    val tmpAcc  = Wire(Vec(N, Vec(params.numInputs, UInt(pmBits.W))))
    // wire for syncreadmem.write()
    val tmpMemWire          = Wire(Vec(N, UInt(m.W)))

    for (currentInput <- 0 until params.numInputs){
      for (currentStates <- 0 until N){
        tmpPM(currentStates/2+currentInput*numRows)(currentStates%2)      := pmRegs(currentStates)
        tmpBM(currentStates/2 + currentInput*numRows)(currentStates % 2)  := branchMetricModule.io.out_dec(currentStates)(currentInput)
        tmpAcc(currentStates)(currentInput) := tmpPM(currentStates)(currentInput) + tmpBM(currentStates)(currentInput)
      }
    }

    for (nRow <- 0 until N){
      when(tmpAcc(nRow)(0) < tmpAcc(nRow)(1)){
        pmRegs(nRow) := tmpAcc(nRow)(0)
        tmpMemWire(nRow) := tmpSP(nRow)(0)
      }.otherwise{
        pmRegs(nRow) := tmpAcc(nRow)(1)
        tmpMemWire(nRow) := tmpSP(nRow)(1)
      }
    }
    survivalPath.write(addrWire, tmpMemWire)

    when(addrReg < (H-1).U){
      addrReg := addrReg + 1.U
      counter := counter + params.n.U
    }.otherwise{
      addrReg := addrReg + 1.U
      SPcalcCompleted := true.B           // need to reset this later
    }
  }

  // *************************************************************************************
  // ************************************* Traceback *************************************
  // *************************************************************************************

  // declare variables for decoding process
  val tmpSPReg          = RegInit(0.U(m.W))
//  val tmpSPWire         = Wire(UInt(m.W))
  val lengthInfoWire    = Wire(Vec(12, UInt(12.W)))
  val outValid          = RegInit(false.B)
  val tbCounter         = RegInit(0.U(log2Ceil(H).W))
  val en_mem            = true.B
  val readMemWire       = Wire(Vec(N, UInt(m.W)))

  // start decoding !
  when(SPcalcCompleted === true.B) {
    val tmpPMMin          = Wire(Vec(N - 1, UInt(pmBits.W)))
    val tmpPMMinIndex     = Wire(Vec(N - 1, UInt(m.W)))

    // find minimum in PM
    tmpPMMin(0)           := Mux(pmRegs(0) < pmRegs(1), pmRegs(0), pmRegs(1))
    tmpPMMinIndex(0)      := Mux(pmRegs(0) < pmRegs(1), 0.U, 1.U)
    for (i <- 1 until N - 1) {
      tmpPMMin(i)         := Mux(tmpPMMin(i - 1) < pmRegs(i + 1), tmpPMMin(i - 1), pmRegs(i + 1))
      tmpPMMinIndex(i)    := Mux(tmpPMMin(i - 1) < pmRegs(i + 1), tmpPMMinIndex(i - 1), (i + 1).U)
    }

    when(counter === (params.n*(H-1)).U){
      decodeReg(counter/(params.n.U)) := tmpPMMinIndex(N-2) >> (m-1)     // grab the minimum PM
      tmpSPReg                        := tmpPMMinIndex(N-2)
    }.otherwise{
      decodeReg(counter/(params.n.U)) := readMemWire(tmpSPReg) >> (m-1)
      tmpSPReg                        := readMemWire(tmpSPReg)
    }

     when(counter === (params.n*(H-1)).U){
       counter := counter - params.n.U
     }.elsewhen(counter > 0.U) {
      addrWire := addrReg - 1.U
      counter := counter - params.n.U
    }.otherwise{
      addrReg := 0.U
      counter := 0.U
      SPcalcCompleted := false.B
      outValid := true.B
      (0 until N).map(i => {pmRegs(i) := 0.U})
      (0 until N*H).map(i => {survivalPath.write(i.U, VecInit(Seq.fill(N){0.U}))})
    }
  }

  readMemWire := survivalPath.read(addrWire - 1.U)

  when(io.headInfo.fire()){
    outValid      := false.B
  }
  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := decodeReg(i)             })
  (0 until 12).map(i  => { lengthInfoWire(i)         := decodeReg(5 + i) << (i)  })
  io.headInfo.bits.dataLen    := lengthInfoWire.reduce(_ + _) << 3  // contains number of data "octets" in a packet
  io.headInfo.valid           := outValid

}

