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
  val survivalPath        = SyncReadMem(N*H, UInt(m.W))
  val pmRegs              = RegInit(VecInit(Seq.fill(N)(0.U(pmBits.W))))
  val addrReg             = RegInit(0.U(log2Ceil(N*H).W))
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
        survivalPath.write(addrReg + nRow.U, tmpSP(nRow)(0))
        printf(p"pmRegs = ${tmpAcc(nRow)(0)} ****** \n")
        printf(p"tmpSP = ${tmpSP(nRow)(0)} ****** \n")
      }.otherwise{
        pmRegs(nRow) := tmpAcc(nRow)(1)
        survivalPath.write(addrReg + nRow.U, tmpSP(nRow)(1))
        printf(p"pmRegs = ${tmpAcc(nRow)(1)} ****** \n")
        printf(p"tmpSP = ${tmpSP(nRow)(1)} ****** \n")
      }
    }

//    for(nRow <- 0 until N){
//      printf(p"pmReg = ${pmRegs(nRow)} ****** \n")
//    }

    when(addrReg < (N*(H-1).U)){
      addrReg := addrReg + N.U
      counter := counter + params.n.U
    }.otherwise{
      SPcalcCompleted := true.B           // need to reset this later
    }
    printf(p"counter = ${counter} ****** \n")
  }

//  printf(p"SPcalcCompleted = ${SPcalcCompleted} ****** \n")
//    printf(p"addrReg = ${addrReg} ****** \n")
  // *************************************************************************************
  // ************************************* Traceback *************************************
  // *************************************************************************************

  // declare variables for decoding process
  val tmpSPReg          = RegInit(0.U(m.W))
  val lengthInfoWire    = Wire(Vec(12, UInt(12.W)))
  val outValid          = RegInit(false.B)
  val tbCounter         = RegInit(0.U(log2Ceil(H).W))
  val en_mem            = true.B
  val addrWire          = Wire(UInt(5.W))
  addrWire := addrReg + tmpSPReg
  val outputMem = survivalPath.read(addrReg + tmpSPReg, en_mem)

  printf(p"addrReg = ${addrReg} \n")
  printf(p"tmpSPReg = ${tmpSPReg} \n")
  //      printf(p"survival Path = ${survivalPath.read(addrReg + tmpSPReg, en_mem)} \n")
  printf(p"survival Path = ${survivalPath.read(addrWire, en_mem)} \n")
  printf(p"survival Path ***OUTMEM = ${outputMem} \n")
  printf(p"survival Path ***HARD = ${survivalPath.read(17.U, en_mem)} \n")
  for(i <- 0 until 17){
    printf(p"addrWire = ${addrWire} \n")
  }

  printf(p"addrReg + tmpSPReg = ${addrReg + tmpSPReg}} \n")

//  tmpSPReg                  := survivalPath.read(addrReg + tmpSPReg, en_mem)

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
//    printf(p"tmpPMMinIndex = ${tmpPMMinIndex(N-2)} \n")
//    printf(p"tmpPMMin = ${tmpPMMin(N-2)} \n")
//    addrWire := addrReg + tmpSPReg
    when(counter === (2*(H-1)).U){
      printf("case 1 ************ \n")
      printf(p"addrReg = ${addrReg} \n")
      decodeReg(counter/(params.n.U))  := tmpPMMinIndex(N-2) >> (m-1)     // grab the minimum PM
      tmpSPReg                  := tmpPMMinIndex(N-2)
      printf(p"tmpPMMinIndex(N-2) = ${tmpPMMinIndex(N-2)}")
    }.otherwise{
//      printf("case 2 ************ \n")
//      printf(p"addrReg = ${addrReg} \n")
//      printf(p"tmpSPReg = ${tmpSPReg} \n")
////      printf(p"survival Path = ${survivalPath.read(addrReg + tmpSPReg, en_mem)} \n")
//      printf(p"survival Path = ${survivalPath.read(addrWire, en_mem)} \n")
//      printf(p"survival Path ***ASDASDSA = ${survivalPath.read(20.U, en_mem)} \n")
//      printf(p"addrReg + tmpSPReg = ${addrReg + tmpSPReg}} \n")
      decodeReg(counter/(params.n.U))  := outputMem >> (m-1)
      tmpSPReg                  := outputMem
    }

//    printf(p"survival path = ${survivalPath.read(addrReg + tmpSPReg)} \n")
//    printf(p"decoded data = ${decodeReg(counter/2.U)} \n")
    when(counter === (params.n*(H-1)).U){
      counter := counter - params.n.U
//      tbCounter := tbCounter - 1.U
    }.elsewhen(counter > 0.U) {
      addrReg := addrReg - N.U
      counter := counter - params.n.U
//      tbCounter := tbCounter - 1.U
    }.otherwise{
      addrReg := 0.U
      counter := 0.U
      SPcalcCompleted := false.B
      outValid := true.B
      (0 until N).map(i => {pmRegs(i) := 0.U})
      (0 until N*H).map(i => {survivalPath.write(i.U, 0.U)})
    }
    tbCounter := tbCounter + 1.U
    printf(p"counter = ${counter} ****** \n")
  }

  when(io.headInfo.fire()){
    outValid      := false.B
  }

//  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := decodeReg(i)             })
//  (0 until 12).map(i  => { lengthInfoWire(i)         := decodeReg(5 + i) << (i)  })
//  io.headInfo.bits.dataLen    := lengthInfoWire.reduce(_ + _) << 3  // contains number of data "octets" in a packet
//  io.headInfo.valid           := outValid

  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := decodeReg(i)             })
  (0 until 12).map(i  => { lengthInfoWire(i)         := 0.U  })
  io.headInfo.bits.dataLen    := lengthInfoWire.reduce(_ + _) << 3  // contains number of data "octets" in a packet
  io.headInfo.valid           := outValid
//  for(i <- (0 until H).reverse){
//    printf(p"decoded data = ${decodeReg(i)} \n")
//  }
//  for(i <- (0 until N*H).reverse){
//    printf(p"survival Path @ (addr = $i) = ${survivalPath.read(i.U)} \n")
//  }
  printf(p"********************** \n")
}

