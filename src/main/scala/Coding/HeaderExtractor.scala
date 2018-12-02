package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: 48-bit minimum latency Viterbi-Decoder to extract information from header block
class HeaderExtractor[T <: Data: Real, U <: Data: Real](params: CodingParams[T, U]) extends Module {
  val io = IO(new Bundle {
    val in        = Input(Vec((params.n * params.H), params.protoBits.cloneType))      // from De-Puncturing
    val isHead    = Input(Bool())                                     // from arbiter
    val headInfo  = Decoupled(DecodeHeadBundle())                     // to De-Puncturing
  })
  // ***************************************************************************************************
  // ************************************* Path Metric Calculation *************************************
  // ***************************************************************************************************
  val H                   = params.H          // # of bits in header block
  val m                   = params.m          // # of memory for convolutional coding
  val N                   = params.nStates    // # of possible states

  val survivalPath        = SyncReadMem(H, Vec(N, UInt(m.W)))
  val pmRegs              = Reg(Vec(N, params.pmBitType.cloneType))   //TODO: to enable tail-biting decoding, I need RegInit
  //  val pmRegs              = RegInit(VecInit(Seq.fill(N){ConvertableTo[U].fromInt(0)}))
  val addrReg = RegInit(0.U(log2Ceil(H).W))
  val addrWire            = Wire(UInt(log2Ceil(H).W))
  val counter             = RegInit(0.U(log2Ceil(H*2).W))
  val SPcalcCompleted     = RegInit(false.B)                  // flag for SP & PM calculation
  val initVal             = ConvertableTo[U].fromInt(16)      // initial PM value for zero-flushing Viterbi Decoder
  val numRows             = math.pow(2.0, (m-1).asInstanceOf[Double]).asInstanceOf[Int]
  val tmpSP               = Wire(Vec(N, Vec(params.numInputs, UInt(m.W))))  // temporarily store survival-path info
  val decodeReg           = Reg(Vec(H, UInt(params.k.W)))     // decoded header info will be stored in decodeReg
  for (currentInput <- 0 until params.numInputs){
    for (currentStates <- 0 until N){
      tmpSP(currentStates/2+currentInput*numRows)(currentStates%2) := currentStates.U
    }
  }

  val branchMetricModule: BranchMetric[T, U] = Module(new BranchMetric[T, U](params))
  (0 until params.n).map(i => {branchMetricModule.io.in(i) := io.in(counter + i.U)})

  addrWire  := addrReg
  addrReg   := addrWire

  // when Arbiter raised io.hdrEnd, reset pmRegs and raise startDecode
  when (io.isHead === false.B){
    addrReg := 0.U
    counter := 0.U
    SPcalcCompleted := false.B

    when(params.tailBitingEn.asBool() === false.B) {          // for zero-flushing
      pmRegs(0) := ConvertableTo[U].fromInt(0)
      (1 until N).map(i => { pmRegs(i) := initVal})
    }
  }

  when(io.isHead === true.B && SPcalcCompleted === false.B){
    // temporary matrix for Path Metric calculation
    val tmpPM       = Wire(Vec(N, Vec(params.numInputs, params.pmBitType.cloneType)))
    // temporary matrix for Branch Metric calculation
    val tmpBM       = Wire(Vec(N, Vec(params.numInputs, params.BMoutdec.cloneType)))
    // temporary matrix for ACS (mainly for accumulation)
    val tmpAcc      = Wire(Vec(N, Vec(params.numInputs, params.pmBitType.cloneType)))
    // wire for SyncReadMem.write()
    val tmpMemWire  = Wire(Vec(N, UInt(m.W)))

    // add BM to current PM to calculate the next PM
    for (currentInput <- 0 until params.numInputs){
      for (currentStates <- 0 until N){
        tmpPM(currentStates/2+currentInput*numRows)(currentStates%2)      := pmRegs(currentStates)
        tmpBM(currentStates/2 + currentInput*numRows)(currentStates % 2)  := branchMetricModule.io.out_dec(currentStates)(currentInput)
        tmpAcc(currentStates)(currentInput) := tmpPM(currentStates)(currentInput) + tmpBM(currentStates)(currentInput)
      }
    }

    // ACS
    for (nRow <- 0 until N){
      when(tmpAcc(nRow)(0) < tmpAcc(nRow)(1)){
        pmRegs(nRow) := tmpAcc(nRow)(0)
        tmpMemWire(nRow) := tmpSP(nRow)(0)
      }.otherwise{
        pmRegs(nRow) := tmpAcc(nRow)(1)
        tmpMemWire(nRow) := tmpSP(nRow)(1)
      }
    }

    // record survival path in SRAM
    survivalPath.write(addrWire, tmpMemWire)

    // increase memory address pointer offset
    when(addrReg < (H-1).U){
      addrReg := addrReg + 1.U
      counter := counter + params.n.U
    }.otherwise{
      addrReg := addrReg + 1.U
      SPcalcCompleted := true.B           // Traceback starts once SPcalcCompleted is raised to high
    }
  }

  // *************************************************************************************
  // ************************************* Traceback *************************************
  // *************************************************************************************

  // declare variables for decoding process
  val tmpSPReg          = RegInit(0.U(m.W))
  val lengthInfoWire    = Wire(Vec(12, UInt(12.W)))
  val outValid          = RegInit(false.B)
  val tbCounter         = RegInit(0.U(log2Ceil(H).W))
  val en_mem            = true.B

  // start decoding !
  when(SPcalcCompleted === true.B) {
    val readMemWire       = Wire(Vec(N, UInt(m.W)))

    // find the index of the minimum path metric at the end of trellis
    val tmpPMMinIndex = pmRegs.zipWithIndex.map(elem => (elem._1, elem._2.U)).reduceLeft((x, y) => {
      val comp = x._1 < y._1
      (Mux(comp, x._1, y._1), Mux(comp, x._2, y._2))
    })._2

    // start traceback from here. Decoded value from the last trellis is caluclated separately from the rest.
    when(counter === (params.n*(H-1)).U){
      decodeReg(counter/(params.n.U)) := tmpPMMinIndex >> (m-1)     // grab the minimum PM
      tmpSPReg                        := tmpPMMinIndex

    // read recorded survival path from SRAM. Its MSB represents the decoded value.
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
      (0 until N).map(i => {ConvertableTo[U].fromInt(0)})
      (0 until N*H).map(i => {survivalPath.write(i.U, VecInit(Seq.fill(N){0.U}))})
    }

    // SRAM read takes 1 clk cycle.
    readMemWire := survivalPath.read(addrWire - 1.U)
  }

  when(io.headInfo.fire()){
    outValid      := false.B
  }
  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := decodeReg(i)             })
  (0 until 12).map(i  => { lengthInfoWire(i)         := decodeReg(5 + i) << (i)  })
  io.headInfo.bits.dataLen    := lengthInfoWire.reduce(_ + _) << 3  // contains number of data "octets" in a packet
  io.headInfo.valid           := outValid

}

