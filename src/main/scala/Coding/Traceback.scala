package Coding

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

class Traceback[T <: Data](params: CodingParams[T]) extends Module {
//  require(params.D > 4)

  val io = IO(new Bundle {
    // ignore very first PM & SP
    val inPM    = Input(Vec(params.nStates, UInt(params.pmBits.W))) // storing Path Metric
    val inSP    = Input(Vec(params.nStates, UInt(params.m.W))) // storing Survival Path
    val inReady = Input(UInt(1.W))
    val out     = Decoupled(Vec(params.D, UInt(params.k.W)))
  })
  val L   = params.L
  val D   = params.D
  val m   = params.m

  // Memory Configuration
  val outValid    = RegInit(false.B)
  val addrSize    = params.nStates * (D+L)
  val addrWidth   = log2Ceil(addrSize) + 2
  val addr        = Wire(UInt(addrWidth.W))
  val enable      = Wire(Bool())
  val tmpSP       = Wire(Vec(D+L, UInt(m.W)))
  val addrReg     = RegInit(0.U(addrWidth.W))

  // setup registers for address
  addr    := addrReg
  enable  := true.B // TODO: connect this to the input of the module
  when(addrReg < (addrSize * 2).U){
    addrReg := addrReg + params.nStates.U
  }.otherwise{
    addrReg := 0.U
  }

  // Store data into memory
  // TODO: I'm not using SyncReadMem currently due to some syntax + logical errors. I may come back and try to use it later
//  val mem = SyncReadMem(addrSize * 2, UInt(m.W))
//  for (i <- 0 until params.nStates) {
//    mem.write(addrReg + i.U, io.inSP(i))
//  }
  val mem = Reg(Vec(addrSize * 2, UInt(m.W)))
  for (i <- 0 until params.nStates){
    mem(addrReg + i.U) := io.inSP(i)
  }

  // declare variables for decoding process
  val memWire           = Wire(Vec(params.nStates * (D+L-1), UInt(m.W)))
  val tmpPMMin          = Wire(Vec(params.nStates - 1, UInt(m.W)))
  val tmpPMMinIndex     = Wire(Vec(params.nStates - 1, UInt(m.W)))
  val tmpPMMinReg       = RegInit(VecInit(Seq.fill(params.nStates - 1)(0.U(m.W))))
  val tmpPMMinIndexReg  = RegInit(VecInit(Seq.fill(params.nStates - 1)(0.U(m.W))))
  val tmpSPReg          = RegInit(VecInit(Seq.fill(params.nStates)(0.U(m.W))))
  val trackValid        = RegInit(VecInit(Seq.fill(3)(0.U(1.W))))
  val addrOffset        = RegInit(0.U(addrWidth.W))
  val decodeStart       = RegInit(0.U(2.W))
  val counterD          = RegInit(0.U((log2Ceil(params.D)+1).W))      // counter for D
  val decodeReg         = Reg(Vec(D, UInt(params.k.W)))
  val memReg            = RegInit(VecInit(Seq.fill(params.nStates * (D+L-1))(0.U(m.W))))

  // find minimum in PM
  tmpPMMin(0)           := Mux(io.inPM(0) < io.inPM(1), io.inPM(0), io.inPM(1))
  tmpPMMinIndex(0)      := Mux(io.inPM(0) < io.inPM(1), 0.U, 1.U)
  for (i <- 1 until params.nStates - 1) {
    tmpPMMin(i)         := Mux(tmpPMMin(i - 1) < io.inPM(i + 1), tmpPMMin(i - 1), io.inPM(i + 1))
    tmpPMMinIndex(i)    := Mux(tmpPMMin(i - 1) < io.inPM(i + 1), tmpPMMinIndex(i - 1), (i + 1).U)
  }
  // should be clocked at nState * (D+L-1)
  when((addrReg % (params.nStates * (D+L)).U === (params.nStates * (D+L-1)).U) && (decodeStart === 0.U)) {
    tmpPMMinReg         := tmpPMMin
    tmpPMMinIndexReg    := tmpPMMinIndex
    tmpSPReg            := io.inSP
    trackValid(0)       := 1.U
    decodeStart         := 1.U
    counterD            := 0.U
    printf("************************ TEST1 **********************\n")
    printf(p"addrReg = ${addrReg} \n")
  }.elsewhen((counterD === (params.D-1).U) && (decodeStart === 1.U)){
    tmpPMMinReg         := tmpPMMin
    tmpPMMinIndexReg    := tmpPMMinIndex
    tmpSPReg            := io.inSP
    trackValid(0)       := 1.U
    counterD            := 0.U
    when(addrReg - (params.nStates * (D+L-1)).U <= ((addrSize*2) - 1).U ){
      addrOffset := addrReg - (params.nStates * (D+L-1)).U
    }.otherwise{
      addrOffset := addrReg - (params.nStates * (D+L-1)).U - (addrSize*2).U
    }
    printf("************************ TEST2 **********************\n")
    printf(p"addrReg = ${addrReg} \n")
  }.otherwise{
    counterD := counterD + 1.U
  }
  when(trackValid(0) === 1.U){
    (2 to 1 by -1).map(i => {trackValid(i) := trackValid(i-1)})
  }

  // Start decoding
  /*  example: D = 5, D = traceback depth of Viterbi decoder
      addrOffset + nState * (D + L - 1)  -> data have been received 'D' times
      addrOffset + nState * (D + L)      -> 'D' data is stored in memory
      addrOffset + nState * (D + L + 1)  -> 'mem.read' is called. Fetch the data from the memory.
      addrOffset + nState * (D + L + 2)  -> data should be fetched on 'decodeReg'. raise 'valid'
      addrOffset + nState * (D + L + 3)  -> fetch data from 'decodeReg' to 'io.out.bits'
  */
  tmpSP(D+L-1) := tmpSPReg(tmpPMMinIndexReg(params.nStates-2))    // grab the minimum PM
  for (i <- 0 until (params.nStates * (D+L-1))){
    when((addrOffset + i.U) <= ((addrSize*2) - 1).U ) {
      memWire(i) := mem(addrOffset + i.U)
    }.otherwise {
      memWire(i) := mem(addrOffset + i.U - (addrSize*2).U)
    }
  }
  for (i <- D+L-2 to 0 by -1) {
    printf(p"${(addrOffset + (params.nStates * i).U + tmpSP(i + 1))}\n")
    tmpSP(i) := memWire((params.nStates * i).U + tmpSP(i + 1))
    if(i < D) {
      decodeReg(i) := tmpSP(i+1) >> (m-1) // get MSB
    }
  }

//  when(addrReg === (params.nStates * (D+L+2)).U){
  when(trackValid(2) === 1.U) {
    outValid      := true.B
    (0 to 2).map(i => {trackValid(i)  := 0.U})
  }
  when(io.out.fire()){
    outValid      := false.B
  }
  io.out.valid    := outValid
  io.out.bits     := decodeReg    // output is available 3 clk cycles after.
}

//  ****** Below is appendix for later use ******
//  TODO: Following is what Paul suggested me to try but currently not working. I need to figure out correct syntax
//  val readEn  = addrReg === (params.nStates * (D+1)).U
//  val regEn   = addrReg === (params.nStates * (D+2)).U
//  tmpSP(D-1)  := tmpSPReg(tmpPMMinIndexReg(params.nStates-2))
//  when (regEn) {
//    decodeReg(D-1)  := tmpPMMinIndexReg(params.nStates-2) >> (m-1)
//  }
//  for (i <- D-2 to 0 by -1) {
//    tmpSP(i)        := mem.read((params.nStates * i).U + tmpSP(i + 1), readEn)
//    when (regEn) {
//      decodeReg(i)  := tmpSP(i+1) >> (m-1) // get MSB
//    }
//  }

//  TODO: memread not working properly -> problem was that values read from memory are not cosistent
//  tmpSP(D+L-1) := tmpSPReg(tmpPMMinIndexReg(params.nStates-2))    // grab the minimum PM
//  for (i <- 0 until (params.nStates * (D+L-1))){
//    memReg(i) := mem(addrOffset + i.U)
//  }
//  for (i <- D+L-2 to 0 by -1) {
//    printf(p"${(addrOffset + (params.nStates * i).U + tmpSP(i + 1))}\n")
//    when((addrOffset + (params.nStates * i).U + tmpSP(i + 1)) <= ((addrSize*2) - 1).U ){
//      tmpSP(i) := memReg(addrOffset + (params.nStates * i).U + tmpSP(i + 1))
//      printf("************************ TEST3 **********************\n")
//    }.otherwise{
//      tmpSP(i) := memReg(addrOffset + (params.nStates * i).U + tmpSP(i + 1) - (addrSize*2).U)
//      printf("************************ TEST4 **********************\n")
//    }
//  }
