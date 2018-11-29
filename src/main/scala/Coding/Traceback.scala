package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// more comments are available on traceback_backup1.scala file
// assuming continous Viterbi Decoding
class Traceback[T <: Data: Real](params: CodingParams[T]) extends Module {
  require(params.D >= 4)

  val io = IO(new Bundle {
    // ignore very first PM & SP
    val inPM    = Input(Vec(params.nStates, UInt(params.pmBits.W))) // storing Path Metric
    val inSP    = Input(Vec(params.nStates, UInt(params.m.W))) // storing Survival Path
    val enable  = Input(Bool())
    val out     = Decoupled(Vec(params.D, UInt(params.k.W)))

  })
  val L   = params.L
  val D   = params.D
  val m   = params.m

  // Memory Configuration
  val outValid    = RegInit(false.B)
  val addrSize    = params.nStates * (D+L)
  val addrWidth   = log2Ceil(addrSize) + 2
  val addrReg     = RegInit(0.U(addrWidth.W))
  val mem         = SyncReadMem(addrSize * 2, UInt(m.W))

  // declare variables for decoding process
  val tmpPMMinReg       = RegInit(VecInit(Seq.fill(params.nStates - 1)(0.U(m.W))))
  val tmpPMMinIndexReg  = RegInit(VecInit(Seq.fill(params.nStates - 1)(0.U(m.W))))
  val tmpSPReg          = RegInit(VecInit(Seq.fill(params.nStates)(0.U(m.W))))
  val trackValid        = RegInit(VecInit(Seq.fill(3)(0.U(1.W))))
  val addrOffset        = RegInit(0.U(addrWidth.W))
  val decodeStart       = RegInit(0.U(2.W))
  val counterD          = RegInit(0.U((log2Ceil(params.D)+1).W))      // counter for D
  val decodeWire        = Wire(Vec(D, UInt(params.k.W)))
  val memReg            = RegInit(VecInit(Seq.fill(params.nStates * (D+L-1))(0.U(m.W))))
  val cntLenReg         = RegInit(0.U(6.W))
  val allDataRecvReg    = RegInit(0.U(1.W))

  decodeWire.foreach(_ := 0.U(params.k.W))
  // setup registers for address
  when(io.enable === true.B){
    val addr              = Wire(UInt(addrWidth.W))
    val tmpSP             = Wire(Vec(D+L, UInt(m.W)))
    val memWire           = Wire(Vec(params.nStates * (D+L-1), UInt(m.W)))
    val tmpPMMin          = Wire(Vec(params.nStates - 1, UInt(m.W)))
    val tmpPMMinIndex     = Wire(Vec(params.nStates - 1, UInt(m.W)))
    addr    := addrReg

    when(addrReg < (addrSize * 2 - params.nStates).U ){
      addrReg := addrReg + params.nStates.U
    }.otherwise{
      addrReg := 0.U
    }
    printf(p"addrReg = ${addrReg} ******* \n")
    // TODO: currently using register file but later I will come back and try to use SyncReadMem instead
    for (i <- 0 until params.nStates){
      mem.write(addrReg + i.U, io.inSP(i))
    }

    // find minimum in PM
    tmpPMMin(0)           := Mux(io.inPM(0) < io.inPM(1), io.inPM(0), io.inPM(1))
    tmpPMMinIndex(0)      := Mux(io.inPM(0) < io.inPM(1), 0.U, 1.U)
    for (i <- 1 until params.nStates - 1) {
      tmpPMMin(i)         := Mux(tmpPMMin(i - 1) < io.inPM(i + 1), tmpPMMin(i - 1), io.inPM(i + 1))
      tmpPMMinIndex(i)    := Mux(tmpPMMin(i - 1) < io.inPM(i + 1), tmpPMMinIndex(i - 1), (i + 1).U)
    }
    cntLenReg := cntLenReg + 1.U

    // when the decoding just started, it starts decoding after it receives D+L bits
    when((addrReg % (params.nStates * (D+L)).U === (params.nStates * (D+L-1)).U) && (decodeStart === 0.U)) {
      tmpPMMinReg         := tmpPMMin
      tmpPMMinIndexReg    := tmpPMMinIndex
      tmpSPReg            := io.inSP
      trackValid(0)       := 1.U                // trackValid is used to raise/lower io.out.valid signal
      decodeStart         := 1.U                // decodeStart indicates whether this is the first time decoding
      counterD            := 0.U                // counterD tracks number of received bits (count up to params.D)
    }.elsewhen((counterD === (params.D-1).U) && (decodeStart === 1.U)){   // decodes every D bits
      tmpPMMinReg         := tmpPMMin
      tmpPMMinIndexReg    := tmpPMMinIndex
      tmpSPReg            := io.inSP
      trackValid(0)       := 1.U
      counterD            := 0.U
      addrOffset := addrOffset + (params.nStates * D).U
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
        memWire(i) := mem.read(addrOffset + i.U)
      }.otherwise {
        memWire(i) := mem.read(addrOffset + i.U - (addrSize*2).U)
      }
    }
    for (i <- D+L-2 to 0 by -1) {
      tmpSP(i) := memWire((params.nStates * i).U + tmpSP(i + 1))
      if(i < D) {
        decodeWire(i) := tmpSP(i+1) >> (m-1) // get MSB
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
  }

  io.out.valid    := outValid
  io.out.bits     := decodeWire    // output is available 3 clk cycles after.
}
