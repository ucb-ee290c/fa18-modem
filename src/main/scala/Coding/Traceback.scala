package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// assuming continous Viterbi Decoding
class Traceback[T <: Data: Real, U <: Data: Real](params: CodingParams[T, U]) extends Module {
  require(params.D >= 4)

  val io = IO(new Bundle {
    // ignore very first PM & SP
    val inPM    = Input(Vec(params.nStates, params.pmBitType.cloneType)) // storing Path Metric
    val inSP    = Input(Vec(params.nStates, UInt(params.m.W))) // storing Survival Path
    val enable  = Input(Bool())     // write enable
    val out     = Decoupled(Vec(params.D, UInt(params.k.W)))
    val headInfo = Flipped(Valid(DecodeHeadBundle()))

  })
  val L   = params.L
  val D   = params.D
  val m   = params.m
  val delay = D + L - 1

  // Memory Configuration
  val outValid    = RegInit(false.B)
  val addrSize    = (D + L) * 2
  val addrWidth   = log2Ceil(addrSize) + 1
  val wrAddrPrev  = RegInit(0.U(addrWidth.W))
  val mem         = SyncReadMem(addrSize, Vec(params.nStates, UInt(m.W)))
  val enReg       = RegNext(io.enable)
  val lastPMReg   = Reg(UInt(params.nStates.W))
  val lastSPReg   = Reg(Vec(params.nStates, UInt(params.m.W)))

  // Bits are decoded every D cycles, with the exception of the first decode.
  // The first decode must wait for D + L cycles to account for the survivor path memory length.
  // Since the traceback algorithm takes ~ D + L cycles of latency but a new decoding window starts every D cycles,
  // multiple tracebacks will be occurring simultaneously.
  // Thus, multiple memory read ports are required, specified by the following constant:
  def roundUp(d: Double) = math.ceil(d).toInt
  val numReadPorts = roundUp(L/D.toDouble) + 1

  // The address for each read port is calculated through two registers:
  // rdAddrOffsetPorts(i):
  //    a counter that increments D every D cycles (aligned to each decoding window)
  // tracebackCounterPorts(i):
  //    a counter that decrements from D + L - 2 to 0 (representing the latency of each traceback)
  val rdAddrOffsetPorts     = Reg(Vec(numReadPorts, UInt(addrWidth.W)))
  val tracebackCounterPorts = Reg(Vec(numReadPorts, UInt(log2Ceil(D + L).W)))

  // This is the general offset for the read address. The specific offsets for each read port are based on this value.
  val rdAddrOffsetBase     = Reg(UInt(addrWidth.W))
  val rdAddrOffsetBaseNext = Wire(UInt(addrWidth.W))
  rdAddrOffsetBaseNext := rdAddrOffsetBase
  rdAddrOffsetBase     := rdAddrOffsetBaseNext

  // This register selects which read port output contains information for the current/most recently decoded sliding window.
  val selReadPortReg  = RegInit(0.U(log2Up(numReadPorts).W))
  val selReadPortWire = Wire(selReadPortReg.cloneType)
  selReadPortReg  := selReadPortWire
  selReadPortWire := selReadPortReg

  val counterD  = Reg(UInt(log2Up(params.D).W))                          // Counts every D cycles
  val dataLen   = RegEnable(io.headInfo.bits.dataLen, io.headInfo.valid) // Records data length
  val cntLenReg = Reg(dataLen.cloneType)                                 // Counts the number of bits decoded
  val cntLenReg2 = Reg(dataLen.cloneType)                                // Counter for lastBitDecode

  // Default assignment
  io.out.bits.foreach(_ := 0.U(params.k.W))

  // FSM states
  val sIdle :: sWaitFirst :: sDecodeFirst :: sWaitRest :: sDecodeRest :: sDecodeLast :: sAbyss :: Nil = Enum(7)
  val state     = RegInit(sIdle)
  val nextState = Wire(state.cloneType)

  val nextStateDecode = nextState === sDecodeFirst || nextState === sDecodeRest
  val nextStateWait   = nextState === sWaitFirst   || nextState === sWaitRest

  nextState := state
  state     := nextState

  // Memory write address
  val wrAddr = Wire(wrAddrPrev.cloneType)
  wrAddrPrev := wrAddr
  wrAddr     := wrAddrPrev

  // storing address for the last block decoding. This is essential since the normal decoding process will be stopped after io.enable is lowered
  val lastValDecodeReg  = RegInit(0.U(addrWidth.W))
  val lastValDecodeWire = Wire(lastValDecodeReg.cloneType)
  lastValDecodeReg  := lastValDecodeWire
  lastValDecodeWire := lastValDecodeReg

  // Update write address (with wrapping) and write to memory
  when (io.enable) {
    lastSPReg := io.inSP
    mem.write(wrAddr, io.inSP)
    wrAddr := Mux(wrAddrPrev < (addrSize - 1).U, wrAddrPrev + 1.U, 0.U)
    lastValDecodeWire := wrAddr - 1.U
  }

  // Update traceback counters
  tracebackCounterPorts.foreach(cntr => { cntr := cntr - 1.U })   // decrease by 1 for each element in a vector
  when (nextStateDecode && nextState =/= state) {
    tracebackCounterPorts(selReadPortWire) := (D + L - 2).U
  }

  // Update counterD
  when (nextStateWait && nextState =/= state) {
    counterD := 0.U
  } //.elsewhen (state === sWaitRest && io.enable) {
    .elsewhen (state === sWaitRest) {
    counterD := counterD + 1.U
  }

  // record decoded data
  val decodeReg  = Reg(Vec(numReadPorts, Vec(D, UInt(params.k.W))))

  // we need another set of register that stores last L+D decoded outputs.
  // Once io.enable (writeEnable) is lowered, check if cntLenReg is still lower than dataLen.
  // If so, then it means we still have more data to be decoded left in the memory. we need to decode this !
  // this process should be working in parallel with above state machine
  // maximum last bit = L + D
  // minimum last bit = L + 1
  val lastDecode  = RegInit(VecInit(Seq.fill(L+D)(0.U(params.k.W))))  // register storing the last L+D decoded values
  val lastBit     = ((dataLen - (L + D + 1).U) % D.U) + L.U + 1.U     // size of the last block to be decoded
  val tbCount     = RegInit(0.U(log2Ceil(L+D).W))
  val restOutVal  = RegInit(false.B)
  val outValidReg = RegInit(false.B)
  val abyssTrack  = RegInit(0.U(log2Ceil(L+D+D).W))
  // FSM logic
  switch (state) {
    is (sIdle) {
      when(io.enable){
        nextState := sWaitFirst
      }.otherwise{
        nextState := sIdle
      }
      wrAddr := 0.U
      outValidReg := false.B
      restOutVal  := false.B
      abyssTrack  := 0.U
    }
    is (sWaitFirst) {
      when (wrAddr % (D + L).U === (D + L - 2).U) {
        nextState        := sDecodeFirst
        rdAddrOffsetBase := 0.U
        selReadPortWire  := 0.U
        cntLenReg        := D.U
        cntLenReg2       := 0.U
        rdAddrOffsetPorts(selReadPortWire) := 0.U
        lastDecode.foreach(_ := 0.U(params.k.W))
      }
    }
    is (sDecodeFirst) {
      nextState := sWaitRest
    }
    is (sWaitRest) {
      when(io.enable){
        when (counterD === (D - 2).U) {
          nextState            := sDecodeRest
          rdAddrOffsetBaseNext := Mux(rdAddrOffsetBase < (addrSize - D).U, rdAddrOffsetBase + D.U, rdAddrOffsetBase - (addrSize - D).U)
          cntLenReg            := cntLenReg + D.U
          selReadPortWire      := Mux(selReadPortReg === (numReadPorts - 1).U, 0.U, selReadPortReg + 1.U)
          rdAddrOffsetPorts(selReadPortWire) := rdAddrOffsetBaseNext
        }
      }.otherwise {
        nextState := sDecodeLast
        tbCount := lastBit - 3.U
      }
    }
    is (sDecodeRest) {
      nextState := Mux(cntLenReg >= dataLen, sIdle, sWaitRest)
    }
    is (sDecodeLast) {
      when(cntLenReg2 >= lastBit){
        restOutVal := true.B
      }
      nextState := Mux(restOutVal === true.B && outValidReg === true.B , sAbyss, sDecodeLast)
    }
    is (sAbyss){  // can't come up with a better name :( - this state is added to process the last block decoding !
      when(lastBit > D.U){
        when(lastBit - abyssTrack > D.U){
          for (i <- 0 until D){
            decodeReg(selReadPortReg)(i) := lastDecode(abyssTrack + i.U)
          }
        }.otherwise{
          for (i <- 0 until D){
            when(i.U < lastBit - abyssTrack){
              decodeReg(selReadPortReg)(i) := lastDecode(abyssTrack + i.U)
            }.otherwise{
              decodeReg(selReadPortReg)(i) := 0.U
            }
          }
        }
        abyssTrack := abyssTrack + D.U
        nextState := Mux( abyssTrack >= lastBit , sIdle, sAbyss)
      }.otherwise{
        for (i <- 0 until D){
          when(i.U <= D.U - lastBit){
            decodeReg(selReadPortReg)(i) := lastDecode(i)
          }.otherwise{
            decodeReg(selReadPortReg)(i) := 0.U
          }
        }
        nextState := sIdle
      }
      outValid := true.B
    }
  }

  // Find the state corresponding to the smallest PM
  val newPMMinIndex = io.inPM.zipWithIndex.map(elem => (elem._1, elem._2.U)).reduceLeft((x, y) => {
    val comp = x._1 < y._1
    (Mux(comp, x._1, y._1), Mux(comp, x._2, y._2))
  })._2

  // Read from memory
  val mem_rv = Wire(Vec(numReadPorts, Vec(params.nStates, UInt(m.W))))
  mem_rv.zipWithIndex.foreach {
    case (wire, idx) => {
      val rdAddrUnwrapped = rdAddrOffsetPorts(idx) +& tracebackCounterPorts(idx)
      val rdAddr = Mux(rdAddrUnwrapped <= (addrSize - 1).U, rdAddrUnwrapped, rdAddrUnwrapped - addrSize.U)
      wire := mem.read(rdAddr)
    }
  }

  val tmpSPReg  = Reg(Vec(numReadPorts, UInt(m.W)))
  val tmpSPWire = Wire(Vec(numReadPorts, UInt(m.W)))

  tmpSPReg  := tmpSPWire
  tmpSPWire := tmpSPReg

  for (i <- 0 until numReadPorts) {
    when (tracebackCounterPorts(i) === (D + L - 2).U) {
      tmpSPWire(i) := io.inSP(newPMMinIndex) // Load from input
    } .otherwise {
      tmpSPWire(i) := mem_rv(i)(tmpSPReg(i)) // Load from memory
    }
    when (tracebackCounterPorts(i) < D.U) {
      decodeReg(i)(tracebackCounterPorts(i)) := tmpSPWire(i)(m - 1)   // tmpSPWire is 1D array. using 2D array accessing style means bit-level access. m-1 means MSB in this case.
    }
  }

  // decoding last block
  lastPMReg := newPMMinIndex
  val lastDecodeWire = Wire(Vec(params.nStates, UInt(m.W)))
  val indexTrackReg  = Reg(UInt(m.W))
  when(restOutVal === false.B){
    when(enReg =/= io.enable && state >= sWaitRest){    // when io.enable is switching from high to low
      lastDecode((lastBit-1.U)) := lastPMReg >> (m-1)
      lastDecode((lastBit-2.U)) := lastSPReg(newPMMinIndex) >> (m-1)
      indexTrackReg             := lastSPReg(newPMMinIndex)
      cntLenReg2                := cntLenReg2 + 2.U
    }.elsewhen(state === sDecodeLast && nextState === sDecodeLast){
      lastDecode(tbCount)       := lastDecodeWire(indexTrackReg) >> (m-1)
      indexTrackReg             := lastDecodeWire(indexTrackReg)
      tbCount                   := tbCount - 1.U
      lastValDecodeWire         := Mux(lastValDecodeReg > 0.U, lastValDecodeReg - 1.U, lastValDecodeReg + addrSize - 1.U)
      cntLenReg2                := cntLenReg2 + 1.U
    }
  }
  lastDecodeWire := mem.read(lastValDecodeWire)

  // delay read port selection to be in sync with output
  val selReadPortReg_delayed = ShiftRegister(selReadPortReg, delay, true.B)
  io.out.bits := decodeReg(selReadPortReg_delayed)

  when (io.out.fire()) {
    outValid := false.B
  } .elsewhen(state < sAbyss) {
    outValid := ShiftRegister(state === sDecodeFirst || state === sDecodeRest, delay, true.B)
  }
  when( (dataLen - cntLenReg < D.U) && outValid === true.B ){
    outValidReg := true.B
  }

  io.out.valid      := outValid
}

//printf(p" last PM Reg = ${lastPMReg} ********** ########## \n ")
//printf(p" last SP Reg0 = ${lastSPReg(0)}  ********** ########## \n")
//printf(p" last SP Reg1 = ${lastSPReg(1)}  ********** ########## \n")
//printf(p" last SP Reg2 = ${lastSPReg(2)}  ********** ########## \n")
//printf(p" last SP Reg3 = ${lastSPReg(3)}  ********** ########## \n")
//
//printf(p"tbCount = ${tbCount} ^&^&^&^&^&^&^&^ \n")
//printf(p"lastValDecodeWire = ${lastValDecodeWire} ^&^&^&^&^&^&^&^ \n")
//printf(p"read !!!! ${lastDecodeWire} \n")
//
//printf(p" wrAddrPrev = ${wrAddrPrev} !!! \n")
//printf(p" wrAddr     = ${wrAddr} !!! \n")
//printf(p" lastValDecodeWire = ${lastValDecodeWire} !!! \n")
//printf(p"lastBit    = ${lastBit}    ****** \n ")
//printf(p"cntLenReg  = ${cntLenReg}  ****** \n ")
//printf(p"cntLenReg2 = ${cntLenReg2}  ****** \n ")
//printf(p"dataLen    = ${dataLen}    ****** \n ")
//printf(p"wrAddr = ${wrAddr} ************** \n")
//printf(p"lastDecode = ${lastDecode} ****** \n")
//printf(p"mem.read(lastValDecodeWire) = ${lastDecodeWire} ****()()()*** \n ")
//printf(p"mem.read(7) = ${mem.read(7.U)} ****()()()*** \n ")
//printf(p"mem.read(8) = ${mem.read(8.U)} ****()()()*** \n ")
//printf(p"mem.read(9) = ${mem.read(9.U)} ****()()()*** \n ")
//printf(p"io.inSP = ${io.inSP} ****()()()*** \n ")