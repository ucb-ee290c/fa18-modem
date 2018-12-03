package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// more comments are available on traceback_backup1.scala file
// assuming continous Viterbi Decoding
class Traceback[T <: Data: Real, U <: Data: Real](params: CodingParams[T, U]) extends Module {
  require(params.D >= 4)

  val io = IO(new Bundle {
    // ignore very first PM & SP
    val inPM    = Input(Vec(params.nStates, params.pmBitType.cloneType)) // storing Path Metric
    val inSP    = Input(Vec(params.nStates, UInt(params.m.W))) // storing Survival Path
    val enable  = Input(Bool())
    val out     = Decoupled(Vec(params.D, UInt(params.k.W)))
    val headInfo = Flipped(Valid(DecodeHeadBundle()))

  })
  val L   = params.L
  val D   = params.D
  val m   = params.m

  // Memory Configuration
  val outValid    = RegInit(false.B)
  val addrSize    = (D + L) * 2
  val addrWidth   = log2Ceil(addrSize) + 1
  val wrAddrPrev  = RegInit(0.U(addrWidth.W))
  val mem         = SyncReadMem(addrSize, Vec(params.nStates, UInt(m.W)))

  // Bits are decoded every D cycles, with the exception of the first decode.
  // The first decode must wait for D + L cycles to account for the survivor path memory length.
  // Since the traceback algorithm takes ~ D + L cycles of latency but a new decoding window starts every D cycles,
  // multiple tracebacks will be occurring simultaneously.
  // Thus, multiple memory read ports are required, specified by the following constant:
  val numReadPorts = ((L - 2) / D) + 2

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

  // Default assignment
  io.out.bits.foreach(_ := 0.U(params.k.W))

  // FSM states
  val sIdle :: sWaitFirst :: sDecodeFirst :: sWaitRest :: sDecodeRest :: Nil = Enum(5)
  val state      = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  val nextStateDecode = state_next === sDecodeFirst || state_next === sDecodeRest
  val nextStateWait   = state_next === sWaitFirst   || state_next === sWaitRest

  state_next := state
  state      := state_next

  // Memory write address
  val wrAddr = Wire(wrAddrPrev.cloneType)
  wrAddrPrev := wrAddr
  wrAddr     := wrAddrPrev
  // Update write address (with wrapping)
  when (io.enable) { wrAddr := Mux(wrAddrPrev < (addrSize - 1).U, wrAddrPrev + 1.U, 0.U) }

  // Update traceback counters
  tracebackCounterPorts.foreach(cntr => { cntr := cntr - 1.U })
  when (nextStateDecode && state_next =/= state) {
    tracebackCounterPorts(selReadPortWire) := (D + L - 2).U
  }

  // Update counterD
  when (nextStateWait && state_next =/= state) {
    counterD := 0.U
  } .elsewhen (state === sWaitRest && io.enable) {
    counterD := counterD + 1.U
  }

  // FSM logic
  when (io.enable) {
    switch (state) {
      is (sIdle) {
        state_next := sWaitFirst
        wrAddr := 0.U
      }
      is (sWaitFirst) {
        when (wrAddr % (D + L).U === (D + L - 2).U) {
          state_next       := sDecodeFirst
          rdAddrOffsetBase := 0.U
          cntLenReg        := D.U
          selReadPortWire  := 0.U
          rdAddrOffsetPorts(selReadPortWire) := 0.U
        }
      }
      is (sDecodeFirst) {
        state_next := sWaitRest
      }
      is (sWaitRest) {
        when (counterD === (D - 2).U) {
          state_next           := sDecodeRest
          rdAddrOffsetBaseNext := Mux(rdAddrOffsetBase < (addrSize - D).U, rdAddrOffsetBase + D.U, rdAddrOffsetBase - (addrSize - D).U)
          cntLenReg            := cntLenReg + D.U
          selReadPortWire      := Mux(selReadPortReg === (numReadPorts - 1).U, 0.U, selReadPortReg + 1.U)
          rdAddrOffsetPorts(selReadPortWire) := rdAddrOffsetBaseNext
        }
      }
      is (sDecodeRest) {
        state_next := Mux(cntLenReg >= dataLen, sIdle, sWaitRest)
      }
    }
  }

  // setup registers for address
  when (io.enable) {
    mem.write(wrAddr, io.inSP)

    // Find the state corresponding to the smallest PM
    val tmpPMMinIndex = io.inPM.zipWithIndex.map(elem => (elem._1, elem._2.U)).reduceLeft((x, y) => {
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

    val tmpSPReg = Reg(Vec(numReadPorts, UInt(m.W)))
    val tmpSPWire = Wire(Vec(numReadPorts, UInt(m.W)))

    tmpSPReg  := tmpSPWire
    tmpSPWire := tmpSPReg

    val decodeReg  = Reg(Vec(numReadPorts, Vec(D, UInt(params.k.W))))

    for (i <- 0 until numReadPorts) {
      when (tracebackCounterPorts(i) === (D + L - 2).U) {
        tmpSPWire(i) := io.inSP(tmpPMMinIndex) // Load from input
      } .otherwise {
        tmpSPWire(i) := mem_rv(i)(tmpSPReg(i)) // Load from memory
      }
      when (tracebackCounterPorts(i) < D.U) {
        decodeReg(i)(tracebackCounterPorts(i)) := tmpSPWire(i)(m - 1)
      }
    }

    val delay = D + L - 1

    // delay read port selection to be in sync with output
    val selReadPortReg_delayed = ShiftRegister(selReadPortReg, delay, io.enable)
    io.out.bits := decodeReg(selReadPortReg_delayed)

    when (io.out.fire()) {
      outValid := false.B
    } .otherwise {
      outValid := ShiftRegister(state === sDecodeFirst || state === sDecodeRest, delay, false.B, io.enable)
    }
  }

  io.out.valid := outValid
}
