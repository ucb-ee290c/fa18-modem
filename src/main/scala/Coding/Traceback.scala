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
    val headInfo = Flipped(Valid(DecodeHeadBundle()))

  })
  val L   = params.L
  val D   = params.D
  val m   = params.m

  // Memory Configuration
  val outValid    = RegInit(false.B)
  val addrSize    = (D + L) * 2
  val addrWidth   = log2Ceil(addrSize) + 1
  val wrAddrPrev     = RegInit(0.U(addrWidth.W))
  val mem         = SyncReadMem(addrSize, Vec(params.nStates, UInt(m.W)))

  // declare variables for decoding process
  val tmpPMMinIndexReg  = RegInit(0.U(m.W))
  val tmpSPReg          = RegInit(VecInit(Seq.fill(params.nStates)(0.U(m.W))))
  val rdAddrOffset      = Reg(UInt(addrWidth.W))
  val counterD          = RegInit(0.U((log2Ceil(params.D)+1).W))      // counter for D
  val decodeWire        = Wire(Vec(D, UInt(params.k.W)))
  val allDataRecvReg    = RegInit(false.B)
  val dataLen = RegEnable(io.headInfo.bits.dataLen, io.headInfo.valid)
  val cntLenReg         = Reg(dataLen.cloneType) //RegInit(0.U(6.W))

  decodeWire.foreach(_ := 0.U(params.k.W))

  val sIdle :: sWaitFirst :: sDecodeFirst :: sWaitRest :: sDecodeRest :: Nil = Enum(5)
  val state      = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  state_next := state
  state := state_next

  val wrAddr = Wire(wrAddrPrev.cloneType)
  wrAddr := wrAddrPrev

  when (io.enable) {
    wrAddr := Mux(wrAddrPrev < (addrSize - 1).U, wrAddrPrev + 1.U, 0.U)
  }

  switch (state) {
    is (sIdle) {
      when (io.enable) {
        state_next := sWaitFirst
        wrAddr := 0.U
      }
    }
    is (sWaitFirst) {
      when (io.enable) {
        when (wrAddr % (D + L).U === (D + L - 2).U) {
          state_next := sDecodeFirst
          rdAddrOffset := 0.U
          cntLenReg := D.U
        }
      }
    }
    is (sDecodeFirst) {
      when (io.enable) {
        state_next := sWaitRest
        counterD := 0.U
      }
    }
    is (sWaitRest) {
      when (io.enable) {
        counterD := counterD + 1.U // counterD tracks number of received bits (count up to params.D)
        when (counterD === (D - 2).U) {
          state_next := sDecodeRest
          rdAddrOffset := Mux(rdAddrOffset < (addrSize - D).U, rdAddrOffset + D.U, rdAddrOffset - (addrSize - D).U)
          cntLenReg := cntLenReg + D.U
        }
      }
    }
    is (sDecodeRest) {
      when (io.enable) {
        counterD := 0.U
        state_next := Mux(cntLenReg >= dataLen, sIdle, sWaitRest)
      }
    }
  }

  wrAddrPrev := wrAddr

  // setup registers for address
  when (io.enable) {
    val tmpSP             = Wire(Vec(D+L, UInt(m.W)))
    val memWire           = Wire(Vec(D+L-1, Vec(params.nStates, UInt(m.W))))

    printf(p"wrAddr = ${wrAddr} ******* \n")
    printf(p"state = ${state} ******* \n")
    mem.write(wrAddr, io.inSP)

    when (state === sDecodeFirst || state === sDecodeRest) {
      // find minimum in PM
      val tmpPMMinIndex = io.inPM.zipWithIndex.map(elem => (elem._1, elem._2.U)).reduceLeft((x, y) => {
        val comp = x._1 < y._1
        (Mux(comp, x._1, y._1), Mux(comp, x._2, y._2))
      })._2

      // when the decoding just started, it starts decoding after it receives D+L bits
      // decodes every D bits
      tmpPMMinIndexReg := tmpPMMinIndex
      tmpSPReg         := io.inSP
    }

    // Start decoding
    /*  example: D = 5, D = traceback depth of Viterbi decoder
        rdAddrOffset + (D + L - 1)  -> data have been received 'D' times
        rdAddrOffset + (D + L)      -> 'D' data is stored in memory
        rdAddrOffset + (D + L + 1)  -> 'mem.read' is called. Fetch the data from the memory.
        rdAddrOffset + (D + L + 2)  -> data should be fetched on 'decodeReg'. raise 'valid'
        rdAddrOffset + (D + L + 3)  -> fetch data from 'decodeReg' to 'io.out.bits'
    */
    for (i <- 0 until D+L-1){
      when ((rdAddrOffset + i.U) <= (addrSize - 1).U ) {
        memWire(i) := mem.read(rdAddrOffset + i.U)
      }.otherwise {
        memWire(i) := mem.read(rdAddrOffset + i.U - addrSize.U)
      }
    }
    tmpSP(D+L-1) := tmpSPReg(tmpPMMinIndexReg)    // grab the minimum PM
    for (i <- D+L-2 to 0 by -1) {
      tmpSP(i) := memWire(i)(tmpSP(i + 1))
      if (i < D) {
        decodeWire(i) := tmpSP(i+1)(m-1) // get MSB
      }
    }

    when (io.out.fire()) {
      outValid := false.B
    } .otherwise {
      outValid := state === sDecodeFirst || state === sDecodeRest
    }
  }

  io.out.valid    := outValid
  io.out.bits     := decodeWire    // output is available 1 clk cycle after.
}
