package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: de-puncturing block for Viterbi decoder
class DePuncturing[T <: Data: Real](params: CodingParams[T]) extends Module {
//  require((params.n * params.H) <= params.n * params.H)
  val io = IO(new Bundle{

    val hdrPktLatch = Output(Bool())

    val in          = Flipped(Decoupled(BitsBundle(params)))      // from Demod
    val in_hard     = Input(Vec(params.n * params.H, SInt(2.W)))  // from Demod
    val isHead      = Input(Bool())                               // from arbiter
    val hdrEnd      = Input(Bool())                               // from arbiter
    val headInfo    = Flipped(Decoupled(DecodeHeadBundle()))      // from Head Extractor

    val outData     = Output(Vec(params.n, SInt(2.W)))            // to PathMetric
    val outHead     = Output(Vec(params.n * params.H, SInt(2.W))) // to HeadExtractor
    val lenCnt      = Output(Bool())                              // to arbiter
    val outEnable   = Output(Bool())                              // to PathMetric
  })
  val puncMatBitWidth     = RegInit(0.U(4.W))
  val punctureVecReg      = RegInit(VecInit(Seq.fill(params.n)(VecInit(Seq.fill(7)(0.U(1.W))))))  // support up to 7/8 coding rate
  val puncIndicesReg      = RegInit(VecInit(Seq.fill(params.n)(VecInit(Seq.fill(7)(0.U((log2Ceil(params.n * params.H)+1).W))))))
  val puncListColSumReg   = RegInit(VecInit(Seq.fill(7)(0.U((log2Ceil(params.n+1)).W))))
  val headInfoReady       = RegInit(true.B)
  /*
    R1-R4 | Rate (Mb/s) | Puncturing Matrix
    1101  | 6           | 1/2
    1111  | 9           | 3/4
    0101  | 12          | 1/2
    0111  | 18          | 3/4
    1001  | 24          | 1/2
    1011  | 36          | 3/4
    0001  | 48          | 2/3
    0011  | 54          | 3/4

    1/2 -> 0.U  ->  nand(R1,R2) && !R3 && R4
    2/3 -> 1.U  ->  !R1 && !R2 && !R3 && R4
    3/4 -> 2.U  ->  R3 && R4
   */
  // re-configure registers for puncturing
  when(io.headInfo.fire() === true.B){
    when( ((io.headInfo.bits.rate(0).toBool() || io.headInfo.bits.rate(1).toBool()) && !io.headInfo.bits.rate(2).toBool() && io.headInfo.bits.rate(3).toBool()) === true.B)
    { // rate = 1/2
      puncMatBitWidth := 1.U
      (0 until params.n).map(i => {
        (0 until 2).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList1(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices1(i)(j).U
        })
      })
      (0 until 2).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum1(i).U })
    }.elsewhen( (!io.headInfo.bits.rate(0).toBool() && !io.headInfo.bits.rate(1).toBool() && !io.headInfo.bits.rate(2).toBool() && io.headInfo.bits.rate(3).toBool()) === true.B)
    { // rate = 2/3
      puncMatBitWidth := 2.U
      (0 until params.n).map(i => {
        (0 until 2).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList2(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices2(i)(j).U
        })
      })
      (0 until 2).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum2(i).U })
    }.elsewhen( (io.headInfo.bits.rate(2).toBool() && io.headInfo.bits.rate(3).toBool()) === true.B)
    { // rate = 3/4
      puncMatBitWidth := 3.U
      (0 until params.n).map(i => {
        (0 until 3).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList3(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices3(i)(j).U
        })
      })
      (0 until 3).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum3(i).U })
    }.otherwise
    { // rate = 1/2
      printf("ajklasdjfkla;sjkflasjl;faadfjklasjfklas;klf l \n ")
      puncMatBitWidth := 1.U
      (0 until params.n).map(i => {
        (0 until 2).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList1(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices1(i)(j).U
        })
      })
      (0 until 2).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum1(i).U })
    }
    headInfoReady := false.B
  }

  val o_cnt         = RegInit(0.U(log2Ceil(params.n * params.H).W))              // counter for data vector tracker
  val p_cnt         = RegInit(0.U(log2Ceil(params.n * params.H).W))              // counter for outReg tracker
  val h_cnt         = RegInit(0.U(6.W))
  val bufData       = RegInit(VecInit(Seq.fill(params.n)(0.S(2.W))))  // buffer for Data
  val pktLatch      = RegInit(false.B)
  val pktCntReg     = RegInit(0.U(15.W))
  val lenCntReg     = RegInit(true.B)
  val bitCntReg     = RegInit((params.n * params.H).U(log2Ceil(params.n * params.H).W))
  val enReg         = RegInit(false.B)

  val inReg       = Reg(Vec(params.n * params.H, SInt(2.W)))
  when(io.in.fire()){
    inReg := io.in_hard
    when((io.in.bits.pktStart === true.B) && (pktLatch === false.B)){
      pktLatch := true.B
    }
    bitCntReg   := 0.U
  }
  when( (io.in.bits.pktEnd === true.B) && (pktLatch === true.B)){             // lower pktLatch
    pktLatch := false.B
  }
  when(io.isHead === true.B){
    pktCntReg := 0.U
  }
  when(io.isHead === true.B || pktCntReg >= io.headInfo.bits.dataLen || bitCntReg >= (params.n * params.H).U) {
    enReg := false.B
  }

  when(pktLatch === true.B && bitCntReg < (params.n * params.H).U){
    // hdrEnd triggers one cycle before the end of header block
    // -> needs to reset o_cnt and p_cnt registers.
    // -> below may not be necessary if params.n * params.H is set to 96 or higher.
    // -> but I don't see any reason not to have params.n * params.H fixed at 48 bits.
    when(io.hdrEnd === true.B) {
      o_cnt     := 0.U
      p_cnt     := 0.U
      enReg     := false.B
      lenCntReg := false.B
      bitCntReg := (params.n * params.H).U
    // when it starts receiving payload
    // need to count number of bits it has received.
    // Once all the data has been received, raise 'lenCntReg' and 'headInfoReady' registers
    }.elsewhen(io.isHead === false.B){
      // puncturing Matrix: [1,1,0],[1,0,1]
      // Input Matrix: [A0,A1,A2], [B0, B1, B2] -> Output Matrix: [A0, B0, A1, B2]
      for (i <- 0 until params.n) {
        when(punctureVecReg((o_cnt+i.U) % params.n.U)((o_cnt / params.n.U) % puncMatBitWidth) === 1.U) {
          bufData(i.U) := inReg(p_cnt - 1.U + puncIndicesReg((o_cnt+i.U) % params.n.U)(((o_cnt+i.U) / params.n.U) % puncMatBitWidth))
          // o_cnt + i.U / params.n.U % puncMatBitWidth.U must reach to puncMatBitWidth.U-1
        }.otherwise{          // add dummy bits
          bufData(i.U) := 0.S
        }
      }
      enReg := true.B
      p_cnt := p_cnt + puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth)
      o_cnt := o_cnt + params.n.U
      when(p_cnt >= (params.n * params.H.U - puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth))) {
        p_cnt := 0.U
      }
      when((o_cnt >= (params.n * params.H - params.n).U) && ((((o_cnt+1.U) / params.n.U) % puncMatBitWidth) === (puncMatBitWidth -1.U))) {
        o_cnt := 0.U
      }

      when(pktCntReg < io.headInfo.bits.dataLen){
        pktCntReg := pktCntReg + puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth)  // count # of processed bits
      }
      bitCntReg := bitCntReg + puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth)    // count # of processed bits
    }
  }
  when(pktCntReg >= io.headInfo.bits.dataLen){                // length counter.
    lenCntReg := true.B
    headInfoReady := true.B
  }

  printf(p"bitCntReg = ${bitCntReg} *********\n")

  // connect registers to output
  io.in.ready       := bitCntReg >= (params.n * params.H).U
  io.outData        := bufData
  io.lenCnt         := lenCntReg
  io.headInfo.ready := headInfoReady
  io.outHead        := inReg
  io.hdrPktLatch    := pktLatch
  io.outEnable      := enReg
}
