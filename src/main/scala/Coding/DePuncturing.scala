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
  val io = IO(new Bundle{
    val in_hard   = Input(Vec(params.O, SInt(2.W)))
    //    val in_soft   = Input(Vec(params.O, DspComplex(FixedPoint(2.W, 30.BP))))
    val outData   = Output(Vec(params.n, SInt(2.W)))
    //    val out_soft  = Output(Vec(params.n, DspComplex(FixedPoint(2.W, 30.BP))))

    val isHead    = Input(Bool())
    val inReady   = Input(UInt(1.W))

    val stateIn   = Input(UInt(2.W))
    val stateOut  = Output(UInt(2.W))

    val headInfo  = Flipped(Decoupled(DecodeHeadBundle()))

    val pktStart  = Input(Bool())
    val pktEnd    = Input(Bool())
    val hdrEnd    = Input(Bool())
  })
  val H                   = 2 * params.H                        // header information is encoded with rate of 1/2
  io.headInfo.ready       := true.B

  val puncMatBitWidth     = RegInit(0.U(4.W))
  val punctureVecReg      = RegInit(VecInit(Seq.fill(params.n)(VecInit(Seq.fill(7)(0.U(1.W))))))  // support up to 7/8 coding rate
  val puncIndicesReg      = RegInit(VecInit(Seq.fill(params.n)(VecInit(Seq.fill(7)(0.U((log2Ceil(params.O)+1).W))))))
  val puncListColSumReg   = RegInit(VecInit(Seq.fill(7)(0.U((log2Ceil(params.n+1)).W))))
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
  }.otherwise{
    puncMatBitWidth := 1.U
    (0 until params.n).map(i => {
      (0 until 2).map(j => {
        punctureVecReg(i)(j) := (CodingVariables.punctureList1(i)(j)).U
        puncIndicesReg(i)(j) := CodingVariables.puncIndices1(i)(j).U
      })
    })
    (0 until 2).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum1(i).U })
  }

  val o_cnt             = RegInit(0.U(log2Ceil(params.O).W))              // counter for data vector tracker
  val p_cnt             = RegInit(0.U(log2Ceil(params.O).W))              // counter for outReg tracker
  val h_cnt             = RegInit(0.U(6.W))
  val bufData           = RegInit(VecInit(Seq.fill(params.n)(0.S(2.W))))  // buffer for interleaver
  val bufHeader         = RegInit(VecInit(Seq.fill(params.n)(0.S(2.W))))  // buffer for interleaver
  val pktLatch          = RegInit(false.B)
  val pktCnt            = RegInit(0.U(12.W))

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val stateWire   = Wire(UInt(2.W))
  stateWire       := 0.U

  when((io.pktStart === true.B) && (pktLatch === false.B)){
    pktLatch := true.B
  }.elsewhen( (io.pktEnd === true.B) && (pktLatch === true.B)){
    pktLatch := false.B
  }
  when((io.pktStart || pktLatch) === true.B){
    when(io.isHead === true.B){
      (0 until params.n).map(i => { bufHeader(i.U) := io.in_hard(o_cnt + i.U) })
      o_cnt := o_cnt + params.n.U
      when(o_cnt === (params.O - params.n).U) {
        stateWire := sDone
      }
      when(o_cnt >= (params.O - params.n).U){
        o_cnt := 0.U
      }

    }.elsewhen(io.hdrEnd === true.B) {
      o_cnt := 0.U
      p_cnt := 0.U
      (0 until params.n).map(i => {
        bufHeader(i.U) := 0.S
      })
    }.elsewhen(io.isHead === false.B){
      //TODO: add counter until it reaches to pktEnd

      // puncturing Matrix: [1,1,0],[1,0,1]
      // Input Matrix: [A0,A1,A2], [B0, B1, B2] -> Output Matrix: [A0, B0, A1, B2]
      for (i <- 0 until params.n) {
        when(punctureVecReg((o_cnt+i.U) % params.n.U)((o_cnt / params.n.U) % puncMatBitWidth) === 1.U) {
          bufData(i.U) := io.in_hard(p_cnt - 1.U + puncIndicesReg((o_cnt+i.U) % params.n.U)(((o_cnt+i.U) / params.n.U) % puncMatBitWidth))
          // o_cnt + i.U / params.n.U % puncMatBitWidth.U must reach to puncMatBitWidth.U-1
        }.otherwise{          // add dummy bits
          bufData(i.U) := 0.S
        }
      }
      p_cnt := p_cnt + puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth)
      o_cnt := o_cnt + params.n.U
      when(p_cnt >= (params.O.U - puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth))) {
        stateWire := sDone
        p_cnt := 0.U
      }
      when((o_cnt >= (params.O - params.n).U) && ((((o_cnt+1.U) / params.n.U) % puncMatBitWidth) === (puncMatBitWidth -1.U))) {
        o_cnt := 0.U
      }
    }
  }

  // connect registers to output
  io.outData := bufData
  io.stateOut := stateWire
}
