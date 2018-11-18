package modem

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// TODO: zero-flush bufInterleaver when all the packets are properly received
class Puncturing[T <: Data](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val in          = Input(Vec(params.n, UInt(1.W)))
    val out         = Output(Vec(params.O, UInt(1.W)))
    val puncMatrix  = Input(Vec(4, UInt(1.W)))
    val inReady     = Input(UInt(1.W))
    val isHead      = Input(UInt(1.W))
    val stateIn     = Input(UInt(2.W))
    val stateOut    = Output(UInt(2.W))
  })
  val puncEn = RegInit(false.B)
  val puncMatBitWidth = RegInit(0.U(4.W))

  // puncturing matrix
  val punctureList1       = Array(Array(1, 0, 0, 0, 0, 0, 0), Array(1, 0, 0, 0, 0, 0, 0))   // rate = 1/2
  val punctureList2       = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(1, 0, 0, 0, 0, 0, 0))   // rate = 2/3
  val punctureList3       = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(1, 0, 1, 0, 0, 0, 0))   // rate = 3/4
  val punctureList4       = Array(Array(1, 1, 0, 1, 0, 0, 0), Array(1, 0, 1, 0, 1, 0, 0))   // rate = 5/6 -> not in 802.11a
  val punctureList5       = Array(Array(1, 1, 1, 1, 0, 1, 0), Array(1, 0, 0, 0, 1, 0, 1))   // rate = 7/8 -> not in 802.11a

  // puncIndices contains buffer address offset
  // ex) [1,1,0],[1,0,1] -> [1,1,0],[2,1,1] : accumulate over rows
  val puncIndices1        = Array(Array(1, 0, 0, 0, 0, 0, 0), Array(2, 0, 0, 0, 0, 0, 0))   // rate = 1/2
  val puncIndices2        = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(2, 1, 0, 0, 0, 0, 0))   // rate = 2/3
  val puncIndices3        = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(2, 1, 1, 0, 0, 0, 0))   // rate = 3/4
  val puncIndices4        = Array(Array(1, 1, 0, 1, 0, 0, 0), Array(2, 1, 1, 1, 1, 0, 0))   // rate = 5/6 -> not in 802.11a
  val puncIndices5        = Array(Array(1, 1, 1, 1, 0, 1, 0), Array(2, 1, 1, 1, 1, 1, 1))   // rate = 7/8 -> not in 802.11a

  // puncListColSum contains summation over rows
  // ex) [1,1,0], [1,0,1] -> [2,1,1]
  val puncListColSum1     = Array(1, 0, 0, 0, 0, 0, 0)  // rate = 1/2
  val puncListColSum2     = Array(2, 1, 0, 0, 0, 0, 0)  // rate = 2/3
  val puncListColSum3     = Array(2, 1, 1, 0, 0, 0, 0)  // rate = 3/4
  val puncListColSum4     = Array(2, 1, 1, 1, 1, 0, 0)  // rate = 5/6 -> not in 802.11a
  val puncListColSum5     = Array(2, 1, 1, 1, 1, 1, 1)  // rate = 7/8 -> not in 802.11a

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
  when(io.isHead === 1.U){
    when( ((io.puncMatrix(0).toBool() || io.puncMatrix(1).toBool()) && !io.puncMatrix(2).toBool() && io.puncMatrix(3).toBool()) === true.B)
    { // rate = 1/2
      puncMatBitWidth := 1.U
      puncEn := false.B
    }.elsewhen( (!io.puncMatrix(0).toBool() && !io.puncMatrix(1).toBool() && !io.puncMatrix(2).toBool() && io.puncMatrix(3).toBool()) === true.B)
    { // rate = 2/3
      puncMatBitWidth := 2.U
      puncEn := true.B
      (0 until params.n).map(i => {
        (0 until 2).map(j => {
          punctureVecReg(i)(j) := (punctureList2(i)(j)).U
          puncIndicesReg(i)(j) := puncIndices2(i)(j).U
        })
      })
      (0 until 2).map(i => { puncListColSumReg(i) := puncListColSum2(i).U })
    }.elsewhen( (io.puncMatrix(2).toBool() && io.puncMatrix(3).toBool()) === true.B)
    { // rate = 3/4
      puncMatBitWidth := 3.U
      puncEn := true.B
      (0 until params.n).map(i => {
        (0 until 3).map(j => {
          punctureVecReg(i)(j) := (punctureList3(i)(j)).U
          puncIndicesReg(i)(j) := puncIndices3(i)(j).U
        })
      })
      (0 until 3).map(i => { puncListColSumReg(i) := puncListColSum3(i).U })
    }
  }

  val o_cnt             = RegInit(0.U(log2Ceil(params.O).W))              // counter for data vector tracker
  val p_cnt             = RegInit(0.U(log2Ceil(params.O).W))              // counter for outReg tracker
  val bufInterleaver    = RegInit(VecInit(Seq.fill(params.O)(0.U(1.W))))  // buffer for interleaver

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val stateWire   = Wire(UInt(2.W))
  stateWire := io.stateIn

  // ex) puncturing Matrix: [1,1,0],[1,0,1]
  // -> Input Matrix: [A0,A1,A2], [B0, B1, B2] -> Output Matrix: [A0, B0, A1, B2]
  when(io.stateIn =/= sDone && io.inReady === 1.U && io.isHead === 0.U){
    when(puncEn === true.B) {    // if puncturing is enabled,
      for (i <- 0 until params.n) {
        when(punctureVecReg((o_cnt+i.U) % params.n.U)((o_cnt / params.n.U) % puncMatBitWidth) === 1.U) {
          bufInterleaver(p_cnt - 1.U + puncIndicesReg((o_cnt+i.U) % params.n.U)(((o_cnt+i.U) / params.n.U) % puncMatBitWidth)) := io.in(i.U)
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
    }.otherwise{                                  // no puncturing
      (0 until params.n).map(i => { bufInterleaver(o_cnt + i.U) := io.in(i.U) })
      o_cnt := o_cnt + params.n.U
      when(o_cnt === (params.O - params.n).U) {
        stateWire := sDone
      }
      when(o_cnt === (params.O - params.n).U) {
        o_cnt := 0.U
      }
    }
  }.elsewhen(io.stateIn =/= sDone && io.inReady === 1.U && io.isHead === 1.U){
    (0 until params.n).map(i => { bufInterleaver(o_cnt + i.U) := io.in(i.U) })
    o_cnt := o_cnt + params.n.U
    when(o_cnt === (params.O - params.n).U) {
      stateWire := sDone
    }
    when(o_cnt === (params.O - params.n).U) {
      o_cnt := 0.U
    }
  }

  // connect registers to output
  io.out   := bufInterleaver
  io.stateOut := stateWire
}
