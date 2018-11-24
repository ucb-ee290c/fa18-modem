package modem

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// TODO: zero-flush bufInterleaver when all the packets are properly received -> assuming this will be done by MAC layer
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

  val puncMatBitWidth = RegInit(0.U(4.W))
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
      puncMatBitWidth := CodingVariables.puncMatBitWidth1.U
      (0 until params.n).map(i => {
        (0 until 2).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList1(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices1(i)(j).U
        })
      })
      (0 until 2).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum1(i).U })
    }.elsewhen( (!io.puncMatrix(0).toBool() && !io.puncMatrix(1).toBool() && !io.puncMatrix(2).toBool() && io.puncMatrix(3).toBool()) === true.B)
    { // rate = 2/3
      puncMatBitWidth := CodingVariables.puncMatBitWidth2.U
      (0 until params.n).map(i => {
        (0 until 2).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList2(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices2(i)(j).U
        })
      })
      (0 until 2).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum2(i).U })
    }.elsewhen( (io.puncMatrix(2).toBool() && io.puncMatrix(3).toBool()) === true.B)
    { // rate = 3/4
      puncMatBitWidth := CodingVariables.puncMatBitWidth3.U
      (0 until params.n).map(i => {
        (0 until 3).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList3(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices3(i)(j).U
        })
      })
      (0 until 3).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum3(i).U })
    }.otherwise{
      // rate = 1/2 for the rest of rate code
      puncMatBitWidth := CodingVariables.puncMatBitWidth1.U
      (0 until params.n).map(i => {
        (0 until 2).map(j => {
          punctureVecReg(i)(j) := (CodingVariables.punctureList1(i)(j)).U
          puncIndicesReg(i)(j) := CodingVariables.puncIndices1(i)(j).U
        })
      })
      (0 until 2).map(i => { puncListColSumReg(i) := CodingVariables.puncListColSum1(i).U })
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
    // if puncturing is enabled,
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
  }.elsewhen(io.stateIn =/= sDone && io.inReady === 1.U && io.isHead === 1.U){    // no puncturing
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
