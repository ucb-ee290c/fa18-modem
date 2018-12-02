package modem

import chisel3._
import chisel3.util._
import dsptools.numbers.Real
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// TODO: zero-flush bufInterleaver when all the packets are properly received -> assuming this will be done by MAC layer
class Puncturing[T <: Data, U <: Data](params: CodingParams[T, U]) extends Module {
  val io = IO(new Bundle {
    val in          = Input(Vec(params.n, UInt(1.W)))
    val inReady     = Input(UInt(1.W))
    val isHead      = Input(Bool())                               // from MAC layer
    val puncMatrix  = Input(Vec(4, UInt(1.W)))                    // from MAC layer
    val pktStrIn    = Input(Bool())
    val pktEndIn    = Input(Bool())

    val out         = Decoupled(Vec(params.bitsWidth, UInt(1.W)))
    val modCtrl     = Output(UInt(2.W))
    val pktStrOut   = Output(Bool())
    val pktEndOut   = Output(Bool())
  })

  val puncMatBitWidth     = RegInit(0.U(4.W))
  val punctureVecReg      = RegInit(VecInit(Seq.fill(params.n)(VecInit(Seq.fill(7)(0.U(1.W))))))  // support up to 7/8 coding rate
  val puncIndicesReg      = RegInit(VecInit(Seq.fill(params.n)(VecInit(Seq.fill(7)(0.U((log2Ceil(params.bitsWidth)+1).W))))))
  val puncListColSumReg   = RegInit(VecInit(Seq.fill(7)(0.U((log2Ceil(params.n+1)).W))))

  val modCtrlReg          = RegInit(0.U(2.W))

  val pktStartReg         = RegInit(false.B)
  pktStartReg := io.pktStrIn
  val pktEndReg           = RegInit(false.B)
  pktEndReg   := io.pktEndIn


  val bpsk = 0.U
  val qpsk = 1.U
  val qam16 = 2.U
  val qam64 = 3.U
  /*
    R1-R4 | Rate (Mb/s) | Puncturing Matrix
    1101  | 6           | 1/2  - BPSK
    1111  | 9           | 3/4  - BPSK
    0101  | 12          | 1/2  - QPSK
    0111  | 18          | 3/4  - QPSK
    1001  | 24          | 1/2  - QAM16
    1011  | 36          | 3/4  - QAM16
    0001  | 48          | 2/3  - QAM64
    0011  | 54          | 3/4  - QAM64

    1/2 -> 0.U  ->  nand(R1,R2) && !R3 && R4
    2/3 -> 1.U  ->  !R1 && !R2 && !R3 && R4
    3/4 -> 2.U  ->  R3 && R4
   */
  when(io.isHead === true.B){
    // select puncturing rate
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

    // select modulation scheme
    when(io.puncMatrix(0).toBool() && io.puncMatrix(1).toBool()){
      modCtrlReg := 0.U   // BPSK
    }.elsewhen(!io.puncMatrix(0).toBool() && io.puncMatrix(1).toBool()){
      modCtrlReg := 1.U   // QPSK
    }.elsewhen(io.puncMatrix(0).toBool() && !io.puncMatrix(1).toBool()){
      modCtrlReg := 2.U   // QAM-16
    }.elsewhen(!io.puncMatrix(0).toBool() && !io.puncMatrix(1).toBool()){
      modCtrlReg := 3.U   // QAM-64
    }.otherwise{
      modCtrlReg := 0.U   // BPSK
    }
  }

  val o_cnt             = RegInit(0.U(log2Ceil(params.bitsWidth).W))              // counter for data vector tracker
  val p_cnt             = RegInit(0.U(log2Ceil(params.bitsWidth).W))              // counter for outReg tracker
  val bufInterleaver    = RegInit(VecInit(Seq.fill(params.bitsWidth)(0.U(1.W))))  // buffer for interleaver

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val outValid    = RegInit(false.B)

  // ex) puncturing Matrix: [1,1,0],[1,0,1]
  // -> Input Matrix: [A0,A1,A2], [B0, B1, B2] -> Output Matrix: [A0, B0, A1, B2]
  when( io.inReady === 1.U && io.isHead === false.B){
    // if puncturing is enabled,
    for (i <- 0 until params.n) {
      when(punctureVecReg((o_cnt+i.U) % params.n.U)((o_cnt / params.n.U) % puncMatBitWidth) === 1.U) {
        bufInterleaver(p_cnt - 1.U + puncIndicesReg((o_cnt+i.U) % params.n.U)(((o_cnt+i.U) / params.n.U) % puncMatBitWidth)) := io.in(i.U)
      }
    }
    p_cnt := p_cnt + puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth)
    o_cnt := o_cnt + params.n.U
    when(p_cnt >= (params.bitsWidth.U - puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth))) {
      outValid  := true.B
      p_cnt := 0.U
    }
    when((o_cnt >= (params.bitsWidth - params.n).U) && ((((o_cnt+1.U) / params.n.U) % puncMatBitWidth) === (puncMatBitWidth -1.U))) {
      o_cnt := 0.U
    }
  }.elsewhen( io.inReady === 1.U && io.isHead === true.B){    // no puncturing
    (0 until params.n).map(i => { bufInterleaver(o_cnt + i.U) := io.in(i.U) })
    o_cnt := o_cnt + params.n.U
    when(o_cnt === (params.bitsWidth - params.n).U) {
      outValid  := true.B
      o_cnt := 0.U
    }
  }
  when(io.out.fire()){
    outValid := false.B
  }
  // connect registers to output
  io.out.bits   := bufInterleaver
  io.out.valid  := outValid
  io.modCtrl    := Mux(io.isHead, 0.U, modCtrlReg)
  io.pktStrOut  := pktStartReg
  io.pktEndOut  := pktEndReg
}
