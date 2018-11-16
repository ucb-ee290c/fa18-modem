package Coding

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// TODO: zero-flush bufInterleaver when all the packets are properly received
class DePuncturing[T <: Data](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val in_hard   = Input(Vec(params.O, SInt(2.W)))
//    val in_soft   = Input(Vec(params.O, DspComplex(FixedPoint(2.W, 30.BP))))
    val out_hard       = Output(Vec(params.n, SInt(2.W)))
//    val out_soft       = Output(Vec(params.n, DspComplex(FixedPoint(2.W, 30.BP))))

    val inReady   = Input(UInt(1.W))

    val stateIn   = Input(UInt(2.W))
    val stateOut  = Output(UInt(2.W))
  })

  val o_cnt             = RegInit(0.U(log2Ceil(params.O).W))              // counter for data vector tracker
  val p_cnt             = RegInit(0.U(log2Ceil(params.O).W))              // counter for outReg tracker
  val bufInterleaver    = RegInit(VecInit(Seq.fill(params.n)(0.S(2.W))))  // buffer for interleaver
  val puncMatBitWidth   = CodingUtils.findMinBitWidth(params.punctureMatrix)

  val punctureList      = CodingUtils.dec2bitarray(params.punctureMatrix, puncMatBitWidth)
  val punctureVec       = Wire(Vec(params.n, Vec(puncMatBitWidth, UInt(1.W))))
  (0 until params.n).map(i => {
    (0 until puncMatBitWidth).map(j => {
      punctureVec(i)(j) := (punctureList(i)(j)).U
    })
  })

  // puncListColSum contains summation over rows
  // ex) [1,1,0], [1,0,1] -> [2,1,1]
  val puncListColSum    = punctureList.map(breeze.linalg.Vector(_)).reduce(_ + _)
  val puncListColSumWire      = Wire(Vec(puncMatBitWidth, UInt((log2Ceil(params.n+1)).W)))
  (0 until puncMatBitWidth).map(i => { puncListColSumWire(i) := puncListColSum(i).U })

  // puncIndices contains buffer address offset
  // ex) [1,1,0],[1,0,1] -> [1,1,0],[2,1,1] : accumulate over rows
  val puncIndices       = punctureList.scanLeft(Array.fill(punctureList(0).length)(0)) ((x,y) =>
    x.zip(y).map(e => e._1 + e._2)).drop(1)
  // convert this to chisel-usable variable using vector of wires
  val puncIndicesWire   = Wire(Vec(params.n, Vec(puncMatBitWidth, UInt((log2Ceil(params.O)+1).W))))
  (0 until params.n).map(i => {
    (0 until puncMatBitWidth).map(j => {
      puncIndicesWire(i)(j) := puncIndices(i)(j).U
    })
  })

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val stateWire   = Wire(UInt(2.W))
  stateWire := io.stateIn

  // puncturing Matrix: [1,1,0],[1,0,1]
  // Input Matrix: [A0,A1,A2], [B0, B1, B2] -> Output Matrix: [A0, B0, A1, B2]
  when(io.stateIn =/= sDone && io.inReady === 1.U){
    when(params.punctureEnable.B === true.B) {    // if puncturing is enabled,
      for (i <- 0 until params.n) {
        when(punctureVec((o_cnt+i.U) % params.n.U)((o_cnt / params.n.U) % puncMatBitWidth.U) === 1.U) {
          bufInterleaver(i.U) := io.in_hard(p_cnt - 1.U + puncIndicesWire((o_cnt+i.U) % params.n.U)(((o_cnt+i.U) / params.n.U) % puncMatBitWidth.U))
          // o_cnt + i.U / params.n.U % puncMatBitWidth.U must reach to puncMatBitWidth.U-1
        }.otherwise{
          bufInterleaver(i.U) := 0.S
        }
      }
      p_cnt := p_cnt + puncListColSumWire((o_cnt/params.n.U) % puncMatBitWidth.U)
      o_cnt := o_cnt + params.n.U
      when(p_cnt >= (params.O.U - puncListColSumWire((o_cnt/params.n.U) % puncMatBitWidth.U))) {
        stateWire := sDone
        p_cnt := 0.U
      }
      when((o_cnt >= (params.O - params.n).U) && ((((o_cnt+1.U) / params.n.U) % puncMatBitWidth.U) === (puncMatBitWidth.U -1.U))) {
        o_cnt := 0.U
      }
    }.otherwise{                                  // no puncturing
      (0 until params.n).map(i => { bufInterleaver(i.U) := io.in_hard(o_cnt + i.U) })
      o_cnt := o_cnt + params.n.U
      when(o_cnt === (params.O - params.n).U) {
        stateWire := sDone
      }
      when(o_cnt >= (params.O - params.n).U){
        o_cnt := 0.U
      }
    }
  }

  // connect registers to output
  io.out_hard := bufInterleaver
  io.stateOut := stateWire
}
