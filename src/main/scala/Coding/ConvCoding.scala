package modem

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

// Written by Kunmo Kim : kunmok@berkeley.edu
// Description: Convolutional encoder for 802.11a standard
class ConvCoding[T <: Data, U <: Data](params: CodingParams[T, U]) extends Module {
  val io = IO(new Bundle {
    val in        = Flipped(Decoupled(UInt(1.W)))
    val out       = Output(Vec(params.n, UInt(1.W)))

    val inReady   = Input(Bool())      // takes ready signal from interleaver buffer
    val isHeadIn  = Input(Bool())
    val pktStrIn  = Input(Bool())
    val pktEndIn  = Input(Bool())

    val outReady  = Output(UInt(1.W))
    val isHeadOut = Output(Bool())
    val pktStrOut = Output(Bool())
    val pktEndOut = Output(Bool())
    val pktLatOut = Output(Bool())
  })
  // Note: m+1 memory will be instantiated because input bit will also be stored in mem(0) for simpler implementation
  val shiftReg          = RegInit(VecInit(Seq.fill(params.K)(0.U(1.W)))) // initialze memory with all zeros
  val termReg           = RegInit(VecInit(Seq.fill(params.K)(0.U(1.W))))
  val regWires          = Wire(Vec(params.K, UInt(1.W)))
  val AXWires           = Wire(Vec(params.n, Vec(params.K, UInt(1.W)))) // Wires for And & Xor
  val n_cnt             = RegInit(0.U(log2Ceil(params.L).W))  // Create a counter             // may not be used
  val outReadyReg       = RegInit(false.B)
  val isHeadReg         = RegInit(false.B)
  val pktStrReg         = RegInit(false.B)
  val pktEndReg         = RegInit(false.B)
  val pktLatReg         = RegInit(false.B)

  val genPolyList       = CodingUtils.dec2bitarray(params.genPolynomial, params.K)
  val genPolyVec        = Wire(Vec(params.n, Vec(params.K, UInt(1.W))))
  (0 until params.n).map(i => {
    (0 until params.K).map(j => {
      genPolyVec(i)(j)  := (genPolyList(i)(j)).U
    })
  })

  /* here is the logic:
  1) keep receiving input data bits
  2) start bit shifting
  3) zero-flush is default and 'm' 0s will be inserted in the incoming data bits
  4) when either io.in.valid or io.in.ready goes to 0, stop receiving data
  * Note: tail-biting portion has been removed since it is not being used at all in 802.11a standard.
  */
  when(io.in.fire() === true.B) {
    shiftReg(0) := io.in.bits // receive input from FIFO. I probably need to use MUX
    (1 to params.m).reverse.map(i => {
      shiftReg(i) := shiftReg(i - 1)
    }) // start bit shifting
  }

  // connect wires to the output of each memory element
  (0 to params.m).map(i => { regWires(i) := shiftReg(i) })

  for (i <- 0 until params.n){
    AXWires(i)(0) := regWires(0) & (genPolyVec(i)(0))                         // AND gate
    for (j <- 1 to params.m) {
      AXWires(i)(j) := AXWires(i)(j-1) ^ (regWires(j) & (genPolyVec(i)(j)))   // AND -> XOR output
    }
  }

  // connect registers to output
  (0 until params.n).map(i => {io.out(i) := AXWires(i)(params.K-1)})   // zero-flush output buffer

  isHeadReg     := io.isHeadIn
  pktStrReg     := io.pktStrIn
  pktEndReg     := io.pktEndIn
  pktLatReg     := io.in.valid

  io.isHeadOut  := isHeadReg
  io.pktStrOut  := pktStrReg
  io.pktEndOut  := pktEndReg
  io.pktLatOut  := pktLatReg
  outReadyReg   := io.inReady
  io.outReady   := outReadyReg    // introducing a single clk cycle delay since ConvCoding has 1 clk cycle delay
  io.in.ready   := io.inReady

}
