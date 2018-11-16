package Coding

import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem

class ConvCoding[T <: Data](params: CodingParams[T]) extends Module {
  val io = IO(new Bundle {
    val in        = Input(UInt(1.W))    // assuming k=1 for all convolutional coding
    val out       = Output(Vec(params.n, UInt(1.W)))

    val inReady   = Input(UInt(1.W))
    val outReady  = Output(UInt(1.W))

    val stateIn   = Input(UInt(2.W))
    val stateOut  = Output(UInt(2.W))
  })
  // Note: m+1 memory will be instantiated because input bit will also be stored in mem(0) for simpler implementation
  val shiftReg          = RegInit(VecInit(Seq.fill(params.K)(0.U(1.W)))) // initialze memory with all zeros
  val termReg           = RegInit(VecInit(Seq.fill(params.K)(0.U(1.W))))
  val regWires          = Wire(Vec(params.K, UInt(1.W)))
  val AXWires           = Wire(Vec(params.n, Vec(params.K, UInt(1.W)))) // Wires for And & Xor
  val n_cnt             = RegInit(0.U(log2Ceil(params.L).W))  // Create a counter             // may not be used
  val tail_cnt          = RegInit(0.U(log2Ceil(params.m).W))  // for zero termination         // may not be used
  val outReadyReg       = RegInit(0.U(1.W))

  val genPolyList       = CodingUtils.dec2bitarray(params.genPolynomial, params.K)
  val genPolyVec        = Wire(Vec(params.n, Vec(params.K, UInt(1.W))))
  (0 until params.n).map(i => {
    (0 until params.K).map(j => {
      genPolyVec(i)(j)  := (genPolyList(i)(j)).U
    })
  })

  // Make states for state machine
  val sStartRecv  = 0.U(2.W)        // start taking input bits
  val sEOS        = 1.U(2.W)
  val sDone       = 2.U(2.W)
  val stateWire   = Wire(UInt(2.W))
  stateWire := io.stateIn

  /* here is the logic:
  1) keep receiving input data bits
  2) start bit shifting
  3) if tail-biting is enabled, keep storing data into termReg
  3-1) zero-flush is default and 'm' 0s will be inserted in the incoming data bits
  4) when either io.in.valid or io.in.ready goes to 0, stop receiving data
  5) if tail-biting is enabled, store last 'm' bits from previous input data back to the shift registers
  */
  when(io.inReady === 1.U && io.stateIn === sStartRecv){
    shiftReg(0) := io.in         // receive input from FIFO. I probably need to use MUX
    (1 to params.m).reverse.map(i => {shiftReg(i) := shiftReg(i-1) })  // start bit shifting

    // if tail-biting is selected, store last 'm' bits into termReg
    when(params.tailBitingEn.asBool() === true.B){
      (0 until params.m).map(i => { termReg(i) := shiftReg(params.m - (i+1)) })
      termReg(params.m) := io.in
    }
  }.otherwise {                  // below starts once we stop receiving bits from input
    when(io.inReady === 0.U && io.stateIn === sStartRecv) {    // io.in.valid === false.B indicate end of input sequence
      when(params.tailBitingEn.asBool() === true.B){
        (0 to params.m).map(i => { shiftReg(i) := termReg(i) })
      }
      stateWire := sEOS
    }
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
  io.stateOut   := stateWire
  outReadyReg   := io.inReady
  io.outReady   := outReadyReg
}
