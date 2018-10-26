package Coding

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem
import org.scalacheck.Prop.False


/**
 * Base class for PacketDetect parameters
 * Type generic
 */
trait CodingParams[T <: Data] {
  val protoIn: T
  val protoOut: T
  val k: Int                        // size of smallest block of input bits
  val n: Int                        // size of smallest block of output bits
  val m: Int                        // number of memory elements. Constraint length is defined as K=m+1
  val K: Int                        // Constraint length
  val L: Int                        // length of input sequence
  val O: Int                        // OFDM bit / symbol. 48, 96, 192 can be used
  val nStates: Int                  // number of states
  val genPolynomial: List[Int]      // Matrix contains the generator polynomial
  val punctureEnable: Boolean       // enable/disable puncturing
  val punctureMatrix: List[Int]     // puncturing matrix
  val CodingScheme: Int             // 0: Convolutional Coding, 1: Turbo Coding, 2: LDPC
  val fbPolynomial: List[Int]       // feedback generator polynomial for Recursive Systematic Coding (Turbo Code)
  val tailBitingScheme: Int         // 0: zero tail-biting. 1: sophisticated tail-biting
}

/**
 * PacketDetect parameters for fixed-point data
 */
case class FixedCoding(
  k: Int = 1,
  n: Int = 2,
  m: Int = 3,
  L: Int = 7,
//  m: Int = 8,
//  L: Int = 6144,
  O: Int = 48,
  genPolynomial: List[Int] = List(7, 5), // generator polynomial
  punctureEnable: Boolean = false,
  punctureMatrix: List[Int] = List(6, 5), // Puncture Matrix
  CodingScheme: Int = 0,
  fbPolynomial: List[Int] = List(0),
  tailBitingScheme: Int = 0
) extends CodingParams[UInt] {
  val protoIn = UInt(1.W)
  val protoOut = UInt(O.W)
  val K = m + 1
  val nStates = math.pow(2.0, m.asInstanceOf[Double]).asInstanceOf[Int]
}

//class CodingBundle[T <: Data](params: CodingParams[T]) extends Bundle {
//  val in: T = params.protoIn.cloneType
//  val out: T = params.protoOut.cloneType
//
//  override def cloneType: this.type = CodingBundle(params).asInstanceOf[this.type]
//}
//object CodingBundle {
//  def apply[T <: Data](params: CodingParams[T]): CodingBundle[T] = new CodingBundle(params)
//}

class CodingIO[T <: Data](params: CodingParams[T]) extends Bundle {
//  val in  = Flipped(Decoupled(CodingBundle(params)))
//  val out = Decoupled(CodingBundle(params))
  val in    = Flipped(Decoupled(params.protoIn))
  val out   = Decoupled(params.protoOut)

  override def cloneType: this.type = CodingIO(params).asInstanceOf[this.type]
}
object CodingIO {
  def apply[T <: Data](params: CodingParams[T]): CodingIO[T] = new CodingIO(params)
}

class Encoding[T <: Data](params: CodingParams[T]) extends Module {
  require(params.m > 1)
  require(params.k > 0)
  require(params.n > 0)

  val io = IO(CodingIO(params))
  val inputLength       = (params.L).asUInt(log2Ceil(params.L).W)
  // Note: m+1 memory will be instantiated because input bit will also be stored in mem(0) for simpler implementation
  val memLength         = (params.K).asUInt(log2Ceil(params.K).W)
  val shiftReg          = RegInit(Vec(Seq.fill(params.K)(0.U(1.W)))) // initialze memory with all zeros
  val outReg            = RegInit(0.U((params.O).W))
  val pReg              = Reg(Vec(params.n, UInt(1.W)))                // pReg stores encoded bits. Ex) p0 and p1 in C.C.
  val termReg           = Reg(Vec(params.K, UInt(1.W)))
  val regWires          = Wire(Vec(params.K, UInt(1.W)))
  val AXWires           = Wire(Vec(params.n, Vec(params.K, UInt(1.W)))) // Wires for And & Xor
  val n_cnt             = RegInit(0.U(log2Ceil(params.L).W)) // Create a counter
  val tail_cnt          = RegInit(0.U(log2Ceil(params.m).W)) // for zero termination

  val genPolyList = Utils.dec2bitarray(params.genPolynomial, params.K)
  val PuntureList = Utils.dec2bitarray(params.punctureMatrix, Utils.findMinBitWidth(params.punctureMatrix))

  // Make states for state machine
  val sStartRecv  = 0.U(3.W)        // start taking input bits
  val sTailBiting = 1.U(3.W)        // stop receiving data & start tail-biting
  val sBreak      = 2.U(3.W)        // Stop receiving data & stop tail-biting
//  val sWork = 2.U(3.W)
//  val sDone = 3.U(3.W)
  val state = RegInit(sStartRecv)

  when(io.in.fire()) {
    // check if input has been received 'L' times
    when(n_cnt === inputLength) {
      when(params.tailBitingScheme.asUInt(1.W) === 1.U) {
        // if tail-biting is selected, store last 'm' bits into termReg
        for (i <- 0 to params.m) {
          termReg(i) := shiftReg(params.m - i)
        }
      }
      state := sTailBiting        // start tail-biting when it receives all the input sequences
      n_cnt := 0.U                // reset the n_cnt and lower the "ready" flag
    }.otherwise {
      shiftReg(0) := io.in.bits.asInstanceOf[Bool]      // receive input from FIFO
      for (i <- (1 to params.m).reverse) {              // start bit shifting
        shiftReg(i) := shiftReg(i-1)
      }
      n_cnt := n_cnt + 1    // increase counter value
    }
  }.otherwise {
    when(state === sTailBiting) {
      when(params.tailBitingScheme.asUInt(1.W) === 1.U) { // Non-zero termination
        for (i <- 0 to params.m) {
          shiftReg(i) := termReg(i)
        }
      }.otherwise{                                        // zero termination starts
        when(tail_cnt === memLength) {
          // finish termination and move on to the next state when tail_cnt reaches to its max
          tail_cnt := 0.U // reset the counter and stop feeding any data to the first memory element
          state := sBreak
        }.otherwise {
          // feed '0' to the first memory element
          shiftReg(0) := false.B
          for (i <- (1 to params.m).reverse) {
            shiftReg(i) := shiftReg(i - 1)
          }
          tail_cnt := tail_cnt + 1
        }
      }
    }
  }

  // connect wires to the output of each memory element
  for (i <- 0 to params.m) {
    regWires(i) := shiftReg(i)
  }

  // Generate encoded data
  for (i <- 0 until params.n){
    AXWires(i)(0) := regWires(0) & (genPolyList(i)(0)).U(1.W)                         // AND gate
    for (j <- 1 to params.m) {
      AXWires(i)(j) := AXWires(i)(j-1) ^ (regWires(j) & (genPolyList(i)(j)).U(1.W))   // AND -> XOR output
    }
    pReg(i) := AXWires(i)(params.m)
  }

  // check if puncturing needs to be done
  when(params.punctureEnable.B === true.B){
    // puncturing is disabled. skip puncturing and put the data directly into the buffer
    for (i <- 0 to )
  }.otherwise{


  }
  // insert punctured data into final output buffer

  when(io.out.fire() && state === sDone) {
    // When io.out.valid && io.out.ready both are high
    state := sInit
  }

  io.out.bits   := outReg
  // connect registers to output
  io.in.ready   := state === sStartRecv
  io.out.valid  := state === sDone
}

//  def genPolyInit(g_matrix: Array[Array[Int]], bit_width: Int): Unit = {
//    val vec2D = VecInit(g_matrix.map(x => VecInit(x.U(bit_width.W))))
//
//  }
//val genPolyList = VecInit(Utils.dec2bitarray(params.genPolynomial, params.K).map(params.protoIn.fromInt(_)))
//  val genPolyList: Unit = genPolyInit(params.genPolynomial, params.K)
//  printf(p"$v")
//
//  val r = Reg(UInt(3.W))
//  for (vi <- v) {
//    r.toBools().zip(vi).map{
//      case(a, b) => a*b
//    }.reduce(_ + _)
//  }