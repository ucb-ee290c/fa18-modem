/**
 * Raised-Cosine Filter Implementation
 *
 * Based on http://www.politecnica.pucrs.br/~decastro/pdf/ShapingFilterDesign.pdf
 *
 * Josh Sanz <jsanz@berkeley.edu>
 */

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import dsptools.numbers._

import breeze.linalg._
import breeze.numerics._

/**
 * Raised-Cosine parameters
 */
trait RCFilterParams[T<:Data] extends IQBundleParams[T] {
  val alpha: Double        // Filter rolloff / BW extension
  val sampsPerSymbol: Int  // Number of samples per symbol
  val symbolSpan: Int      // Number of symbol periods the filter should span
}
// FixedPoint case class for easy instantiation
case class FixedRCFilterParams(
  dataWidth: Int,
  binaryPoint: Int,
  alpha: Double,
  sampsPerSymbol: Int,
  symbolSpan: Int
) extends RCFilterParams[FixedPoint] {
  // IQ Data prototype
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, binaryPoint.BP))
}

/**
 * IO definition for RC filter
 */
class RCFilterIO[T<:Data](params: RCFilterParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
  val out = Decoupled(IQBundle(params))

  override def cloneType: this.type = RCFilterIO(params).asInstanceOf[this.type]
}
object RCFilterIO {
  def apply[T <: Data](params: RCFilterParams[T]): RCFilterIO[T] = {
    new RCFilterIO(params)
  }
}

/**
 * Tree-Reduce for better hardware instantiation
 */
object TreeReduce {
  def apply[V](in: Seq[V], func: (V, V) => V): V = {
    if (in.length == 1) {
      return in(0)
    }
    if (in.length == 2) {
      return func(in(0), in(1))
    }
    if (in.length % 2 == 0) {
      val withIdxs = in.zipWithIndex
      val evens = withIdxs.filter{case (_, idx) => idx % 2 == 0}.map(_._1)
      val odds  = withIdxs.filter{case (_, idx) => idx % 2 != 0}.map(_._1)
      val evenOddPairs: Seq[(V, V)] = evens zip odds
      return TreeReduce(evenOddPairs.map(x => func(x._1, x._2)), func)
    } else {
      return TreeReduce(Seq(in(0), TreeReduce(in.drop(1), func)), func)
    }
  }
}

/**
 * Raised-cosine taps generator
 */
object RCTaps {
  def apply[T <: Data](params: RCFilterParams[T]): Seq[Double] = {
    require(params.symbolSpan >= 1)
    require(params.sampsPerSymbol >= 1)
    require(params.alpha > 0.0)
    require(params.alpha <= 1.0)
    val ntaps = params.sampsPerSymbol * params.symbolSpan + 1
    val n = linspace(0, ntaps-1, ntaps)
    println(s"n: $n")
    val taps = sinc(n / params.sampsPerSymbol) *
               cos(params.alpha * Constants.Pi * n / params.sampsPerSymbol) /
               (1 - pow(2 * params.alpha * n / params.sampsPerSymbol, 2))
    println(s"taps: $taps")
    val normalized = taps / norm(taps)
    println(s"normalized: $normalized")
    normalized
  }
}

/**
 * Raised-Cosine filter
 *
 * Input:
 *   PacketBundle of width 1 to ensure each packet is flushed properly
 *
 * Output:
 *   IQBundle to be fed to the D2A converter
 */
class RCFilter[T <: Data : Ring : ConvertableTo](val params: RCFilterParams[T]) extends Module {
  val io = IO(RCFilterIO(params))
  // Flush state variables
  val sMain :: sFlush :: Nil = Enum(2)
  val state = RegInit(sFlush)
  val flushCount = Reg(UInt(32.W))
  // Convert taps to fixedpoint
  val doubleTaps = RCTaps(params).tail.reverse ++ RCTaps(params) // TODO: convert to single-sided implementation
  println(s"double taps $doubleTaps")
  val taps = doubleTaps map {case x => ConvertableTo[T].fromDouble(x)}
  println(s"taps symmetric $taps")
  // Push incoming samples through buffer
  val xn = Reg(Vec(2 * taps.length - 1, params.protoIQ), en=io.in.fire())
  xn.foldLeft(io.in.bits.iq){
    (prev, curr) => {
      curr := prev
    }
  }
  // Tree-reduce addition to reduce critical path
  val outReal := TreeReduce(xn zip taps map {(x,y) => x.real * y}, (a,b) => a + b)
  val outImag := TreeReduce(xn zip taps map {(x,y) => x.imag * y}, (a,b) => a + b)
  // Extra state logic to handle packets
  switch(state) {
    is(sMain) {
      flushCount := 0.U
      state := Mux(io.in.fire() && io.in.pktEnd, sFlush, sMain)
    }
    is(sFlush) {
      flushCount := flushCount + 1.U
      val zero = Wire(params.protoIQ)
      zero.real := Ring[T].zero
      zero.imag := Ring[T].zero
      xn(0) := zero
      state := Mux(io.in.fire() && (flushCount >= taps.length - 1), sMain, sFlush)
    }
  }
  // Decoupled logic
  io.out.bits.iq.real := outReal
  io.out.bits.iq.imag := outImag
  io.out.valid := io.in.fire()
  io.in.ready := (state == sMain) && io.out.ready
}