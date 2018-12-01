/**
 * Raised-Cosine Filter Implementation
 *
 * Based on http://www.politecnica.pucrs.br/~decastro/pdf/ShapingFilterDesign.pdf
 *
 * Josh Sanz <jsanz@berkeley.edu>
 */
package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled
import dsptools.numbers._


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
  alpha: Double = 0.22,
  sampsPerSymbol: Int = 4,
  symbolSpan: Int = 3
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
 * Raised-Cosine filter
 *
 * Input:
 *   PacketBundle of width 1 to ensure each packet is flushed properly
 *
 * Output:
 *   IQBundle to be fed to the D2A converter
 */
class RCFilter[T <: Data : Real : ConvertableTo](val params: RCFilterParams[T]) extends Module {
  val io = IO(RCFilterIO(params))
  // Flush state variables
  val sMain :: sFlush :: Nil = Enum(2)
  val state = RegInit(sFlush)
  val flushCount = Reg(UInt(32.W))
  val nTaps = params.sampsPerSymbol * params.symbolSpan + 1
  // Convert taps to fixedpoint
  val doubleTaps = RCTaps(params).tail.reverse ++ RCTaps(params) // TODO: convert to single-sided implementation
  println(s"double taps $doubleTaps")
  val taps: Seq[T] = doubleTaps map {case x => ConvertableTo[T].fromDouble(x)}
  println(s"taps symmetric $taps")
  // Push incoming samples through buffer
  val x0 = Wire(params.protoIQ)
  val doShift = Wire(Bool())
  val xn = Seq.fill(taps.length)(Reg(params.protoIQ))
  xn.foldLeft(x0){case (prev, curr) => {
    curr := Mux(doShift, prev, curr)
    curr
    }
  }
  // Tree-reduce addition to reduce critical path
  val realProducts = xn zip taps map  {case (x, y) => x.real * y}
  val imagProducts = xn zip taps map  {case (x, y) => x.imag * y}
  val outReal = TreeReduce(realProducts, (a:T,b:T) => a+b)
  val outImag = TreeReduce(imagProducts, (a:T,b:T) => a+b)
  // Extra state logic to handle packets
  switch(state) {
    is(sMain) {
      flushCount := 0.U
      state := Mux(io.in.fire() && io.in.bits.pktEnd, sFlush, sMain)
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
  io.in.ready := (state === sMain) && io.out.ready
}