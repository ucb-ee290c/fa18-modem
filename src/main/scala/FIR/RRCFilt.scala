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
 * Raised-cosine taps generator
 */
object RCTaps {
  def apply[T <: Data](params: RCFilterParams[T]): Seq(Double) = {
    require(params.symbolSpan >= 1)
    require(params.sampsPerSymbol >= 1)
    require(params.alpha > 0.0)
    require(params.alpha <= 1.0)
    val ntaps = params.sampsPerSymbol * params.symbolSpan + 1
    val n = linspace(0, ntaps-1, ntaps)
    println("n: $n")
    val taps = sinc(n / params.sampsPerSymbol) *
               cos(params.alpha * Constants.Pi * n / params.sampsPerSymbol) /
               (1 - pow(2 * params.alpha * n / params.sampsPerSymbol, 2))
    println("taps: $taps")
    val normalized = taps / norm(taps)
    println("normalized: $normalized")
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
  val doubleTaps = RCTaps(params)
  val taps = doubleTaps map {case x => ConvertableTo[T].fromDouble(x)}
  val xn = Reg(Vec(taps.length, params.protoIQ))

}