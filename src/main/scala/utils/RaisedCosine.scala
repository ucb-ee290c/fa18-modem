package modem

import chisel3._
import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants.Pi

/**
 * Raised-cosine taps generator
 * Returns a single side of the full filter, since it is real and symmetric
 */
object RCTaps {
  def apply[T <: Data](params: RCFilterParams[T]): scala.collection.immutable.Vector[Double] = {
    require(params.symbolSpan >= 1)
    require(params.sampsPerSymbol >= 1)
    require(params.alpha > 0.0)
    require(params.alpha <= 1.0)
    val ntaps = params.sampsPerSymbol * params.symbolSpan + 1
    val N = linspace(0, ntaps-1, ntaps)
    val taps = N map {n => sincpi(n / params.sampsPerSymbol.toDouble) *
               cos(params.alpha * Pi * n / params.sampsPerSymbol.toDouble) /
               (1 - pow(2.0 * params.alpha * n / params.sampsPerSymbol.toDouble, 2))}
    val normalized = taps / norm(taps)
    normalized.toScalaVector
  }
}