package modem

import chisel3._
import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants.Pi

/**
 * Raised-cosine taps generator
 */
object RCTaps {
  def apply[T <: Data](params: RCFilterParams[T]): scala.collection.immutable.Vector[Double] = {
    require(params.symbolSpan >= 1)
    require(params.sampsPerSymbol >= 1)
    require(params.alpha > 0.0)
    require(params.alpha <= 1.0)
    val ntaps = params.sampsPerSymbol * params.symbolSpan + 1
    val taps = linspace(0, ntaps-1, ntaps)
    println(s"n: $taps")
    taps foreach {n => sinc(n / params.sampsPerSymbol.toDouble) *
               cos(params.alpha * Pi * n / params.sampsPerSymbol.toDouble) /
               (1 - pow(2.0 * params.alpha * n / params.sampsPerSymbol.toDouble, 2))}
    println(s"taps: $taps")
    val normalized = taps / norm(taps)
    println(s"normalized: $normalized")
    normalized.toScalaVector
  }
}