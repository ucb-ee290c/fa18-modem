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

import scala.math.pow


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
 * Zero-pad incoming data when oversampled to produce proper impulse response
 */
class ComplexZeroPad[T<:Data:ConvertableTo](val params: RCFilterParams[T]) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
    val out = Decoupled(PacketBundle(1, params.protoIQ))
  })
  if (params.sampsPerSymbol == 1) {
    io.in <> io.out
  }
  else {
    val pktEndReg = RegEnable(next=io.in.bits.pktEnd, enable=io.in.fire())
    // We have to create a zero literal the hacky way because DspComplex is a Bundle and Bundles are not literals. Or something.
    val zero = Wire(params.protoIQ)
    zero.real := ConvertableTo[T].fromDouble(0.0)
    zero.imag := ConvertableTo[T].fromDouble(0.0)
    require(params.sampsPerSymbol < pow(2, 8))  // Hopefully we're not oversapling by more than 256...
    val padCount = RegInit(0.U(8.W))
    padCount := Mux(io.out.fire(),
                    Mux(padCount === (params.sampsPerSymbol - 1).U, 0.U, padCount + 1.U),
                    padCount)
    printf("padCount %d\n", padCount)
    when(padCount === 0.U) {
      io.out.bits.iq := io.in.bits.iq
      io.out.bits.pktStart := io.in.bits.pktStart
      io.out.bits.pktEnd := false.B
      io.in.ready := io.out.ready
      io.out.valid := io.in.valid
    }.otherwise {
      io.out.bits.iq(0) := zero
      io.out.bits.pktStart := false.B
      io.out.bits.pktEnd := Mux(padCount === (params.sampsPerSymbol - 1).U, pktEndReg, false.B)
      io.in.ready := false.B
      io.out.valid := true.B
    }
  }
}
object ComplexZeroPad {
  def apply[T<:Data:ConvertableTo](params: RCFilterParams[T], in: DecoupledIO[PacketBundle[T]]): DecoupledIO[PacketBundle[T]] = {
    val zp = Module(new ComplexZeroPad(params))
    zp.io.in <> in
    zp.io.out
  }
}

/**
 * Raised-Cosine filter
 *
 * Input:
 *   PacketBundle of width 1
 *    -> pktEnd field is used to ensure each packet is flushed properly
 *
 * Output:
 *   IQBundle to be fed to the D2A converter
 */
class RCFilter[T <: Data : Real : ConvertableTo](val params: RCFilterParams[T]) extends Module {
  val io = IO(RCFilterIO(params))
  // Zero-pad inputs
  val zp = ComplexZeroPad(params, io.in)
  // Flush state variables
  val sIdle :: sMain :: sFlush :: Nil = Enum(3)
  val state = RegInit(sFlush)
  val flushCount = RegInit(0.U(32.W))
  val nTaps = params.sampsPerSymbol * params.symbolSpan + 1
  // Convert taps to fixedpoint
  val doubleTaps = RCTaps(params).tail.reverse ++ RCTaps(params) // TODO: convert to single-sided implementation
  val taps: Seq[T] = doubleTaps map {case x => ConvertableTo[T].fromDouble(x)}
  // Push incoming samples through buffer
  val x0 = Wire(params.protoIQ)
  x0 := zp.bits.iq(0)
  val doShift = Wire(Bool())
  doShift := false.B
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
  val zero = Wire(params.protoIQ)
  zero.real := Ring[T].zero
  zero.imag := Ring[T].zero
  switch(state) {
    is(sIdle) {
      flushCount := 0.U
      doShift := true.B
      x0 := Mux(zp.fire(), zp.bits.iq(0), zero)
      state := Mux(zp.fire(),
                   Mux(zp.bits.pktEnd, sFlush, sMain),
                   sIdle)
    }
    is(sMain) {
      flushCount := 0.U
      doShift := zp.fire()
      x0 := zp.bits.iq(0)
      state := Mux(zp.fire() && zp.bits.pktEnd, sFlush, sMain)
    }
    is(sFlush) {
      flushCount := flushCount + io.out.ready
      x0 := zero
      doShift := io.out.ready
      state := Mux(io.out.ready && (flushCount >= taps.length - 1), sIdle, sFlush)
    }
  }
  // Decoupled logic
  io.out.bits.iq.real := outReal
  io.out.bits.iq.imag := outImag
  io.out.valid := ((state === sMain) && zp.fire()) || (state === sFlush)
  zp.ready := ((state === sMain) && io.out.ready) || (state === sIdle)
}
