package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{Decoupled, log2Ceil, log2Floor}

import dsptools.numbers._

import scala.math._

/**
 * Base class for Deserializer parameters
 *
 * These are type generic
 */
trait DeserializerParams[T <: Data] extends IQBundleParams[T] {
  val deserRatio: Int
}
object DeserializerParams {
  def apply[T <: Data](proto: DspComplex[T], ratio: Int): DeserializerParams[T] = new DeserializerParams[T] {
    val protoIQ = proto
    val deserRatio = ratio
  }
}

/**
 * Deserializer parameters object for fixed-point Deserializers
 */
case class FixedDeserializerParams(
  deserRatio: Int,
  // width of Input and Output
  dataWidth: Int,
  maxVal: Int
) extends DeserializerParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, (dataWidth - 2 - log2Ceil(maxVal)).BP))
}

/**
 * Bundle type as IO for Deserializer modules
 */
class DeserializerIO[T <: Data : Ring](params: DeserializerParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(PacketBundle(1, params.protoIQ.cloneType)))
  val out = Decoupled(PacketBundle(params.deserRatio, params.protoIQ.cloneType))

  override def cloneType: this.type = DeserializerIO(params).asInstanceOf[this.type]
}
object DeserializerIO {
  def apply[T <: Data : Ring](params: DeserializerParams[T]): DeserializerIO[T] =
    new DeserializerIO(params)
}

class Deserializer[T <: Data : Real : BinaryRepresentation : ChiselConvertableFrom](val params: DeserializerParams[T]) extends Module {
  val io = IO(DeserializerIO(params))

  val cntr = RegInit(0.U((log2Floor(params.deserRatio) + 1).W))
  val cntr_next = Wire(UInt((log2Floor(params.deserRatio) + 1).W))

  cntr := cntr_next

  val deserDone = cntr === params.deserRatio.U

  val deser = Reg(io.out.bits.cloneType)

  cntr_next := cntr
  when (io.in.fire()) {
    cntr_next := Mux(deserDone, 1.U, cntr + 1.U)

    // Set pktStart/pktEnd if there has been a pktStart/pktEnd asserted in the current window
    when (cntr_next === 1.U) {
      deser.pktStart := io.in.bits.pktStart
      deser.pktEnd   := io.in.bits.pktEnd
    } .otherwise {
      deser.pktStart := deser.pktStart || io.in.bits.pktStart
      deser.pktEnd   := deser.pktEnd   || io.in.bits.pktEnd
    }
  }

  // Shift register
  deser.iq.foldRight(io.in.bits.iq(0)) {
    case (reg, inp) => {
      when (io.in.fire()) {
        reg := inp
      }
      reg
    }
  }

  io.in.ready  := !io.out.valid || io.out.ready // Deserializer can always receive more data unless there's backpressure
  io.out.valid := deserDone

  io.out.bits := deser
}
