package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

import dsptools.numbers._

import scala.math._

/**
 * Base class for Serializer/Deserializer parameters
 *
 * These are type generic
 */
trait SerDesParams[T <: Data] extends IQBundleParams[T] {
  val ratio: Int
}
object SerDesParams {
  def apply[T <: Data](proto: DspComplex[T], serdesRatio: Int): SerDesParams[T] = new SerDesParams[T] {
    val protoIQ = proto
    val ratio = serdesRatio
  }
}

/**
 * Serializer/Deserializer parameters object for fixed-point Serializer/Deserializer
 */
case class FixedSerDesParams(
  ratio: Int,
  // width of Input and Output
  dataWidth: Int,
  maxVal: Int
) extends SerDesParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, (dataWidth - 2 - log2Ceil(maxVal)).BP))
}

/**
 * Bundle type as IO for Deserializer modules
 */
class DeserializerIO[T <: Data : Ring](params: SerDesParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(PacketBundle(1, params.protoIQ.cloneType)))
  val out = Decoupled(PacketBundle(params.ratio, params.protoIQ.cloneType))

  override def cloneType: this.type = DeserializerIO(params).asInstanceOf[this.type]
}
object DeserializerIO {
  def apply[T <: Data : Ring](params: SerDesParams[T]): DeserializerIO[T] =
    new DeserializerIO(params)
}

class Deserializer[T <: Data : Real : BinaryRepresentation](val params: SerDesParams[T]) extends Module {
  val io = IO(DeserializerIO(params))

  val sIdle :: sComp :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  val cntr = RegInit(0.U(log2Up(params.ratio).W))
  val cntr_next = Wire(cntr.cloneType)

  // Default values
  cntr_next  := cntr
  state_next := state

  val deser = Reg(io.out.bits.cloneType)

  switch (state) {
    is (sIdle) {
      when (io.in.fire()) { state_next := sComp }
    }
    is (sComp) {
      cntr_next      := cntr + 1.U
      deser.pktStart := deser.pktStart || io.in.bits.pktStart
      deser.pktEnd   := deser.pktEnd   || io.in.bits.pktEnd
      when (cntr === (params.ratio - 2).U) { state_next := sDone }
    }
    is (sDone) {
      when (io.in.fire())       { state_next := sComp }
      .elsewhen (io.out.fire()) { state_next := sIdle }
    }
  }

  cntr  := cntr_next
  state := state_next

  when (state_next === sComp && state =/= sComp) {
    cntr_next      := 0.U
    deser.pktStart := io.in.bits.pktStart
    deser.pktEnd   := io.in.bits.pktEnd
  }

  // Shift register
  deser.iq.foldRight(io.in.bits.iq(0)) {
    case (reg, inp) => {
      when (io.in.fire()) { reg := inp }
      reg
    }
  }

  io.in.ready  := !io.out.valid || io.out.ready // Deserializer can always receive more data unless there's backpressure
  io.out.valid := state === sDone
  io.out.bits  := deser
}

/**
 * Bundle type as IO for Serializer modules
 */
class SerializerIO[T <: Data : Ring](params: SerDesParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(PacketBundle(params.ratio, params.protoIQ.cloneType)))
  val out = Decoupled(PacketBundle(1, params.protoIQ.cloneType))

  override def cloneType: this.type = SerializerIO(params).asInstanceOf[this.type]
}
object SerializerIO {
  def apply[T <: Data : Ring](params: SerDesParams[T]): SerializerIO[T] =
    new SerializerIO(params)
}

class Serializer[T <: Data : Real : BinaryRepresentation](val params: SerDesParams[T]) extends Module {
  val io = IO(SerializerIO(params))

  val sIdle :: sComp :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val in_flopped = Reg(io.in.bits.cloneType)

  val cntr = RegInit(0.U(log2Up(params.ratio).W))
  val cntr_next = Wire(cntr.cloneType)

  val serLast = cntr === (params.ratio - 1).U

  cntr_next := cntr

  when (state === sIdle) {
    when (io.in.fire()) {
      cntr_next := 0.U
      in_flopped := io.in.bits
      state := sComp
    }
  } .elsewhen (io.out.fire()) {
    when (serLast) {
      when (io.in.fire()) {
        cntr_next := 0.U
        in_flopped := io.in.bits
      } .otherwise {
        state := sIdle
      }
    } .otherwise {
      cntr_next := cntr + 1.U
    }
  }

  cntr := cntr_next

  io.out.bits.iq(0) := in_flopped.iq(cntr)
  io.out.bits.pktStart := in_flopped.pktStart && (cntr === 0.U)
  io.out.bits.pktEnd := in_flopped.pktEnd && serLast
  io.out.valid := state === sComp
  // Serializer can receive more data if...
  // 1. idle
  // 2. done with current in_flopped
  io.in.ready := state === sIdle || (io.out.fire() && serLast)
}
