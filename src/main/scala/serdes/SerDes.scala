package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

import dsptools.numbers._

/**
 * Base classes for serializer and deserializer parameters
 *
 * These are type generic.
 * Note that there are 2 versions of these parameters.
 * One for the serializer/deserialization of PacketBundles, and another for BitsBundles.
 * TODO: With a better class hierarchy between PacketBundles and BitsBundles (as opposed to them being separate identities),
 * there would not be a need for two copies of these parameter classes, as well as the serializer and deserializer modules themselves.
 */
trait PacketSerDesParams[T <: Data] extends PacketBundleParams[T] {
  val ratio: Int         // serialization/deserialization ratio
  lazy val width = ratio // overrides the `width` parameter of PacketBundleParams
}
object PacketSerDesParams {
  def apply[T <: Data](proto: DspComplex[T], serdesRatio: Int): PacketSerDesParams[T] = new PacketSerDesParams[T] {
    val protoIQ = proto
    val ratio   = serdesRatio
  }
}

trait BitsSerDesParams[T <: Data] extends BitsBundleParams[T] {
  val ratio: Int             // serialization/deserialization ratio
  lazy val bitsWidth = ratio // overrides the `width` parameter of BitsBundleParams
}
object BitsSerDesParams {
  def apply[T <: Data](proto: T, serdesRatio: Int): BitsSerDesParams[T] = new BitsSerDesParams[T] {
    val protoBits = proto
    val ratio     = serdesRatio
  }
}

/**
 * Type specific case classes for the parameter base classes
 */

// FixedPoint for serialization/deserialization of PacketBundles
case class FixedPacketSerDesParams(
  ratio    : Int,
  dataWidth: Int, // width of input and output
  binPoint : Int  // binary point of input and output
) extends PacketSerDesParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
}

// UInt for serialization/deserialization of BitsBundles
case class UIntBitsSerDesParams(
  ratio    : Int,
  dataWidth: Int // width of input and output
) extends BitsSerDesParams[UInt] {
  val protoBits = UInt(dataWidth.W)
}

/**
 * Bundle type as IO for PacketDeserializer modules
 */
class PacketDeserializerIO[T <: Data : Ring](params: PacketSerDesParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
  val out = Decoupled(PacketBundle(params))

  override def cloneType: this.type = PacketDeserializerIO(params).asInstanceOf[this.type]
}
object PacketDeserializerIO {
  def apply[T <: Data : Ring](params: PacketSerDesParams[T]): PacketDeserializerIO[T] =
    new PacketDeserializerIO(params)
}

/**
 * Deserializer module.
 *
 * There's only one defined for deserializing PacketBundles because none is needed for BitsBundles.
 */
class PacketDeserializer[T <: Data : Real : BinaryRepresentation](val params: PacketSerDesParams[T]) extends Module {
  val io = IO(PacketDeserializerIO(params))

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
class PacketSerializerIO[T <: Data : Ring](params: PacketSerDesParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(PacketBundle(params)))
  val out = Decoupled(PacketBundle(1, params.protoIQ))

  override def cloneType: this.type = PacketSerializerIO(params).asInstanceOf[this.type]
}
object PacketSerializerIO {
  def apply[T <: Data : Ring](params: PacketSerDesParams[T]): PacketSerializerIO[T] =
    new PacketSerializerIO(params)
}

class BitsSerializerIO[T <: Data](params: BitsSerDesParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(BitsBundle(params)))
  val out = Decoupled(BitsBundle(1, params.protoBits))

  override def cloneType: this.type = BitsSerializerIO(params).asInstanceOf[this.type]
}
object BitsSerializerIO {
  def apply[T <: Data](params: BitsSerDesParams[T]): BitsSerializerIO[T] =
    new BitsSerializerIO(params)
}

/**
 * Serializer modules
 *
 * Two are defined, one for PacketBundle and another for BitsBundle.
 */
class PacketSerializer[T <: Data : Real : BinaryRepresentation](val params: PacketSerDesParams[T]) extends Module {
  val io = IO(PacketSerializerIO(params))

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

class BitsSerializer[T <: Data](val params: BitsSerDesParams[T]) extends Module {
  val io = IO(BitsSerializerIO(params))

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

  io.out.bits.bits(0) := in_flopped.bits(cntr)

  io.out.bits.pktStart := in_flopped.pktStart && (cntr === 0.U)
  io.out.bits.pktEnd := in_flopped.pktEnd && serLast
  io.out.valid := state === sComp
  io.in.ready := state === sIdle || (io.out.fire() && serLast)
}

