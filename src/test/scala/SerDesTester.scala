package modem

import dsptools.DspTester
import breeze.math.Complex
import chisel3._
import dsptools.numbers._
import chisel3.util.Decoupled

/**
 * DspTester for PacketDeserializer
 */
class PacketDeserializerTester[T <: chisel3.Data](c: PacketDeserializer[T], inp: Seq[Complex], tolLSBs: Int = 1) extends DspTester(c) with HasTesterUtil[PacketDeserializer[T]] {
  val maxCyclesWait = 5
  assert(inp.length % c.params.ratio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  inp.zipWithIndex.foreach { case (value, index) =>
    poke(c.io.in.bits.iq(0), value)
    poke(c.io.in.bits.pktStart, (index == 0))
    poke(c.io.in.bits.pktEnd  , (index == inp.length - 1))
    wait_for_assert(c.io.in.ready, maxCyclesWait)
    step(1)
    poke(c.io.in.valid, 0)
    if (index % c.params.ratio == c.params.ratio - 1) {
      wait_for_assert(c.io.out.valid, maxCyclesWait)
      expect(c.io.out.bits.pktStart, (index < c.params.ratio))
      expect(c.io.out.bits.pktEnd  , (index == inp.length - 1))

      val deser_idx = index / c.params.ratio
      fixTolLSBs.withValue(tolLSBs) {
        expect_complex_seq(c.io.out.bits.iq, inp.slice(deser_idx * c.params.ratio, (deser_idx + 1) * c.params.ratio))
      }
    }
    poke(c.io.in.valid, 1)
  }
}

/**
 * DspTester for PacketSerializer
 */
class PacketSerializerTester[T <: chisel3.Data](c: PacketSerializer[T], inp: Seq[Complex], tolLSBs: Int = 1) extends DspTester(c) with HasTesterUtil[PacketSerializer[T]] {
  val maxCyclesWait = 5
  assert(inp.length % c.params.ratio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  var out_idx = 0

  for (deser_idx <- 0 until inp.length / c.params.ratio) {
    poke_complex_seq(c.io.in.bits.iq, inp.slice(deser_idx * c.params.ratio, (deser_idx + 1) * c.params.ratio))

    poke(c.io.in.bits.pktStart, deser_idx == 0)
    poke(c.io.in.bits.pktEnd  , deser_idx == inp.length / c.params.ratio - 1)

    wait_for_assert(c.io.in.ready, maxCyclesWait)
    step(1)

    poke(c.io.in.valid, 0)
    for (i <- 0 until c.params.ratio) {
      wait_for_assert(c.io.out.valid, maxCyclesWait)
      expect(c.io.out.bits.pktStart, (out_idx == 0))
      expect(c.io.out.bits.pktEnd  , (out_idx == inp.length - 1))
      fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits.iq(0), inp(out_idx)) }
      out_idx += 1
      step(1)
    }
    poke(c.io.in.valid, 1)
  }
}

/**
 * DspTester for BitsSerializer
 */
class BitsSerializerTester[T <: chisel3.Data](c: BitsSerializer[T], inp: IndexedSeq[Int], tolLSBs: Int = 0) extends DspTester(c) with HasTesterUtil[BitsSerializer[T]] {
  val maxCyclesWait = 5
  assert(inp.length % c.params.ratio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  var out_idx = 0

  for (deser_idx <- 0 until inp.length / c.params.ratio) {
    poke(c.io.in.bits.bits, inp.slice(deser_idx * c.params.ratio, (deser_idx + 1) * c.params.ratio).map(BigInt(_)).reverse)

    poke(c.io.in.bits.pktStart, deser_idx == 0)
    poke(c.io.in.bits.pktEnd  , deser_idx == inp.length / c.params.ratio - 1)

    wait_for_assert(c.io.in.ready, maxCyclesWait)
    step(1)

    poke(c.io.in.valid, 0)
    for (i <- 0 until c.params.ratio) {
      wait_for_assert(c.io.out.valid, maxCyclesWait)
      expect(c.io.out.bits.pktStart, (out_idx == 0))
      expect(c.io.out.bits.pktEnd  , (out_idx == inp.length - 1))
      fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits.bits(0), inp(out_idx)) }
      out_idx += 1
      step(1)
    }
    poke(c.io.in.valid, 1)
  }
}

/**
 * DspTester for PacketDeserializer and PacketSerializer in series
 */
class PacketDesSerTester[T <: chisel3.Data](c: PacketDesSerTestModule[T], inp: Seq[Complex], tolLSBs: Int = 1) extends DspTester(c) with HasTesterUtil[PacketDesSerTestModule[T]] {
  val maxCyclesWait = 5
  assert(inp.length % c.params.ratio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  var out_idx = 0

  inp.zipWithIndex.foreach { case (value, index) =>
    poke(c.io.in.bits.iq(0), value)
    poke(c.io.in.bits.pktStart, (index == 0))
    poke(c.io.in.bits.pktEnd  , (index == inp.length - 1))
    wait_for_assert(c.io.in.ready, maxCyclesWait)
    step(1)
    if (index % c.params.ratio == c.params.ratio - 1) {
      poke(c.io.in.valid, 0)
      while (out_idx <= index) {
        wait_for_assert(c.io.out.valid, maxCyclesWait)
        expect(c.io.out.bits.pktStart, (out_idx == 0))
        expect(c.io.out.bits.pktEnd  , (out_idx == inp.length - 1))
        fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits.iq(0), inp(out_idx)) }
        out_idx += 1
        step(1)
      }
      poke(c.io.in.valid, 1)
    }
  }
}

/**
 * Testing module for PacketDesSerTester
 */
class PacketDesSerTestModule[T <: Data : Real : BinaryRepresentation](val params: PacketSerDesParams[T]) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
    val out = Decoupled(PacketBundle(1, params.protoIQ))
  })

  val ser = Module(new PacketSerializer(params))
  val des = Module(new PacketDeserializer(params))

  des.io.in  <> io.in
  ser.io.in  <> des.io.out
  ser.io.out <> io.out
}

/**
 * Convenience functions for running tests
 */
object FixedPacketDeserializerTester {
  def apply(params: FixedPacketSerDesParams, inp: Seq[Complex]): Boolean = {
    dsptools.Driver.execute(() => new PacketDeserializer(params), TestSetup.dspTesterOptions) { c => new PacketDeserializerTester(c, inp) }
  }
}
object FixedPacketSerializerTester {
  def apply(params: FixedPacketSerDesParams, inp: Seq[Complex]): Boolean = {
    dsptools.Driver.execute(() => new PacketSerializer(params), TestSetup.dspTesterOptions) { c => new PacketSerializerTester(c, inp) }
  }
}
object FixedPacketDesSerTester {
  def apply(params: FixedPacketSerDesParams, inp: Seq[Complex]): Boolean = {
    dsptools.Driver.execute(() => new PacketDesSerTestModule(params), TestSetup.dspTesterOptions) { c => new PacketDesSerTester(c, inp) }
  }
}
object UIntBitsSerializerTester {
  def apply(params: UIntBitsSerDesParams, inp: IndexedSeq[Int]): Boolean = {
    dsptools.Driver.execute(() => new BitsSerializer(params), TestSetup.dspTesterOptions) { c => new BitsSerializerTester(c, inp) }
  }
}
