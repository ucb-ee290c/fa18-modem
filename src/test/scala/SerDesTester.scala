package modem

import dsptools.DspTester
import breeze.math.Complex
import chisel3._
import dsptools.numbers._
import chisel3.util.Decoupled

trait HasTesterUtil[T <: Module] extends DspTester[T] {

  def wait_for_assert(signal: Bool, maxCyclesWait: Int) {
    require(maxCyclesWait > 0, "maximum number of cycles to wait must be positive")
    var cyclesWaiting = 0
    while (!peek(signal) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for input too long")
      }
      step(1)
    }
  }

  def poke_seq[U <: chisel3.Data](sig_vec: Vec[DspComplex[U]], stim_seq: Seq[Complex]) {
    stim_seq.zipWithIndex.foreach { case (value, index) => poke(sig_vec(index), value) }
  }

  def expect_seq[U <: chisel3.Data](sig_vec: Vec[DspComplex[U]], exp_seq: Seq[Complex]) {
    exp_seq.zipWithIndex.foreach { case (expected, index) => expect(sig_vec(index), expected) }
  }
}

/**
 * DspTester for Deserializer
 */
class DeserializerTester[T <: chisel3.Data](c: Deserializer[T], inp: Seq[Complex], tolLSBs: Int = 1) extends DspTester(c) with HasTesterUtil[Deserializer[T]] {
  val maxCyclesWait = 5
  assert(inp.length % c.params.ratio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  inp.zipWithIndex.foreach { case (value, index) =>
    poke(c.io.in.bits.iq, value)
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
        expect_seq(c.io.out.bits.iq, inp.slice(deser_idx * c.params.ratio, (deser_idx + 1) * c.params.ratio))
      }
    }
    poke(c.io.in.valid, 1)
  }
}

/**
 * DspTester for Serializer
 */
class SerializerTester[T <: chisel3.Data](c: Serializer[T], inp: Seq[Complex], tolLSBs: Int = 1) extends DspTester(c) with HasTesterUtil[Serializer[T]] {
  val maxCyclesWait = 5
  assert(inp.length % c.params.ratio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  var out_idx = 0

  for (deser_idx <- 0 until inp.length / c.params.ratio) {
    poke_seq(c.io.in.bits.iq, inp.slice(deser_idx * c.params.ratio, (deser_idx + 1) * c.params.ratio))

    poke(c.io.in.bits.pktStart, deser_idx == 0)
    poke(c.io.in.bits.pktEnd  , deser_idx == inp.length / c.params.ratio - 1)

    wait_for_assert(c.io.in.ready, maxCyclesWait)
    step(1)

    poke(c.io.in.valid, 0)
    for (i <- 0 until c.params.ratio) {
      wait_for_assert(c.io.out.valid, maxCyclesWait)
      expect(c.io.out.bits.pktStart, (out_idx == 0))
      expect(c.io.out.bits.pktEnd  , (out_idx == inp.length - 1))
      fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits.iq, inp(out_idx)) }
      out_idx += 1
      step(1)
    }
    poke(c.io.in.valid, 1)
  }
}

class DesSerTestModule[T <: Data : Real : BinaryRepresentation](val params: SerDesParams[T]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(SerialPacketBundle(params)))
    val out = Decoupled(SerialPacketBundle(params))
    val debug = Output(DeserialPacketBundle(params))
    val debug_valid = Output(Bool())
    val debug_ready = Output(Bool())
  })

  val ser = Module(new Serializer(params))
  val des = Module(new Deserializer(params))

  des.io.in <> io.in
  ser.io.in <> des.io.out
  io.out <> ser.io.out

  io.debug := des.io.out.bits
  io.debug_valid := des.io.out.valid
  io.debug_ready := des.io.out.ready
}

/**
 * DspTester for Deserializer and Serializer in series
 */
class DesSerTester[T <: chisel3.Data](c: DesSerTestModule[T], inp: Seq[Complex], tolLSBs: Int = 1) extends DspTester(c) with HasTesterUtil[DesSerTestModule[T]] {
  val maxCyclesWait = 5
  assert(inp.length % c.params.ratio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  var out_idx = 0

  inp.zipWithIndex.foreach { case (value, index) =>
    poke(c.io.in.bits.iq, value)
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
        fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits.iq, inp(out_idx)) }
        out_idx += 1
        step(1)
      }
      poke(c.io.in.valid, 1)
    }
  }
}

/**
 * Convenience function for running tests
 */
object FixedDeserializerTester {
  def apply(params: FixedSerDesParams, inp: Seq[Complex]): Boolean = {
    // chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Deserializer(params)) {
    dsptools.Driver.execute(() => new Deserializer(params), TestSetup.dspTesterOptions) {
       c => new DeserializerTester(c, inp) }
  }
}
object FixedSerializerTester {
  def apply(params: FixedSerDesParams, inp: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Serializer(params)) { c => new SerializerTester(c, inp) }
  }
}
object FixedDesSerTester {
  def apply(params: FixedSerDesParams, inp: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new DesSerTestModule(params)) { c => new DesSerTester(c, inp) }
  }
}
