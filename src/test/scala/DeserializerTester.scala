package modem

import dsptools.DspTester

import breeze.math.Complex

/**
 * DspTester for FixedDeserializer
 *
 * Run each trial in @trials
 */
class DeserializerTester[T <: chisel3.Data](c: Deserializer[T], inp: Seq[Complex], tolLSBs: Int = 1) extends DspTester(c) {
  val maxCyclesWait = 5
  assert(inp.length % c.params.deserRatio == 0, "input sequence should be a multiple of deser ratio")

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  var cyclesWaiting = 0
  
  inp.zipWithIndex.foreach { case (value, index) =>
    poke(c.io.in.bits.iq(0), value)
    poke(c.io.in.bits.pktStart, (index == 0))
    poke(c.io.in.bits.pktEnd  , (index == inp.length - 1))
    // wait until input is accepted
    while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for input too long")
      }
      step(1)
    }
    step(1)
    poke(c.io.in.valid, 0)
    if (index % c.params.deserRatio == c.params.deserRatio - 1) {
      // wait until output is accepted
      cyclesWaiting = 0
      while (!peek(c.io.out.valid) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for output too long")
        }
        step(1)
      }
      expect(c.io.out.bits.pktStart, (index < c.params.deserRatio))
      expect(c.io.out.bits.pktEnd  , (index == inp.length - 1))

      val deser_idx = index / c.params.deserRatio
      fixTolLSBs.withValue(tolLSBs) {
        inp.slice(deser_idx * c.params.deserRatio, (deser_idx + 1) * c.params.deserRatio).zipWithIndex.foreach {
          case (expected, index) => expect(c.io.out.bits.iq(index), expected)
        }
      }
    }
    poke(c.io.in.valid, 1)
  }
}

/**
 * Convenience function for running tests
 */
object FixedDeserializerTester {
  def apply(params: FixedDeserializerParams, inp: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Deserializer(params)) {
      c => new DeserializerTester(c, inp)
    }
  }
}
