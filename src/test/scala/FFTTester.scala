package modem

import dsptools.DspTester

import breeze.math.{Complex}

/**
 * DspTester for FixedFFT
 *
 * Run each trial in @trials
 */
class FFTTester[T <: chisel3.Data](c: FFT[T], inp: Seq[Complex], out: Seq[Complex], pktStart: Boolean = true, pktEnd: Boolean = true, tolLSBs: Int = 5) extends DspTester(c) {

  val maxCyclesWait = 50
  var cyclesWaiting = 0

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  inp.zipWithIndex.foreach { case (value, index) =>
    poke(c.io.in.bits.iq(index), value)
  }
  poke(c.io.in.bits.pktStart, pktStart)
  poke(c.io.in.bits.pktEnd  , pktEnd)
  // wait until input is accepted
  cyclesWaiting = 0
  while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
    cyclesWaiting += 1
    if (cyclesWaiting >= maxCyclesWait) {
      expect(false, "waited for input too long")
    }
    step(1)
  }

  // wait until output is accepted
  cyclesWaiting = 0
  while (!peek(c.io.out.valid) && cyclesWaiting < maxCyclesWait) {
    cyclesWaiting += 1
    if (cyclesWaiting >= maxCyclesWait) {
      expect(false, "waited for output too long")
    }
    step(1)
  }
  expect(c.io.out.bits.pktStart, pktStart)
  expect(c.io.out.bits.pktEnd  , pktEnd)

  fixTolLSBs.withValue(tolLSBs) {
    // check every output where we have an expected value
    out.zipWithIndex.foreach { case (expected, index) => expect(c.io.out.bits.iq(index), expected) }
  }
}
class IFFTTester[T <: chisel3.Data](c: IFFT[T], inp: Seq[Complex], out: Seq[Complex], pktStart: Boolean = true, pktEnd: Boolean = true, tolLSBs: Int = 2) extends DspTester(c) {val maxCyclesWait = 50
  var cyclesWaiting = 0

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  inp.zipWithIndex.foreach { case (value, index) =>
    poke(c.io.in.bits.iq(index), value)
  }
  poke(c.io.in.bits.pktStart, pktStart)
  poke(c.io.in.bits.pktEnd  , pktEnd)
  // wait until input is accepted
  cyclesWaiting = 0
  while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
    cyclesWaiting += 1
    if (cyclesWaiting >= maxCyclesWait) {
      expect(false, "waited for input too long")
    }
    step(1)
  }

  // wait until output is accepted
  cyclesWaiting = 0
  while (!peek(c.io.out.valid) && cyclesWaiting < maxCyclesWait) {
    cyclesWaiting += 1
    if (cyclesWaiting >= maxCyclesWait) {
      expect(false, "waited for output too long")
    }
    step(1)
  }
  expect(c.io.out.bits.pktStart, pktStart)
  expect(c.io.out.bits.pktEnd  , pktEnd)

  fixTolLSBs.withValue(tolLSBs) {
    // check every output where we have an expected value
    out.zipWithIndex.foreach { case (expected, index) => expect(c.io.out.bits.iq(index), expected) }
  }
}

/**
 * Convenience function for running tests
 */
object FixedFFTTester {
  def apply(params: FixedFFTParams, inp: Seq[Complex], out: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new FFT(params)) {
      c => new FFTTester(c, inp, out)
    }
  }
}
object FixedIFFTTester {
  def apply(params: FixedFFTParams, inp: Seq[Complex], out: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new IFFT(params)) {
      c => new IFFTTester(c, inp, out)
    }
  }
}
