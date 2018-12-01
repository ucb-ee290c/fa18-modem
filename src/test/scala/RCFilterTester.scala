package modem

import breeze.math.Complex
import breeze.numerics.abs
import dsptools.DspTester

/**
  * Case class holding information needed to run an individual test
  */
case class RCIQ(
  // input iq vectors
  iqin: Seq[Complex],
  iqout: Seq[Complex]
)

/**
  * DspTester for RCFilter
  *
  * Run each trial in @trials
  */
class RCFilterTester[T <: chisel3.Data](c: RCFilter[T], trials: Seq[RCIQ], tolLSBs: Int = 2, tol: Double = 1e-6) extends DspTester(c) {
  def getIQOut(c: RCFilter[T], v: Vector[Complex]): Vector[Complex] = {
    var vout = v
    if (peek(c.io.out.valid)) {
      vout = vout :+ peek(c.io.out.bits.iq)
    }
    vout
  }

  val maxCyclesWait = 64

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 0)
  step(32)
  poke(c.io.in.valid, 1)

  for (trial <- trials) {
    println("Trial begins...")
    var iqOut = Vector[Complex]()
    for (i <- trial.iqin.indices) {
      poke(c.io.in.bits.iq(0), trial.iqin(i))
      poke(c.io.in.bits.pktStart, i==0)
      poke(c.io.in.bits.pktEnd, i==trial.iqin.length-1)
      // wait until input is accepted
      var cyclesWaiting = 0
      while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for input too long")
        }
        iqOut = getIQOut(c, iqOut)
        step(1)
      }
      iqOut = getIQOut(c, iqOut)
      step(1)
    }
    // wait for remaining output after pushing in IQ data
    poke(c.io.in.valid, 0)
    poke(c.io.in.bits.iq(0), Complex(0, 0))
    var cyclesWaiting = 0
    while (cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      iqOut = getIQOut(c, iqOut)
      step(1)
    }
    iqOut = getIQOut(c, iqOut)
    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
      if (trial.iqout.isEmpty) {
        assert(iqOut.isEmpty, "No IQ should have been passed through")
      } else {
        val iqRef = trial.iqout
        assert(iqOut.length == iqRef.length,
               s"The packet length was ${iqOut.length} but should have been ${iqRef.length}")
        iqOut.indices.foreach {
          i => assert(abs(iqOut(i) - iqRef(i)) < tol, s"iq mismatch: ref ${iqRef(i)} != ${iqOut(i)} @$i")}
      }
    }
  }
}

/**
  * Convenience function for running tests
  */
object FixedRCFilterTester {
  def apply(params: FixedRCFilterParams, trials: Seq[RCIQ]): Boolean = {
    // chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new RCFilter(params)) {
    dsptools.Driver.execute(() => new RCFilter(params), TestSetup.dspTesterOptions) {
      c => new RCFilterTester(c, trials)
    }
  }
}
