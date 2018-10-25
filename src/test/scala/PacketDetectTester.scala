package modem

import breeze.math.Complex
import dsptools.DspTester

/**
  * Case class holding information needed to run an individual test
  */
case class IQ(
  // input iq vectors
  iqin: Seq[Complex],
  iqout: Option[Seq[Complex]] = None
)

/**
  * DspTester for PacketDetect
  *
  * Run each trial in @trials
  */
class PacketDetectTester[T <: chisel3.Data](c: PacketDetect[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  def getIQOut(c: PacketDetect[T], v: Vector[Complex]): Vector[Complex] = {
    var vout = v
    if (peek(c.io.out.valid)) {
      vout = vout :+ peek(c.io.out.bits.iq(0))
    }
    vout
  }

  val maxCyclesWait = 32

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  for (trial <- trials) {
    var iqOut = Vector[Complex]()
    for (iq <- trial.iqin) {
      poke(c.io.in.bits.iq, iq)
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
      peek(c.io.debug.powerHigh)
      peek(c.io.debug.powerLow)
      peek(c.io.debug.corrComp)
      peek(c.io.debug.corrNum)
      peek(c.io.debug.corrDenom)
//      peek(c.io.debug.iq)
      step(1)
    }
    // wait for remaining output after pushing in IQ data
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.iq, Complex(0, 0))
    var cyclesWaiting = 0
    while (cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      peek(c.io.debug.powerHigh)
      peek(c.io.debug.powerLow)
      peek(c.io.debug.corrComp)
      peek(c.io.debug.corrNum)
      peek(c.io.debug.corrDenom)
      //      peek(c.io.debug.iq)
      iqOut = getIQOut(c, iqOut)
      step(1)
    }
    peek(c.io.debug.powerHigh)
    peek(c.io.debug.powerLow)
    peek(c.io.debug.corrComp)
    peek(c.io.debug.corrNum)
    peek(c.io.debug.corrDenom)
    //    peek(c.io.debug.iq)
    iqOut = getIQOut(c, iqOut)
    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
      if (trial.iqout.isEmpty) {
        assert(iqOut.isEmpty, "No IQ should have been passed through")
      } else {
        val iqRef = trial.iqout.get
        assert(iqOut.length == iqRef.length,
               s"The packet length was ${iqOut.length} but should have been ${iqRef.length}")
        iqOut.indices.foreach {
         i => assert(iqOut(i) == iqRef(i), s"iq mismatch: ref ${iqRef(i)} != ${iqOut(i)} @$i")}
      }
    }
  }
}

/**
  * Convenience function for running tests
  */
object FixedPacketDetectTester {
  def apply(params: FixedPacketDetectParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new PacketDetect(params)) {
      c => new PacketDetectTester(c, trials)
    }
  }
}

object RealPacketDetectTester {
  def apply(params: PacketDetectParams[dsptools.numbers.DspReal], trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new PacketDetect(params)) {
      c => new PacketDetectTester(c, trials)
    }
  }
}
