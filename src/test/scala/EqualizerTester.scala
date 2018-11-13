package modem

import breeze.math.Complex
import dsptools.DspTester

/**
  * Case class holding information needed to run an individual test
  */
case class IQWide(
  // input iq vectors
  iqin: Seq[Seq[Complex]],
  iqout: Option[Seq[Seq[Complex]]] = None
)
case class IQNarrow(
  // input iq vectors
  iqin: Seq[Complex],
  iqout: Option[Seq[Complex]] = None
)

/**
  * DspTester for Equalizer
  *
  * Run each trial in @trials
  */
class EqualizerTester[T <: chisel3.Data](c: Equalizer[T], trials: Seq[IQWide], tolLSBs: Int = 2) extends DspTester(c) {
  def peekIQ(c: Equalizer[T], v: Vector[Vector[Complex]]): Vector[Vector[Complex]] = {
    var vout = v
    if (peek(c.io.out.valid)) {
      var tmp = Vector[Complex]()
      for (i <- 0 until 64) {
        tmp = tmp :+ peek(c.io.out.bits.iq(i))
      }
      vout = vout :+ tmp
    }
    vout
  }

  def pokeIQ(c: Equalizer[T], v: Seq[Complex], pktStart: Boolean, pktEnd: Boolean): Unit = {
    for (i <- 0 until 64) {
      poke(c.io.in.bits.iq(i), v(i))
    }
    poke(c.io.in.bits.pktStart, pktStart)
    poke(c.io.in.bits.pktEnd, pktEnd)
  }

  val maxCyclesWait = 2 * c.nLTFCarriers

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  for (trial <- trials) {
    var iqOut = Vector[Vector[Complex]]()
    var nIQWritten = 0
    for (iq <- trial.iqin) {
      pokeIQ(c, iq, nIQWritten == 0, nIQWritten == trial.iqin.length - 1)
      nIQWritten = nIQWritten + 1
      // wait until input is accepted
      var cyclesWaiting = 0
      while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for input too long")
        }
        iqOut = peekIQ(c, iqOut)
        if (iqOut.length == 1) {
          assert(peek(c.io.out.bits.pktStart) == true, "pktStart was not asserted")
        }
        step(1)
      }
      iqOut = peekIQ(c, iqOut)
      step(1)
    }
    // wait for remaining output after pushing in IQ data
    iqOut = peekIQ(c, iqOut)
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
  * DspTester for ChannelInverter
  *
  * Run each trial in @trials
  */
class ChannelInverterTester[T <: chisel3.Data](c: ChannelInverter[T], trials: Seq[IQNarrow], tolLSBs: Int = 2) extends DspTester(c) {
  def peekIQ(c: ChannelInverter[T], v: Vector[Complex]): Vector[Complex] = {
    var vout = v
    if (peek(c.io.out.valid)) {
      vout = vout :+ peek(c.io.out.bits.iq)
    }
    vout
  }

  val maxCyclesWait = 20

  for (trial <- trials) {
    var iqOut = Vector[Complex]()
    var nIQWritten = 0
    poke(c.io.in.valid, 1)
    for (iq <- trial.iqin) {
      poke(c.io.in.bits.iq, iq)
      iqOut = peekIQ(c, iqOut)
      step(1)
    }
    // wait for remaining output after pushing in IQ data
    poke(c.io.in.valid, 0)
    var cyclesWaiting = 0
    while(cyclesWaiting < maxCyclesWait) {
      iqOut = peekIQ(c, iqOut)
      step(1)
      cyclesWaiting += 1
    }
    // compare output
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
object FixedEqualizerTester {
  def apply(params: FixedEqualizerParams, trials: Seq[IQWide]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Equalizer(params)) {
      c => new EqualizerTester(c, trials)
    }
  }
}

object RealEqualizerTester {
  def apply(params: EqualizerParams[dsptools.numbers.DspReal], trials: Seq[IQWide]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new Equalizer(params)) {
      c => new EqualizerTester(c, trials)
    }
  }
}

object ChannelInverterTester {
  def apply(params: FixedEqualizerParams, trials: Seq[IQNarrow]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new ChannelInverter(params)) {
      c => new ChannelInverterTester(c, trials)
    }
  }
}
