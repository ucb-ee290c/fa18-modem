package modem

import scala.math._
import breeze.math.Complex
import breeze.numerics.{cos, sin, sqrt}
import dsptools.DspTester

/**
  * Case class holding information needed to run an individual test
  */

/**
  * DspTester for CFO
  *
  * Run each trial in @trials
  */
class PreambleTester[T <: chisel3.Data](c: PreambleAdder[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  poke(c.io.in.valid, 1)
  poke(c.io.out.ready, 1)
  for (trial <- trials){
    poke(c.io.in.bits.pktStart, 1)
    for(iq <- trial.iqin){
      poke(c.io.in.bits.pktStart, 0)
      poke(c.io.in.bits.iq(0), iq)
      peek(c.io.out.bits.iq)
      step(1)
    }
    for ( i <-0 until 320){
      peek(c.io.out.bits.iq)
      step(1)
      }
    poke(c.io.in.bits.pktEnd, 1)
  }
}

/**
  * Convenience function for running tests
  */
object FixedPreambleTester {
  def apply(params: FixedPreambleParams, trials: Seq[IQ], tolLSBs: Int=3): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new PreambleAdder(params)) {
      c => new PreambleTester(c, trials, tolLSBs)
    }
  }
}
