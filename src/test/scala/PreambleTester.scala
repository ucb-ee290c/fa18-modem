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
class PreambleTester[T <: chisel3.Data](c: PreambleAdder[T], trials: Seq[IQ], tfVec: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  poke(c.io.in.valid, 1)
  poke(c.io.out.ready, 1)
  val tries = trials zip tfVec
  for ((trial, tf) <- tries){
    poke(c.io.in.bits.pktStart, 1)
    val attempt = trial.iqin zip tf.iqin
    for((iq, tfiq) <- attempt){
      poke(c.io.in.bits.iq(0), iq)
      peek(c.io.out.bits.iq(0))
      step(1)
      fixTolLSBs.withValue(tolLSBs){
        expect(c.io.out.bits.iq(0), tfiq)
      }
      poke(c.io.in.bits.pktStart, 0)
    }
    for ( i <-0 until 320){
      peek(c.io.out.bits.iq)
      step(1)
      fixTolLSBs.withValue(tolLSBs){
        expect(c.io.out.bits.iq(0), trials(0).iqin(i))
      }
      //step(1)
      }
    poke(c.io.in.bits.pktEnd, 1)
  }
}

/**
  * Convenience function for running tests
  */
object FixedPreambleTester {
  def apply(params: FixedPreambleParams, trials: Seq[IQ], tfVec: Seq[IQ], tolLSBs: Int=3): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new PreambleAdder(params)) {
      c => new PreambleTester(c, trials, tfVec, tolLSBs)
    }
  }
}
