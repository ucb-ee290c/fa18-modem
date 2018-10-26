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
  * DspTester for CFO
  *
  * Run each trial in @trials
  */
class CFOTester[T <: chisel3.Data](c: CFOCorrection[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  for(trial <- trials){
    var iqOut = Vector[Complex]()
    for(iq <- trial.iqin){
      poke(c.io.in.bits.iq, iq)
      var cyclesWaiting = 0
      while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for input too long")
        }
        iqOut = peek(c.io.out.bits.iq)
        step(1)
      }
    }
  }
}

/**
  * Convenience function for running tests
  */
object FixedCFOCorrectionTester {
  def apply(params: FixedCFOCorrectionParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new CFOCorrection(params)) {
      c => new CFOTester(c, trials)
    }
  }
}

object RealCFOCorrectionTester {
  def apply(params: CFOCorrectionParams[dsptools.numbers.DspReal], trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CFOCorrection(params)) {
      c => new CFOTester(c, trials)
    }
  }
}
