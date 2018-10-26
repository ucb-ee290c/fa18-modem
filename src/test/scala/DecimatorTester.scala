package decimator

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
class DecimatorTester[T <: chisel3.Data](c: DecimateByN[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  val maxCyclesWait = 50
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
object FixedDecimationTester {
  def apply(params: FixedDecimationParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new DecimateByN(params)) {
      c => new DecimatorTester(c, trials)
    }
  }
}

// object RealCFOCorrectionTester {
//   def apply(params: CFOCorrectionParams[dsptools.numbers.DspReal], trials: Seq[IQ]): Boolean = {
//     chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CFOCorrection(params)) {
//       c => new CFOTester(c, trials)
//     }
//   }
// }
