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
class DecimatorTester[T <: chisel3.Data](c: DecimateByN[T], nDecimation: Int, trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  poke(c.io.in.valid, 1)
  poke(c.io.out.ready, 1)

  for(trial <- trials){
    var outVec = Vector[Complex]()
    for(iq <- trial.iqin){
      poke(c.io.in.bits.iq, iq)
      if (peek(c.io.out.valid) == 1){
        assert(peek(c.io.out.bits.iq) == iq, "Decimator should be outputting the same value as given")
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
      c => new DecimatorTester(c, params.nDecimation, trials)
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
