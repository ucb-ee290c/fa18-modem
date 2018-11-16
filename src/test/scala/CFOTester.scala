package modem

import breeze.math.Complex
import dsptools.DspTester

/**
  * Case class holding information needed to run an individual test
  */
case class CFOIQ(
  // input iq vectors
  iqin: Seq[Complex],
  iqout: Option[Seq[Complex]] = None
)

/**
  * DspTester for CFO
  *
  * Run each trial in @trials
  */
class CFOEstimationTester[T <: chisel3.Data](c: CFOEstimation[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  // var outVec = Vector[Complex]()
  for(trial <- trials){
    poke(c.io.in.bits.pktStart,0)
    poke(c.io.in.bits.pktEnd, 1)
    step(1)
    poke(c.io.in.bits.pktEnd, 0)
    step(50)
    poke(c.io.in.bits.pktStart, 1)
    for(iq <- trial.iqin){
      var waitCycles = 0
      while(!peek(c.io.in.ready) && waitCycles < maxWaitCycles){
        waitCycles += 1
        if(waitCycles >= maxWaitCycles){
          expect(false, "Block input ready timed out.")
        }
        step(1)
      }
      poke(c.io.in.valid, 1)
      poke(c.io.in.bits.iq, iq)
      waitCycles = 0
      while(!peek(c.io.out.valid) && waitCycles < maxWaitCycles){
        waitCycles += 1
        if(waitCycles >= maxWaitCycles){
          expect(false, "Block output valid timed out.")
        }
        step(1)
      }
      peek(c.io.out.bits.iq)
      peek(c.io.pErr)
      // fixTolLSBs.withValue(tolLSBs){
      //  if (peek(c.io.out.valid)){
      //    //assert(peek(c.io.out.bits.iq) == iq, "Decimator should be outputting the same value as given")
      //    //val iqout = peek(c.io.out.bits.iq)
      //    expect(c.io.out.bits.iq, iq)
      //  }
      // }
      // step(1)
    }
  }
}
/**
  * Convenience function for running tests
  */
object FixedCFOEstimationTester {
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
