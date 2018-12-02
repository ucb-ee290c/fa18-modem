package modem

import dsptools.DspTester

case class BranchMetricInOut(
  // input sequence
  inBit0: Float,
  inBit1: Float,
  outBitSeq: Array[Array[Array[Int]]]
)

class BranchMetricUnitTester[T <: chisel3.Data, U <: chisel3.Data](c: BranchMetric[T, U], trials: Seq[BranchMetricInOut]) extends DspTester(c) {

  for(trial <- trials){
    poke(c.io.in(0), trial.inBit0)
    poke(c.io.in(1), trial.inBit1)

    for (currentStates <- 0 until 4) {
      for (currentInputs <- 0 until 2) {
        fixTolLSBs.withValue(1) {
          expect(c.io.out_dec(currentStates)(currentInputs), -1 * ((trial.outBitSeq(currentStates)(currentInputs)(0) * (trial.inBit0)) + (trial.outBitSeq(currentStates)(currentInputs)(1) * (trial.inBit1))))
        }
      }
    }
  }
}

object FixedBranchMetricTester {
  def apply(params: FixedCoding, trials: Seq[BranchMetricInOut]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BranchMetric(params)) {
      c => new BranchMetricUnitTester(c, trials)
    }
  }
}
