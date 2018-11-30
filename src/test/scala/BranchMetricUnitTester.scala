package modem

import dsptools.DspTester

case class BranchMetricInOut(
  // input sequence
  inBit0: Int,
  inBit1: Int,
  outBitSeq: Array[Array[Array[Int]]]
)

class BranchMetricUnitTester[T <: chisel3.Data](c: BranchMetric[T], trials: Seq[BranchMetricInOut]) extends DspTester(c) {

  for(trial <- trials){
    poke(c.io.in(0), trial.inBit0)
    poke(c.io.in(1), trial.inBit1)

    for (currentStates <- 0 until 4) {
      for (currentInputs <- 0 until 2) {
        expect(c.io.out(currentStates)(currentInputs)(0), trial.outBitSeq(currentStates)(currentInputs)(0) ^ ((trial.inBit0+1)/2))
        expect(c.io.out(currentStates)(currentInputs)(1), trial.outBitSeq(currentStates)(currentInputs)(1) ^ ((trial.inBit1+1)/2))
        expect(c.io.out_dec(currentStates)(currentInputs), (trial.outBitSeq(currentStates)(currentInputs)(0) ^ ((trial.inBit0+1)/2)) + (trial.outBitSeq(currentStates)(currentInputs)(1) ^ ((trial.inBit1+1)/2)))
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
