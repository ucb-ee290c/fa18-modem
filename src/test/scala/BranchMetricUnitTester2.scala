package modem 

import dsptools.DspTester

case class BranchMetricInOut2(
  // input sequence
  inBit0: Float,
  inBit1: Float,
  outBitSeq: Array[Array[Array[Int]]]
)

class BranchMetricUnitTester2[T <: chisel3.Data](c: BranchMetric_backup[T], trials: Seq[BranchMetricInOut2]) extends DspTester(c) {

  for(trial <- trials){
    poke(c.io.in(0), trial.inBit0)
    poke(c.io.in(1), trial.inBit1)

    for (currentStates <- 0 until 4) {
      for (currentInputs <- 0 until 2) {
//        expect(c.io.out(currentStates)(currentInputs)(0), trial.outBitSeq(currentStates)(currentInputs)(0) ^ ((trial.inBit0+1)/2))
//        expect(c.io.out(currentStates)(currentInputs)(1), trial.outBitSeq(currentStates)(currentInputs)(1) ^ ((trial.inBit1+1)/2))
//        expect(c.io.out_dec(currentStates)(currentInputs), (trial.outBitSeq(currentStates)(currentInputs)(0) ^ ((trial.inBit0+1)/2)) + (trial.outBitSeq(currentStates)(currentInputs)(1) ^ ((trial.inBit1+1)/2)))
          fixTolLSBs.withValue(1) {
            expect(c.io.out_dec(currentStates)(currentInputs), -1 * ((trial.outBitSeq(currentStates)(currentInputs)(0) * (trial.inBit0)) + (trial.outBitSeq(currentStates)(currentInputs)(1) * (trial.inBit1))))
          }
      }
    }
  }
}

object FixedBranchMetricTester2 {
  def apply(params: FixedCoding, trials: Seq[BranchMetricInOut2]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BranchMetric_backup(params)) {
      c => new BranchMetricUnitTester2(c, trials)
    }
  }
}
