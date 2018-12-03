package modem

import dsptools.DspTester

case class BranchMetricInOut2_SINT(
  // input sequence
  inBit0: Int,
  inBit1: Int,
  outBitSeq: Array[Array[Array[Int]]]
)

class BranchMetricUnitTester2_SINT[T <: chisel3.Data, U <: chisel3.Data](c: BranchMetric[T, U], trials: Seq[BranchMetricInOut2_SINT]) extends DspTester(c) {

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

object FixedBranchMetricTester2_SINT {
  def apply(params: HardCoding, trials: Seq[BranchMetricInOut2_SINT]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BranchMetric(params)) {
      c => new BranchMetricUnitTester2_SINT(c, trials)
    }
  }
}
