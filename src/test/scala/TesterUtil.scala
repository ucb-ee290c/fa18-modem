package modem

import dsptools.DspTester
import breeze.math.Complex
import chisel3._
import dsptools.numbers._

/*
 * Contains useful helper functions for testers
 */
trait HasTesterUtil[T <: Module] extends DspTester[T] {

  /*
   * Waits to see if a signal is asserted. If more than the allowed number
   * of cycles is passed while waiting, fail the test.
   */
  def wait_for_assert(signal: Bool, maxCyclesWait: Int) {
    require(maxCyclesWait > 0, "maximum number of cycles to wait must be positive")
    var cyclesWaiting = 0
    while (!peek(signal) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) { expect(false, "waited for input too long") }
      step(1)
    }
  }

  // Poke the individual elements of a Vec of DspComplex
  def poke_complex_seq[U <: Data](sig_vec: Vec[DspComplex[U]], stim_seq: Seq[Complex]) {
    stim_seq.zipWithIndex.foreach { case (value, index) => poke(sig_vec(index), value) }
  }

  // Inspect and expect the individual elements of a Vec of DspComplex
  def expect_complex_seq[U <: Data](sig_vec: Vec[DspComplex[U]], exp_seq: Seq[Complex]) {
    exp_seq.zipWithIndex.foreach { case (expected, index) => expect(sig_vec(index), expected) }
  }

}