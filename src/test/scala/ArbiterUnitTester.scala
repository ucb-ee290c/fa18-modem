package modem

import dsptools.DspTester

class ArbiterUnitTester[T <: chisel3.Data](c: Arbiter[T]) extends DspTester(c) {

}

object FixedArbiterTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Arbiter(params)) {
      c => new ArbiterUnitTester(c)
    }
  }
}
