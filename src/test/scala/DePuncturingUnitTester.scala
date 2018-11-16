package modem

import dsptools.DspTester

class DePuncturingUnitTester[T <: chisel3.Data](c: DePuncturing[T]) extends DspTester(c) {
  poke(c.io.in_hard(0), 1)
  poke(c.io.in_hard(1), 1)
  poke(c.io.in_hard(2), -1)
  poke(c.io.in_hard(3), -1)
  poke(c.io.in_hard(4), 1)
  poke(c.io.in_hard(5), -1)
  poke(c.io.in_hard(6), -1)
  poke(c.io.in_hard(7), 1)
  poke(c.io.in_hard(8), 1)
  poke(c.io.in_hard(9), 1)
  poke(c.io.inReady, 1)
  poke(c.io.stateIn, 0)
  expect(c.io.out_hard(0), 0)
  expect(c.io.out_hard(1), 0)
  step(1)                                   // 11, o_cnt = 2
  expect(c.io.out_hard(0), 1)
  expect(c.io.out_hard(1), 1)
  step(1)                                   // 10, o_cnt = 4
  expect(c.io.out_hard(0), -1)
  expect(c.io.out_hard(1), 0)
  step(1)                                   // 01, o_cnt = 6
  expect(c.io.out_hard(0), 0)
  expect(c.io.out_hard(1), -1)
  step(1)                                   // 11, o_cnt = 8
  expect(c.io.out_hard(0), 1)
  expect(c.io.out_hard(1), -1)
  step(1)                                   // 10, o_cnt = 0
  expect(c.io.out_hard(0), -1)
  expect(c.io.out_hard(1), 0)
  step(1)                                   // 01
  expect(c.io.out_hard(0), 0)
  expect(c.io.out_hard(1), 1)
  step(1)
  expect(c.io.out_hard(0), 1)
  expect(c.io.out_hard(1), 1)
  poke(c.io.in_hard(0), 1)
  poke(c.io.in_hard(1), 1)
  poke(c.io.in_hard(2), -1)
  poke(c.io.in_hard(3), -1)
  poke(c.io.in_hard(4), 1)
  poke(c.io.in_hard(5), -1)
  poke(c.io.in_hard(6), -1)
  poke(c.io.in_hard(7), 1)
  poke(c.io.in_hard(8), 1)
  poke(c.io.in_hard(9), 1)
  step(1)
  expect(c.io.out_hard(0), 1)
  expect(c.io.out_hard(1), 0)
  step(1)                                   // 10, o_cnt = 4
  expect(c.io.out_hard(0), 0)
  expect(c.io.out_hard(1), 1)
  step(1)                                   // 01, o_cnt = 6
  expect(c.io.out_hard(0), -1)
  expect(c.io.out_hard(1), -1)
  step(1)                                   // 11, o_cnt = 8
  expect(c.io.out_hard(0), 1)
  expect(c.io.out_hard(1), 0)
  step(1)                                   // 10, o_cnt = 0
  expect(c.io.out_hard(0), 0)
  expect(c.io.out_hard(1), -1)
  step(1)                                   // 01
  expect(c.io.out_hard(0), -1)
  expect(c.io.out_hard(1), 1)
  step(1)
  expect(c.io.out_hard(0), 1)
  expect(c.io.out_hard(1), 0)
  step(1)
  expect(c.io.out_hard(0), 0)
  expect(c.io.out_hard(1), 1)
}

  /**
    * Convenience function for running tests
    */
object FixedDePuncturingTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new DePuncturing(params)) {
      c => new DePuncturingUnitTester(c)
    }
  }
}

// Appendix:
// confirm that when puncturing is disabled, everything works fine.
// ****** puncturing disabled ******
//poke(c.io.in_hard(0), 1)
//poke(c.io.in_hard(1), 1)
//poke(c.io.in_hard(2), -1)
//poke(c.io.in_hard(3), -1)
//poke(c.io.in_hard(4), 1)
//poke(c.io.in_hard(5), -1)
//poke(c.io.in_hard(6), -1)
//poke(c.io.in_hard(7), 1)
//poke(c.io.in_hard(8), 1)
//poke(c.io.in_hard(9), 1)
//poke(c.io.inReady, 1)
//expect(c.io.out_hard(0), 0)
//expect(c.io.out_hard(1), 0)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 1)
//step(1)
//expect(c.io.out_hard(0), -1)
//expect(c.io.out_hard(1), -1)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), -1)
//step(1)
//expect(c.io.out_hard(0), -1)
//expect(c.io.out_hard(1), 1)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 1)
//poke(c.io.in_hard(0), 0)
//poke(c.io.in_hard(1), 0)
//poke(c.io.in_hard(2), 1)
//poke(c.io.in_hard(3), 1)
//poke(c.io.in_hard(4), -1)
//poke(c.io.in_hard(5), 1)
//poke(c.io.in_hard(6), 1)
//poke(c.io.in_hard(7), -1)
//poke(c.io.in_hard(8), -1)
//poke(c.io.in_hard(9), -1)
//step(1)
//expect(c.io.out_hard(0), 0)
//expect(c.io.out_hard(1), 0)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 1)
//step(1)
//expect(c.io.out_hard(0), -1)
//expect(c.io.out_hard(1), 1)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), -1)
//step(1)
//expect(c.io.out_hard(0), -1)
//expect(c.io.out_hard(1), -1)


// ****** puncturing enabled ******
//poke(c.io.in_hard(0), 1)
//poke(c.io.in_hard(1), 1)
//poke(c.io.in_hard(2), -1)
//poke(c.io.in_hard(3), -1)
//poke(c.io.in_hard(4), 1)
//poke(c.io.in_hard(5), -1)
//poke(c.io.in_hard(6), -1)
//poke(c.io.in_hard(7), 1)
//poke(c.io.in_hard(8), 1)
//poke(c.io.in_hard(9), 1)
//poke(c.io.inReady, 1)
//poke(c.io.stateIn, 0)
//expect(c.io.out_hard(0), 0)
//expect(c.io.out_hard(1), 0)
//step(1)                                   // 11, o_cnt = 2
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 1)
//step(1)                                   // 10, o_cnt = 4
//expect(c.io.out_hard(0), -1)
//expect(c.io.out_hard(1), 0)
//step(1)                                   // 01, o_cnt = 6
//expect(c.io.out_hard(0), 0)
//expect(c.io.out_hard(1), -1)
//step(1)                                   // 11, o_cnt = 8
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), -1)
//step(1)                                   // 10, o_cnt = 0
//expect(c.io.out_hard(0), -1)
//expect(c.io.out_hard(1), 0)
//step(1)                                   // 01
//expect(c.io.out_hard(0), 0)   // wrong
//expect(c.io.out_hard(1), 1)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 1)
//poke(c.io.in_hard(0), 1)
//poke(c.io.in_hard(1), 1)
//poke(c.io.in_hard(2), -1)
//poke(c.io.in_hard(3), -1)
//poke(c.io.in_hard(4), 1)
//poke(c.io.in_hard(5), -1)
//poke(c.io.in_hard(6), -1)
//poke(c.io.in_hard(7), 1)
//poke(c.io.in_hard(8), 1)
//poke(c.io.in_hard(9), 1)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 0)
//step(1)                                   // 10, o_cnt = 4
//expect(c.io.out_hard(0), 0)
//expect(c.io.out_hard(1), 1)
//step(1)                                   // 01, o_cnt = 6
//expect(c.io.out_hard(0), -1)
//expect(c.io.out_hard(1), -1)
//step(1)                                   // 11, o_cnt = 8
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 0)
//step(1)                                   // 10, o_cnt = 0
//expect(c.io.out_hard(0), 0)
//expect(c.io.out_hard(1), -1)
//step(1)                                   // 01
//expect(c.io.out_hard(0), -1)   // wrong
//expect(c.io.out_hard(1), 1)
//step(1)
//expect(c.io.out_hard(0), 1)
//expect(c.io.out_hard(1), 0)
//step(1)
//expect(c.io.out_hard(0), 0)
//expect(c.io.out_hard(1), 1)