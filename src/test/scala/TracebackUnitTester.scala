package modem

import dsptools.DspTester

class TracebackUnitTester[T <: chisel3.Data, U <: chisel3.Data](c: Traceback[T, U]) extends DspTester(c) {
  poke(c.io.enable, 1)
  poke(c.io.out.ready, 1)
  // addr = 0
  poke(c.io.inPM(0), 0)
  poke(c.io.inPM(1), 3)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 3)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 0)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 0)   // addr+3 -> 0
  step(1)
  // addr = nState * 1
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 3)
  poke(c.io.inPM(2), 0)
  poke(c.io.inPM(3), 3)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 3)  // addr+7 -> 1
  step(1)
  // addr = nState * 2
  poke(c.io.inPM(0), 3)
  poke(c.io.inPM(1), 1)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 1)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 2)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 2)   // addr+11 -> 0
  step(1)
  // addr = nState * 3
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 2)
  poke(c.io.inPM(2), 2)
  poke(c.io.inPM(3), 2)
  poke(c.io.inSP(0), 1)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+15 -> 1 **
  step(1)
  // addr = nState * 4
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 3)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 2)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+19 -> 1
  step(1)
  // addr = nState * 5
  poke(c.io.inPM(0), 3)
  poke(c.io.inPM(1), 2)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+23 -> 0
  step(1)
  // addr = nState * 6
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 4)
  poke(c.io.inPM(2), 4)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 1)
  poke(c.io.inSP(1), 2)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+27 -> 0
  step(1)
  // addr = nState * 7
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 5)
  poke(c.io.inPM(2), 5)
  poke(c.io.inPM(3), 5)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+31 -> 0 **
  step(1)
  // addr = nState * 8
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 5)
  poke(c.io.inPM(2), 5)
  poke(c.io.inPM(3), 5)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+35 -> 0
  step(1)
  // addr = nState * 9
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 5)
  poke(c.io.inPM(2), 5)
  poke(c.io.inPM(3), 5)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+35 -> 0
  expect(c.io.out.valid, 0)
  step(1)                         // daa available in 'decodeReg'
  // addr = nState * 10
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 5)
  poke(c.io.inPM(2), 5)
  poke(c.io.inPM(3), 5)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)  // addr+7 -> 0
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 1)
  expect(c.io.out.valid, 1)
  step(1)
  // addr = nState * 11
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 16)
  poke(c.io.inPM(2), 0)
  poke(c.io.inPM(3), 16)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 3)  // addr+7 -> 1 **
  expect(c.io.out.valid, 0)
  step(1)
  // addr = nState * 12
  poke(c.io.inPM(0), 3)
  poke(c.io.inPM(1), 1)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 1)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 2)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 2)   // addr+11 -> 0
  expect(c.io.out.valid, 0)
  step(1)
  // addr = nState * 13
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 2)
  poke(c.io.inPM(2), 2)
  poke(c.io.inPM(3), 2)
  poke(c.io.inSP(0), 1)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+15 -> 1
  expect(c.io.out.valid, 0)
  step(1)
  // addr = nState * 14
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 3)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 2)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+19 -> 1
  expect(c.io.out.bits(0), 1)
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.valid, 1)
  step(1)
  // addr = nState * 15
  poke(c.io.inPM(0), 3)
  poke(c.io.inPM(1), 2)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+23 -> 0 **
  expect(c.io.out.valid, 0)
  step(1)
  // addr = nState * 16
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 4)
  poke(c.io.inPM(2), 4)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 1)
  poke(c.io.inSP(1), 2)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+27
  expect(c.io.out.valid, 0)
  step(1)
  // addr = nState * 17
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 5)
  poke(c.io.inPM(2), 5)
  poke(c.io.inPM(3), 5)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+31
  expect(c.io.out.valid, 0)
  step(1)
  // addr = nState * 18
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 5)
  poke(c.io.inPM(2), 5)
  poke(c.io.inPM(3), 5)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+31
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 1)
  expect(c.io.out.valid, 1)
  step(1)
  expect(c.io.out.valid, 0)
  step(1)
  expect(c.io.out.valid, 0)
  step(1)
  expect(c.io.out.valid, 0)
  step(1)
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 1)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.valid, 1)

}

object FixedTracebackTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Traceback(params)) {
      c => new TracebackUnitTester(c)
    }
  }
}

/*
// D = 5, L = 2
  poke(c.io.enable, 1)
  poke(c.io.out.ready, 1)
  // addr = 0
  poke(c.io.inPM(0), 0)
  poke(c.io.inPM(1), 3)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 3)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 0)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 0)   // addr+3
  step(1)
  // addr = nState * 1
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 3)
  poke(c.io.inPM(2), 0)
  poke(c.io.inPM(3), 3)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 3)  // addr+7
  step(1)
  // addr = nState * 2
  poke(c.io.inPM(0), 3)
  poke(c.io.inPM(1), 1)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 1)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 2)
  poke(c.io.inSP(2), 0)
  poke(c.io.inSP(3), 2)   // addr+11
  step(1)
  // addr = nState * 3
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 2)
  poke(c.io.inPM(2), 2)
  poke(c.io.inPM(3), 2)
  poke(c.io.inSP(0), 1)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+15
  step(1)
  // addr = nState * 4
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 3)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 2)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+19
  step(1)
  // addr = nState * 5
  poke(c.io.inPM(0), 3)
  poke(c.io.inPM(1), 2)
  poke(c.io.inPM(2), 3)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+23
  step(1)
  // addr = nState * 6
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 4)
  poke(c.io.inPM(2), 4)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 1)
  poke(c.io.inSP(1), 2)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 3)   // addr+27
  expect(c.io.out.valid, 0)
  step(1)
  // addr = nState * 7
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 5)
  poke(c.io.inPM(2), 5)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+31
  step(1)
  // addr = nState * 8
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 6)
  poke(c.io.inPM(2), 6)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+35
  step(1)
  // addr = nState * 9
  poke(c.io.inPM(0), 2)
  poke(c.io.inPM(1), 7)
  poke(c.io.inPM(2), 7)
  poke(c.io.inPM(3), 4)
  poke(c.io.inSP(0), 0)
  poke(c.io.inSP(1), 3)
  poke(c.io.inSP(2), 1)
  poke(c.io.inSP(3), 2)   // addr+35
  expect(c.io.out.valid, 0)
  step(1)                         // daa available in 'decodeReg'
  // addr = nState * 10 = nStates* (5 + 2 + 3)
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 1)
  expect(c.io.out.bits(4), 1)
  expect(c.io.out.valid, 1)
  step(1)
  expect(c.io.out.valid, 0)
 */