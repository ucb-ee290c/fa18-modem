package modem

import dsptools.DspTester

class ConvCodingUnitTester[T <: chisel3.Data](c: ConvCoding[T, T]) extends DspTester(c) {
  /*
  Following is for G=(7, 5)
  State | In  | Out | Next State
  00    | 0   | 00  | 00
  00    | 1   | 11  | 10

  10    | 0   | 10  | 01
  10    | 1   | 01  | 11

  01    | 0   | 11  | 00
  01    | 1   | 00  | 10

  11    | 0   | 01  | 01
  11    | 1   | 10  | 11
   */
  // currently there is 1 delay after taking input
//  poke(c.io.inReady, 0)
  poke(c.io.isHeadIn, 0)
  poke(c.io.in.valid, 0)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 0)
  poke(c.io.in.bits, 0)
  expect(c.io.isHeadOut, 0)

  step(1)
//  poke(c.io.inReady, 0)
  poke(c.io.in.valid, 0)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 0)
  poke(c.io.in.bits, 0)
  expect(c.io.isHeadOut, 0)

  step(1)
//  poke(c.io.inReady, 1)
  poke(c.io.in.valid, 1)
  poke(c.io.isHeadIn, 1)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 0)
  poke(c.io.in.bits, 1)
  expect(c.io.isHeadOut, 0)

  step(1)
  expect(c.io.out(0), 1)
  expect(c.io.out(1), 1)    // state 10
  poke(c.io.in.bits, 1)
  expect(c.io.isHeadOut, 1)

  step(1)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 1)    // state 11
  poke(c.io.in.bits, 1)

  step(1)
  expect(c.io.out(0), 1)
  expect(c.io.out(1), 0)    // state 11
  poke(c.io.in.bits, 0)

  step(1)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 1)    // state 01
  poke(c.io.in.bits, 0)

  step(1)
  expect(c.io.out(0), 1)
  expect(c.io.out(1), 1)    // state 00
  poke(c.io.in.bits, 0)

  step(1)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 0)    // state 00
  poke(c.io.in.bits, 1)

  step(1)
  expect(c.io.out(0), 1)
  expect(c.io.out(1), 1)    // state 10
  poke(c.io.in.bits, 1)

  step(1)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 1)    // state 11
  poke(c.io.in.bits, 1)

  step(1)
  expect(c.io.out(0), 1)
  expect(c.io.out(1), 0)    // state 11
  poke(c.io.in.bits, 0)

  step(1)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 1)    // state 01
  poke(c.io.in.bits, 1)
//  poke(c.io.inReady, 0)        // last input 0, 1, 1 -> p0 = 0, p1 = 1
  poke(c.io.in.valid, 0)

  step(1)
  expect(c.io.out(0), 0)
  expect(c.io.out(1), 1)    // state 10
  poke(c.io.in.bits, 0)

}

  /**
    * Convenience function for running tests
    */
object FixedConvCodingTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new ConvCoding(params)) {
      c => new ConvCodingUnitTester(c)
    }
  }
}

// ***** test case 1 : convolutional coding test (without tail-biting) *****
//poke(c.io.inReady, 0)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)
//poke(c.io.in.bits, 0)
//
//step(1)
//poke(c.io.inReady, 0)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)
//poke(c.io.in.bits, 0)
//
//step(1)
//poke(c.io.inReady, 1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 10
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 11
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 0)    // state 11
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 01
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 00
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)    // state 00
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 10
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 11
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 0)    // state 11
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 01
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)    // state 10
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 0)    // state 01
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 00
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)    // state 00
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 10
//poke(c.io.in.bits, 1)
//poke(c.io.inReady, 0)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 10
//poke(c.io.in.bits, 1)

// ***** test case 2 : convolutional coding test (with tail-biting) *****
//poke(c.io.inReady, 0)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)
//poke(c.io.in.bits, 0)
//
//step(1)
//poke(c.io.inReady, 0)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)
//poke(c.io.in.bits, 0)
//
//step(1)
//poke(c.io.inReady, 1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 10
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 11
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 0)    // state 11
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 01
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 00
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 0)    // state 00
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 1)    // state 10
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 11
//poke(c.io.in.bits, 1)
//
//step(1)
//expect(c.io.out(0), 1)
//expect(c.io.out(1), 0)    // state 11
//poke(c.io.in.bits, 0)
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 01
//poke(c.io.inReady, 0)        // last input 0, 1, 1 -> p0 = 0, p1 = 1
//
//step(1)
//expect(c.io.out(0), 0)
//expect(c.io.out(1), 1)    // state 10
//poke(c.io.in.bits, 0)