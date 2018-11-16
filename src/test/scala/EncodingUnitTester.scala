package modem

import dsptools.DspTester

case class EncodingInOut(
  // input sequence
  inBit: Int,
  stateIn: Int,
  // optional outputs
  // if None, then don't check the result
  // if Some(...), check that the result matches
  outBitSeq: Option[List[Int]] = None,

)

class EncodingUnitTester[T <: chisel3.Data](c: Encoding[T], trials: Seq[EncodingInOut]) extends DspTester(c) {
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
  poke(c.io.in.valid, 1)
  poke(c.io.out.ready, 1)
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 1)

  step(1) // state: 00
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 0)
  expect(c.io.out.bits(5), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 1)

  step(1) // state: 10
  expect(c.io.out.bits(0), 1)
  expect(c.io.out.bits(1), 1) // <- output: '11', next state: 10
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 0)
  expect(c.io.out.bits(5), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 1)

  step(1) // state: 11
  expect(c.io.out.bits(0), 1)
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 0) // <- output: '0'1, next state: 11
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 0)
  expect(c.io.out.bits(5), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 1)

  step(1) // state: 10
  expect(c.io.out.bits(0), 1)
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0) // <- output: 1'0', next state: 11
  expect(c.io.out.bits(4), 0)
  expect(c.io.out.bits(5), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 0)

  step(1) // state: 10
  expect(c.io.out.bits(0), 1)
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 1)
  expect(c.io.out.bits(5), 0) // <- output: '10', next state: 11
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 0)

  step(1) // state: 10
  expect(c.io.out.bits(0), 0) // <- output: '0'1, next state: 01
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 1)
  expect(c.io.out.bits(5), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 1)

  step(1) // state: 10
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 1) // <- output: '0'1, next state: 00
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 1)
  expect(c.io.out.bits(5), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 1)

  step(1) // state: 10
  expect(c.io.out.bits(0), 0)
  expect(c.io.out.bits(1), 1)
  expect(c.io.out.bits(2), 1)
  expect(c.io.out.bits(3), 1) // <- output: '11', next state: 10
  expect(c.io.out.bits(4), 1)
  expect(c.io.out.bits(5), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.in.ready, 1)
  poke(c.io.in.bits, 1)
}

  /**
    * Convenience function for running tests
    */
object FixedEncodingTester {
  def apply(params: FixedCoding, trials: Seq[EncodingInOut]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Encoding(params)) {
      c => new EncodingUnitTester(c, trials)
    }
  }
}

// puncturing = disabled
// currently there is 1 delay after taking input
//poke(c.io.in.valid, 1)
//poke(c.io.out.ready, 1)
//expect(c.io.out.bits(0), 0)
//expect(c.io.out.bits(1), 0)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 1)
//
//step(1) // state: 00
//expect(c.io.out.bits(0), 0)
//expect(c.io.out.bits(1), 0)
//expect(c.io.out.bits(2), 0)
//expect(c.io.out.bits(3), 0)
//expect(c.io.out.bits(4), 0)
//expect(c.io.out.bits(5), 0)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 1)
//
//step(1) // state: 10
//expect(c.io.out.bits(0), 1)
//expect(c.io.out.bits(1), 1)
//expect(c.io.out.bits(2), 0)
//expect(c.io.out.bits(3), 0)
//expect(c.io.out.bits(4), 0)
//expect(c.io.out.bits(5), 0)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 1)
//
//step(1) // state: 11
//expect(c.io.out.bits(0), 1)
//expect(c.io.out.bits(1), 1)
//expect(c.io.out.bits(2), 0)
//expect(c.io.out.bits(3), 1)
//expect(c.io.out.bits(4), 0)
//expect(c.io.out.bits(5), 0)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 1)
//
//step(1) // state: 11
//expect(c.io.out.bits(0), 1)
//expect(c.io.out.bits(1), 1)
//expect(c.io.out.bits(2), 0)
//expect(c.io.out.bits(3), 1)
//expect(c.io.out.bits(4), 1)
//expect(c.io.out.bits(5), 0)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 0)
//
//step(1) // state: 11
//expect(c.io.out.bits(0), 1)
//expect(c.io.out.bits(1), 0)
//expect(c.io.out.bits(2), 0)
//expect(c.io.out.bits(3), 1)
//expect(c.io.out.bits(4), 1)
//expect(c.io.out.bits(5), 0)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 0)
//
//step(1) // state: 11
//expect(c.io.out.bits(0), 1)
//expect(c.io.out.bits(1), 0)
//expect(c.io.out.bits(2), 0)
//expect(c.io.out.bits(3), 1)
//expect(c.io.out.bits(4), 1)
//expect(c.io.out.bits(5), 0)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 0)
//
//step(1) // state: 11
//expect(c.io.out.bits(0), 1)
//expect(c.io.out.bits(1), 0)
//expect(c.io.out.bits(2), 0)
//expect(c.io.out.bits(3), 1)
//expect(c.io.out.bits(4), 1)
//expect(c.io.out.bits(5), 1)
//expect(c.io.out.valid, 0)
//expect(c.io.in.ready, 1)
//poke(c.io.in.bits, 1)