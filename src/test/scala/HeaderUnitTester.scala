package modem

import dsptools.DspTester

class HeaderUnitTester[T <: chisel3.Data](c: HeaderExtractor[T]) extends DspTester(c) {
  // GP = [111, 110], rate = 1011 (3/4), length = 26, H = 24
  poke(c.io.headInfo.ready, 1)
  poke(c.io.isHead, 1)
  poke(c.io.in(0), 1)
  poke(c.io.in(1), 1)
  poke(c.io.in(2), 1)
  poke(c.io.in(3), -1)
  poke(c.io.in(4), 1)
  poke(c.io.in(5), 1)
  poke(c.io.in(6), -1)
  poke(c.io.in(7), -1)            // rate information
  poke(c.io.in(8), -1)
  poke(c.io.in(9), 1)             // reserved
  poke(c.io.in(10), 1)
  poke(c.io.in(11), -1)           // len 0
  poke(c.io.in(12), 1)
  poke(c.io.in(13), 1)            // len 1
  poke(c.io.in(14), 1)
  poke(c.io.in(15), -1)           // len 2
  poke(c.io.in(16), 1)
  poke(c.io.in(17), 1)            // len 3
  poke(c.io.in(18), -1)
  poke(c.io.in(19), -1)           // len 4
  poke(c.io.in(20), -1)
  poke(c.io.in(21), 1)            // len 5
  poke(c.io.in(22), 1)
  poke(c.io.in(23), -1)           // len 6
  poke(c.io.in(24), -1)
  poke(c.io.in(25), -1)           // len 7
  poke(c.io.in(26), -1)
  poke(c.io.in(27), -1)           // len 8
  poke(c.io.in(28), -1)
  poke(c.io.in(29), -1)           // len 9
  poke(c.io.in(30), -1)
  poke(c.io.in(31), -1)           // len 10
  poke(c.io.in(32), -1)
  poke(c.io.in(33), -1)           // len 11
  poke(c.io.in(34), -1)
  poke(c.io.in(35), -1)           // parity *
  poke(c.io.in(36), -1)
  poke(c.io.in(37), -1)           // tail 0
  poke(c.io.in(38), -1)
  poke(c.io.in(39), -1)           // tail 1
  poke(c.io.in(40), -1)
  poke(c.io.in(41), -1)           // tail 2
  poke(c.io.in(42), -1)
  poke(c.io.in(43), -1)           // tail 3
  poke(c.io.in(44), -1)
  poke(c.io.in(45), -1)           // tail 4
  poke(c.io.in(46), -1)
  poke(c.io.in(47), -1)           // tail 5
  step(1)
  step(1)
  step(1) // computation should be completed at this time
  step(1) // clk1
  step(1) // clk2
  step(1) // clk3
  expect(c.io.headInfo.bits.rate(0), 1)
  expect(c.io.headInfo.bits.rate(1), 0)
  expect(c.io.headInfo.bits.rate(2), 1)
  expect(c.io.headInfo.bits.rate(3), 1)
  expect(c.io.headInfo.bits.dataLen, 26*8)
  expect(c.io.headInfo.valid, 1)
  poke(c.io.isHead, 0)
  step(1)
  expect(c.io.headInfo.bits.rate(0), 1)
  expect(c.io.headInfo.bits.rate(1), 0)
  expect(c.io.headInfo.bits.rate(2), 1)
  expect(c.io.headInfo.bits.rate(3), 1)
  expect(c.io.headInfo.bits.dataLen, 26*8)
  expect(c.io.headInfo.valid, 0)

}

object FixedHeaderTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new HeaderExtractor(params)) {
      c => new HeaderUnitTester(c)
    }
  }
}

/* test case 1
  // H = 6
  poke(c.io.headInfo.ready, 1)
  poke(c.io.isHead, 1)
  poke(c.io.in(0), 1)
  poke(c.io.in(1), 1)
  poke(c.io.in(2), 1)
  poke(c.io.in(3), -1)
  poke(c.io.in(4), 1)
  poke(c.io.in(5), 1)
  poke(c.io.in(6), -1)
  poke(c.io.in(7), -1)
  poke(c.io.in(8), -1)
  poke(c.io.in(9), 1)
  poke(c.io.in(10), 1)
  poke(c.io.in(11), -1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
 */

/*
// GP = [111, 110], rate = 1011 (3/4), length = 26, H = 24
  poke(c.io.headInfo.ready, 1)
  poke(c.io.isHead, 1)
  poke(c.io.in(0), 1)
  poke(c.io.in(1), 1)
  poke(c.io.in(2), 1)
  poke(c.io.in(3), -1)
  poke(c.io.in(4), 1)
  poke(c.io.in(5), 1)
  poke(c.io.in(6), -1)
  poke(c.io.in(7), -1)            // rate information
  poke(c.io.in(8), -1)
  poke(c.io.in(9), 1)             // reserved
  poke(c.io.in(10), 1)
  poke(c.io.in(11), -1)           // len 0
  poke(c.io.in(12), 1)
  poke(c.io.in(13), 1)            // len 1
  poke(c.io.in(14), 1)
  poke(c.io.in(15), -1)           // len 2
  poke(c.io.in(16), 1)
  poke(c.io.in(17), 1)            // len 3
  poke(c.io.in(18), -1)
  poke(c.io.in(19), -1)           // len 4
  poke(c.io.in(20), -1)
  poke(c.io.in(21), 1)            // len 5
  poke(c.io.in(22), 1)
  poke(c.io.in(23), -1)           // len 6
  poke(c.io.in(24), -1)
  poke(c.io.in(25), -1)           // len 7
  poke(c.io.in(26), -1)
  poke(c.io.in(27), -1)           // len 8
  poke(c.io.in(28), -1)
  poke(c.io.in(29), -1)           // len 9
  poke(c.io.in(30), -1)
  poke(c.io.in(31), -1)           // len 10
  poke(c.io.in(32), -1)
  poke(c.io.in(33), -1)           // len 11
  poke(c.io.in(34), -1)
  poke(c.io.in(35), -1)           // parity *
  poke(c.io.in(36), -1)
  poke(c.io.in(37), -1)           // tail 0
  poke(c.io.in(38), -1)
  poke(c.io.in(39), -1)           // tail 1
  poke(c.io.in(40), -1)
  poke(c.io.in(41), -1)           // tail 2
  poke(c.io.in(42), -1)
  poke(c.io.in(43), -1)           // tail 3
  poke(c.io.in(44), -1)
  poke(c.io.in(45), -1)           // tail 4
  poke(c.io.in(46), -1)
  poke(c.io.in(47), -1)           // tail 5
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  expect(c.io.headInfo.bits.rate(0), 1)
  expect(c.io.headInfo.bits.rate(1), 0)
  expect(c.io.headInfo.bits.rate(2), 1)
  expect(c.io.headInfo.bits.rate(3), 1)
  expect(c.io.headInfo.bits.dataLen, 26)
  expect(c.io.headInfo.valid, 1)
 */