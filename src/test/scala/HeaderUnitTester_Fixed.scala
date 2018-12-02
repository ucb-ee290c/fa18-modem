package modem

import dsptools.DspTester

class HeaderUnitTester_Fixed[T <: chisel3.Data, U <: chisel3.Data](c: HeaderExtractor[T, U]) extends DspTester(c) {
    poke(c.io.isHead, 0)
    step(1)
    step(1)
    poke(c.io.headInfo.ready, 1)
    poke(c.io.isHead, 1)

    poke(c.io.in(0), 0.1)
    poke(c.io.in(1), 0.1)
    poke(c.io.in(2), 0.1)
    poke(c.io.in(3), -0.1)
    poke(c.io.in(4), 0.1)
    poke(c.io.in(5), 0.1)
    poke(c.io.in(6), -0.1)
    poke(c.io.in(7), -0.1)            // rate information
    poke(c.io.in(8), -0.1)
    poke(c.io.in(9), 0.1)             // reserved
    poke(c.io.in(10), 0.1)
    poke(c.io.in(11), -0.1)           // len 0
    poke(c.io.in(12), 0.1)
    poke(c.io.in(13), 0.1)            // len 1
    poke(c.io.in(14), 0.1)
    poke(c.io.in(15), -0.1)           // len 2
    poke(c.io.in(16), 0.1)
    poke(c.io.in(17), 0.1)            // len 3
    poke(c.io.in(18), -0.1)
    poke(c.io.in(19), -0.1)           // len 4
    poke(c.io.in(20), -0.1)
    poke(c.io.in(21), 0.1)            // len 5
    poke(c.io.in(22), 0.1)
    poke(c.io.in(23), -0.1)           // len 6
    poke(c.io.in(24), -0.1)
    poke(c.io.in(25), -0.1)           // len 7
    poke(c.io.in(26), -0.1)
    poke(c.io.in(27), -0.1)           // len 8
    poke(c.io.in(28), -0.1)
    poke(c.io.in(29), -0.1)           // len 9
    poke(c.io.in(30), -0.1)
    poke(c.io.in(31), -0.1)           // len 10
    poke(c.io.in(32), -0.1)
    poke(c.io.in(33), -0.1)           // len 11
    poke(c.io.in(34), -0.1)
    poke(c.io.in(35), -0.1)           // parity *
    poke(c.io.in(36), -0.1)
    poke(c.io.in(37), -0.1)           // tail 0
    poke(c.io.in(38), -0.1)
    poke(c.io.in(39), -0.1)           // tail 1
    poke(c.io.in(40), -0.1)
    poke(c.io.in(41), -0.1)           // tail 2
    poke(c.io.in(42), -0.1)
    poke(c.io.in(43), -0.1)           // tail 3
    poke(c.io.in(44), -0.1)
    poke(c.io.in(45), -0.1)           // tail 4
    poke(c.io.in(46), -0.1)
    poke(c.io.in(47), -0.1)           // tail 5

    step(48)

    expect(c.io.headInfo.bits.rate(0), 1)
    expect(c.io.headInfo.bits.rate(1), 0)
    expect(c.io.headInfo.bits.rate(2), 1)
    expect(c.io.headInfo.bits.rate(3), 1)
    expect(c.io.headInfo.bits.dataLen, 26*8)
}

object FixedHeaderTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new HeaderExtractor(params)) {
      c => new HeaderUnitTester_Fixed(c)
    }
  }
}
