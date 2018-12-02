package modem

import dsptools.DspTester

class ArbiterUnitTester[T <: chisel3.Data](c: Arbiter[T, T]) extends DspTester(c) {
  poke(c.io.lenCnt, 0)
  poke(c.io.hdrPktLatch, 0)
  expect(c.io.isHead, 0)
  expect(c.io.hdrEnd, 0)

  step(1)
  poke(c.io.hdrPktLatch, 1)
  poke(c.io.lenCnt, 1)
//  poke(c.io.inHead(0), 1)
//  poke(c.io.inHead(1), 1)
//  poke(c.io.inHead(2), 0)
//  poke(c.io.inHead(3), 1)
//  poke(c.io.inHead(4), 0)
//  poke(c.io.inHead(5), 1)
//  poke(c.io.inHead(6), 1)
//  poke(c.io.inHead(7), 0)
  expect(c.io.isHead, 0)
  expect(c.io.hdrEnd, 0)

  step(1)
  expect(c.io.isHead, 1)
  expect(c.io.hdrEnd, 0)

  step(1)
  expect(c.io.isHead, 1)
  expect(c.io.hdrEnd, 0)

  step(1)
  expect(c.io.isHead, 1)
  expect(c.io.hdrEnd, 1)

  step(1)
  expect(c.io.isHead, 0)
  expect(c.io.hdrEnd, 0)
  poke(c.io.lenCnt, 0)

  step(1)
  expect(c.io.isHead, 0)
  expect(c.io.hdrEnd, 0)

  step(1)
  expect(c.io.isHead, 0)
  expect(c.io.hdrEnd, 0)
}

object FixedArbiterTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Arbiter(params)) {
      c => new ArbiterUnitTester(c)
    }
  }
}
