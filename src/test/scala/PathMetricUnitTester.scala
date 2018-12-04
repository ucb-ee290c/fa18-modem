package modem

import dsptools.DspTester

class PathMetricUnitTester[T <: chisel3.Data](c: PathMetric[T, T]) extends DspTester(c) {
//  /*
//  Following is for G=(7, 6)
//  Example can be found from http://web.mit.edu/6.02/www/f2010/handouts/lectures/L9.pdf
//    -----------------Path Metric---------------
//    [  0     1     2     3     4     5     6  ]
//    -------------------------------------------
//    |  0     2     3     2     2     3     2  |
//    | inf   inf    1     2     3     2     4  |
//    | inf    0     3     2     3     3     4  |
//    | inf   inf    1     2     2     4     4  |
//
//
//    ---------------Survivor Path---------------
//    [  0     1     2     3     4     5     6  ]
//    -------------------------------------------
//    |  x     0     0     1     0     0     1o |
//    |  x     x     2o    3     3     3o    2  |
//    |  x     0o    0     1o    1     1     1  |
//    |  x     x     2     3     2o    3     3  |
//  */
  // below works for generator polynomial : 111, 110
  poke(c.io.hdrEnd, 0)
  poke(c.io.in(0), -1)
  poke(c.io.in(1), -1)
  poke(c.io.inEnable, 0)
//  expect(c.io.outPM(0), 0)
//  expect(c.io.outPM(1), 0)
//  expect(c.io.outPM(2), 0)
//  expect(c.io.outPM(3), 0)
  expect(c.io.outSP(0), 0)
  expect(c.io.outSP(1), 0)
  expect(c.io.outSP(2), 0)
  expect(c.io.outSP(3), 0)

  step(1)                                   // startDecode becomes 1 in the next clk cycle. pmRegs(1~3) === 100.U
  poke(c.io.hdrEnd, 1)
  poke(c.io.in(0), -1)
  poke(c.io.in(1), -1)
//  expect(c.io.outPM(0), 0)
//  expect(c.io.outPM(1), 0)
//  expect(c.io.outPM(2), 0)
//  expect(c.io.outPM(3), 0)
  expect(c.io.outSP(0), 0)
  expect(c.io.outSP(1), 0)
  expect(c.io.outSP(2), 0)
  expect(c.io.outSP(3), 0)
  expect(c.io.outEnable, 0)

  step(1)                                   // startDecode is 1. PDSU starts!
  poke(c.io.hdrEnd, 0)
  poke(c.io.in(0), -1)
  poke(c.io.in(1), -1)
  expect(c.io.outPM(0), 0)
  expect(c.io.outPM(1), 16)
  expect(c.io.outPM(2), 16)
  expect(c.io.outPM(3), 16)
  expect(c.io.outSP(0), 0)
  expect(c.io.outSP(1), 0)
  expect(c.io.outSP(2), 0)
  expect(c.io.outSP(3), 0)
  expect(c.io.outEnable, 0)

  step(1)                                   // startDecode is 1. PDSU starts!
  poke(c.io.inEnable, 1)
  poke(c.io.in(0), 1)
  poke(c.io.in(1), 1)
  expect(c.io.outPM(0), 0)
  expect(c.io.outPM(1), 16)
  expect(c.io.outPM(2), 16)
  expect(c.io.outPM(3), 16)
  expect(c.io.outSP(0), 0)
  expect(c.io.outSP(1), 0)
  expect(c.io.outSP(2), 0)
  expect(c.io.outSP(3), 0)
  expect(c.io.outEnable, 0)

  step(1)
  poke(c.io.hdrEnd, 0)
  poke(c.io.in(0), 1)
  poke(c.io.in(1), -1)
//  expect(c.io.outPM(0), 2)
//  expect(c.io.outPM(1), 16)
//  expect(c.io.outPM(2), 0)
//  expect(c.io.outPM(3), 17)
  expect(c.io.outSP(0), 0)
//  expect(c.io.outSP(1), 0)              // I don't care about outSP(1) and outSP(2) at this point
  expect(c.io.outSP(2), 0)
//  expect(c.io.outSP(3), 0)              // I don't care about outSP(1) and outSP(2) at this point
  expect(c.io.outEnable, 1)

  step(1)
  poke(c.io.hdrEnd, 0)
  poke(c.io.in(0), 1)
  poke(c.io.in(1), 1)
//  expect(c.io.outPM(0), 3)
//  expect(c.io.outPM(1), 1)
//  expect(c.io.outPM(2), 3)
//  expect(c.io.outPM(3), 1)
  expect(c.io.outSP(0), 0)
  expect(c.io.outSP(1), 2)
  expect(c.io.outSP(2), 0)
  expect(c.io.outSP(3), 2)
  expect(c.io.outEnable, 1)

  step(1)
  poke(c.io.hdrEnd, 0)
  poke(c.io.in(0), -1)
  poke(c.io.in(1), -1)
//  expect(c.io.outPM(0), 2)
//  expect(c.io.outPM(1), 2)
//  expect(c.io.outPM(2), 2)
//  expect(c.io.outPM(3), 2)
  expect(c.io.outSP(0), 1)
  expect(c.io.outSP(1), 3)
  expect(c.io.outSP(2), 1)
  expect(c.io.outSP(3), 3)
  expect(c.io.outEnable, 1)

  step(1)
  poke(c.io.hdrEnd, 0)
  poke(c.io.in(0), -1)
  poke(c.io.in(1), 1)
//  expect(c.io.outPM(0), 2)
//  expect(c.io.outPM(1), 3)
//  expect(c.io.outPM(2), 3)
//  expect(c.io.outPM(3), 2)
  expect(c.io.outSP(0), 0)
  expect(c.io.outSP(1), 3)
  expect(c.io.outSP(2), 1)
  expect(c.io.outSP(3), 2)
  expect(c.io.outEnable, 1)

  step(1)
  poke(c.io.hdrEnd, 0)
  poke(c.io.in(0), 1)
  poke(c.io.in(1), -1)
//  expect(c.io.outPM(0), 3)
//  expect(c.io.outPM(1), 2)
//  expect(c.io.outPM(2), 3)
//  expect(c.io.outPM(3), 4)
  expect(c.io.outSP(0), 0)
  expect(c.io.outSP(1), 3)
  expect(c.io.outSP(2), 1)
  expect(c.io.outSP(3), 3)
  expect(c.io.outEnable, 1)

  step(1)
  poke(c.io.hdrEnd, 0)
//  poke(c.io.in(0), 1)
//  poke(c.io.in(1), 0)
//  expect(c.io.outPM(0), 2)
//  expect(c.io.outPM(1), 4)
//  expect(c.io.outPM(2), 4)
//  expect(c.io.outPM(3), 4)
  expect(c.io.outSP(0), 1)
  expect(c.io.outSP(1), 2)
  expect(c.io.outSP(2), 1)
  expect(c.io.outSP(3), 3)
  expect(c.io.outEnable, 1)

}

  /**
    * Convenience function for running tests
    */
object FixedPathMetricTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new PathMetric(params)) {
      c => new PathMetricUnitTester(c)
    }
  }
}
