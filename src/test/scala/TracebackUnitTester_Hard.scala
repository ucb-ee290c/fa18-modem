package modem

import dsptools.DspTester
import chisel3._

trait HasTracebackTesterUtil[T <: Module] extends DspTester[T] {
  def check_expected_if_valid[U <: chisel3.Data](sig_vec: Vec[U], expected: Seq[Seq[Int]], idx: Int, valid: Bool): Int = {
    if (peek(valid) && idx < expected.length) {
      sig_vec.zip(expected(idx)).foreach { case (sig, exp) => expect(sig, exp) }
      idx + 1
    } else {
      idx
    }
  }
}

class TracebackUnitTester_Hard[T <: chisel3.Data, U <: chisel3.Data](c: Traceback[T, U]) extends DspTester(c)
      with HasTesterUtil[Traceback[T, U]] with HasTracebackTesterUtil[Traceback[T, U]] {

  val expected_out = Seq(Seq(0, 1, 0, 1), Seq(1, 0, 0, 0), Seq(0, 0, 0, 1), Seq(0, 1, 1, 0))
  val inPMs = IndexedSeq(
    IndexedSeq(0, 3, 3, 3),      // 0
    IndexedSeq(2, 3, 0, 3),      // 2
    IndexedSeq(3, 1, 3, 1),      // 1
    IndexedSeq(2, 2, 2, 2),      // 0
    IndexedSeq(2, 3, 3, 2),      // 0
    IndexedSeq(3, 2, 3, 4),      // 1
    IndexedSeq(2, 4, 4, 4),      // 0 <-
    IndexedSeq(2, 5, 5, 5),      // 0
    IndexedSeq(2, 5, 5, 5),      // 0
    IndexedSeq(2, 5, 5, 5),      // 0
    IndexedSeq(2, 5, 5, 5),      // 0 <-
    IndexedSeq(2, 16, 0, 16),    // 2
    IndexedSeq(3, 1, 3, 1),      // 1
    IndexedSeq(2, 2, 2, 2),      // 0
    IndexedSeq(3, 3, 3, 2),      // 0 <-
    IndexedSeq(3, 2, 3, 4),      // 1
    IndexedSeq(2, 4, 4, 4),      // 0
    IndexedSeq(2, 5, 5, 5),      // 0
    IndexedSeq(2, 5, 5, 5)       // 0 <-
  ).map(_.map(BigInt(_)))
  val inSPs = IndexedSeq(
    IndexedSeq(0, 0, 0, 0),      //
    IndexedSeq(0, 3, 0, 3),      //
    IndexedSeq(0, 2, 0, 2),      //
    IndexedSeq(1, 3, 1, 3),      //
    IndexedSeq(0, 3, 1, 2),      //
    IndexedSeq(0, 3, 1, 3),      //
    IndexedSeq(1, 2, 1, 3),      // <-
    IndexedSeq(0, 3, 1, 2),      //
    IndexedSeq(0, 3, 1, 2),      //
    IndexedSeq(0, 3, 1, 2),      //
    IndexedSeq(0, 3, 1, 2),      // <-
    IndexedSeq(0, 3, 0, 3),      //
    IndexedSeq(0, 2, 0, 2),      //
    IndexedSeq(1, 3, 1, 3),      //
    IndexedSeq(0, 3, 1, 2),      // <-
    IndexedSeq(0, 3, 1, 3),      //
    IndexedSeq(1, 2, 1, 3),      //
    IndexedSeq(0, 3, 1, 2),      //
    IndexedSeq(0, 3, 1, 2)       // <-
  ).map(_.map(BigInt(_)))
  var out_idx = 0

  poke(c.io.enable, 0)
  poke(c.io.out.ready, 1)
  poke(c.io.headInfo.valid, 1)
  poke(c.io.headInfo.bits.dataLen, 16)
  step(1)
  poke(c.io.enable, 1)
  poke(c.io.headInfo.valid, 0)
  inPMs.zip(inSPs).foreach { case (inPM, inSP) => {
    poke(c.io.inPM, inPM.reverse)
    poke(c.io.inSP, inSP.reverse)
    out_idx = check_expected_if_valid(c.io.out.bits, expected_out, out_idx, c.io.out.valid)
    step(1)
  }}

  while (out_idx < expected_out.length) {
    wait_for_assert(c.io.out.valid, 50)
    c.io.out.bits.zip(expected_out(out_idx)).foreach { case (sig, exp) => expect(sig, exp) }
    out_idx += 1
    step(1)
  }

}

object HardTracebackTester {
  def apply(params: HardCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Traceback(params)) {
      c => new TracebackUnitTester_Hard(c)
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