//package modem
//
//import dsptools.DspTester
//
//import chisel3._
//trait HasTesterUtil[T <: Module] extends DspTester[T] {
//
//  def wait_for_assert(signal: Bool, maxCyclesWait: Int) {
//    require(maxCyclesWait > 0, "maximum number of cycles to wait must be positive")
//    var cyclesWaiting = 0
//    while (!peek(signal) && cyclesWaiting < maxCyclesWait) {
//      cyclesWaiting += 1
//      if (cyclesWaiting >= maxCyclesWait) {
//        expect(false, "waited for input too long")
//      }
//      step(1)
//    }
//  }
//
//  def poke_seq[U <: chisel3.Data](sig_vec: Vec[U], stim_seq: Seq[Double]) {
//    stim_seq.zipWithIndex.foreach { case (value, index) => poke(sig_vec(index), value) }
//  }
//
//  def expect_seq[U <: chisel3.Data](sig_vec: Vec[U], exp_seq: Seq[Double]) {
//    exp_seq.zipWithIndex.foreach { case (expected, index) => expect(sig_vec(index), expected) }
//  }
//}
//
//class TracebackUnitTester_Hard[T <: chisel3.Data, U <: chisel3.Data](c: Traceback[T, U]) extends DspTester(c) with HasTesterUtil[Traceback[T, U]] {
//  def check_expected_if_valid[U <: chisel3.Data](sig_vec: Vec[U], expected: Seq[Seq[Int]], idx: Int, valid: Bool): Int = {
//    if (peek(valid) && idx < expected.length) {
//      expect_seq(sig_vec, expected(idx).map(_.toDouble))
//      idx + 1
//    } else {
//      idx
//    }
//  }
//  val expected_out = Seq(Seq(0, 1, 0, 1), Seq(1, 0, 0, 0), Seq(0, 0, 0, 1), Seq(0, 1, 1, 0))
//  val inPMs = Seq(
//    Seq(0, 3, 3, 3),      // 0
//    Seq(2, 3, 0, 3),      // 2
//    Seq(3, 1, 3, 1),      // 1
//    Seq(2, 2, 2, 2),      // 0
//    Seq(2, 3, 3, 2),      // 0
//    Seq(3, 2, 3, 4),      // 1
//    Seq(2, 4, 4, 4),      // 0 <-
//    Seq(2, 5, 5, 5),      // 0
//    Seq(2, 5, 5, 5),      // 0
//    Seq(2, 5, 5, 5),      // 0
//    Seq(2, 5, 5, 5),      // 0 <-
//    Seq(2, 16, 0, 16),  // 2
//    Seq(3, 1, 3, 1),      // 1
//    Seq(2, 2, 2, 2),      // 0
//    Seq(3, 3, 3, 2),      // 0 <-
//    Seq(3, 2, 3, 4),      // 1
//    Seq(2, 4, 4, 4),      // 0
//    Seq(2, 5, 5, 5),      // 0
//    Seq(2, 5, 5, 5)       // 0 <-
//  )
//  val inSPs = Seq(
//    Seq(0, 0, 0, 0),      //
//    Seq(0, 3, 0, 3),      //
//    Seq(0, 2, 0, 2),      //
//    Seq(1, 3, 1, 3),      //
//    Seq(0, 3, 1, 2),      //
//    Seq(0, 3, 1, 3),      //
//    Seq(1, 2, 1, 3),      // <-
//    Seq(0, 3, 1, 2),      //
//    Seq(0, 3, 1, 2),      //
//    Seq(0, 3, 1, 2),      //
//    Seq(0, 3, 1, 2),      // <-
//    Seq(0, 3, 0, 3),      //
//    Seq(0, 2, 0, 2),      //
//    Seq(1, 3, 1, 3),      //
//    Seq(0, 3, 1, 2),      // <-
//    Seq(0, 3, 1, 3),      //
//    Seq(1, 2, 1, 3),      //
//    Seq(0, 3, 1, 2),      //
//    Seq(0, 3, 1, 2)       // <-
//  )
//  var out_idx = 0
//
//  poke(c.io.enable, 0)
//  poke(c.io.out.ready, 1)
//  poke(c.io.headInfo.valid, 1)
//  poke(c.io.headInfo.bits.dataLen, 16)
//  step(1)
//  poke(c.io.enable, 1)
//  poke(c.io.headInfo.valid, 0)
//  inPMs.zip(inSPs).foreach { case (inPM, inSP) => {
//    poke_seq(c.io.inPM, inPM.map(_.toDouble))
//    poke_seq(c.io.inSP, inSP.map(_.toDouble))
//    out_idx = check_expected_if_valid(c.io.out.bits, expected_out, out_idx, c.io.out.valid)
//    step(1)
//  }}
//
//  while (out_idx < expected_out.length) {
//    wait_for_assert(c.io.out.valid, 50)
//    expect_seq(c.io.out.bits, expected_out(out_idx).map(_.toDouble))
//    out_idx += 1
//    step(1)
//  }
//
//}
//
//object HardTracebackTester {
//  def apply(params: HardCoding): Boolean = {
//    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Traceback(params)) {
//      c => new TracebackUnitTester_Hard(c)
//    }
//  }
//}
//
///*
//// D = 5, L = 2
//  poke(c.io.enable, 1)
//  poke(c.io.out.ready, 1)
//  // addr = 0
//  poke(c.io.inPM(0), 0)
//  poke(c.io.inPM(1), 3)
//  poke(c.io.inPM(2), 3)
//  poke(c.io.inPM(3), 3)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 0)
//  poke(c.io.inSP(2), 0)
//  poke(c.io.inSP(3), 0)   // addr+3
//  step(1)
//  // addr = nState * 1
//  poke(c.io.inPM(0), 2)
//  poke(c.io.inPM(1), 3)
//  poke(c.io.inPM(2), 0)
//  poke(c.io.inPM(3), 3)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 3)
//  poke(c.io.inSP(2), 0)
//  poke(c.io.inSP(3), 3)  // addr+7
//  step(1)
//  // addr = nState * 2
//  poke(c.io.inPM(0), 3)
//  poke(c.io.inPM(1), 1)
//  poke(c.io.inPM(2), 3)
//  poke(c.io.inPM(3), 1)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 2)
//  poke(c.io.inSP(2), 0)
//  poke(c.io.inSP(3), 2)   // addr+11
//  step(1)
//  // addr = nState * 3
//  poke(c.io.inPM(0), 2)
//  poke(c.io.inPM(1), 2)
//  poke(c.io.inPM(2), 2)
//  poke(c.io.inPM(3), 2)
//  poke(c.io.inSP(0), 1)
//  poke(c.io.inSP(1), 3)
//  poke(c.io.inSP(2), 1)
//  poke(c.io.inSP(3), 3)   // addr+15
//  step(1)
//  // addr = nState * 4
//  poke(c.io.inPM(0), 2)
//  poke(c.io.inPM(1), 3)
//  poke(c.io.inPM(2), 3)
//  poke(c.io.inPM(3), 2)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 3)
//  poke(c.io.inSP(2), 1)
//  poke(c.io.inSP(3), 2)   // addr+19
//  step(1)
//  // addr = nState * 5
//  poke(c.io.inPM(0), 3)
//  poke(c.io.inPM(1), 2)
//  poke(c.io.inPM(2), 3)
//  poke(c.io.inPM(3), 4)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 3)
//  poke(c.io.inSP(2), 1)
//  poke(c.io.inSP(3), 3)   // addr+23
//  step(1)
//  // addr = nState * 6
//  poke(c.io.inPM(0), 2)
//  poke(c.io.inPM(1), 4)
//  poke(c.io.inPM(2), 4)
//  poke(c.io.inPM(3), 4)
//  poke(c.io.inSP(0), 1)
//  poke(c.io.inSP(1), 2)
//  poke(c.io.inSP(2), 1)
//  poke(c.io.inSP(3), 3)   // addr+27
//  expect(c.io.out.valid, 0)
//  step(1)
//  // addr = nState * 7
//  poke(c.io.inPM(0), 2)
//  poke(c.io.inPM(1), 5)
//  poke(c.io.inPM(2), 5)
//  poke(c.io.inPM(3), 4)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 3)
//  poke(c.io.inSP(2), 1)
//  poke(c.io.inSP(3), 2)   // addr+31
//  step(1)
//  // addr = nState * 8
//  poke(c.io.inPM(0), 2)
//  poke(c.io.inPM(1), 6)
//  poke(c.io.inPM(2), 6)
//  poke(c.io.inPM(3), 4)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 3)
//  poke(c.io.inSP(2), 1)
//  poke(c.io.inSP(3), 2)   // addr+35
//  step(1)
//  // addr = nState * 9
//  poke(c.io.inPM(0), 2)
//  poke(c.io.inPM(1), 7)
//  poke(c.io.inPM(2), 7)
//  poke(c.io.inPM(3), 4)
//  poke(c.io.inSP(0), 0)
//  poke(c.io.inSP(1), 3)
//  poke(c.io.inSP(2), 1)
//  poke(c.io.inSP(3), 2)   // addr+35
//  expect(c.io.out.valid, 0)
//  step(1)                         // daa available in 'decodeReg'
//  // addr = nState * 10 = nStates* (5 + 2 + 3)
//  expect(c.io.out.bits(0), 0)
//  expect(c.io.out.bits(1), 1)
//  expect(c.io.out.bits(2), 0)
//  expect(c.io.out.bits(3), 1)
//  expect(c.io.out.bits(4), 1)
//  expect(c.io.out.valid, 1)
//  step(1)
//  expect(c.io.out.valid, 0)
// */