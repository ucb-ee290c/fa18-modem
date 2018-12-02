package modem

import dsptools.DspTester

class ViterbiDecoderUnitTester[T <: chisel3.Data, U <: chisel3.Data](c: ViterbiDecoder[T, U]) extends DspTester(c) {
  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 0)
  poke(c.io.in.bits.pktStart, 0)
  poke(c.io.in.bits.pktEnd, 0)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en2, 0)
  step(1)
  poke(c.io.in.valid, 1)
  poke(c.io.in.bits.pktStart, 1)
  poke(c.io.in.bits.pktEnd, 0)
  //  poke(c.io.in.bits.bits(0), 1)
  //  poke(c.io.in.bits.bits(1), 1)
  //  poke(c.io.in.bits.bits(2), 1)
  //  poke(c.io.in.bits.bits(3), -1)
  //  poke(c.io.in.bits.bits(4), 1)
  //  poke(c.io.in.bits.bits(5), 1)
  //  poke(c.io.in.bits.bits(6), -1)
  //  poke(c.io.in.bits.bits(7), -1)            // rate information
  poke(c.io.in.bits.bits(0), -1)
  poke(c.io.in.bits.bits(1), -1)
  poke(c.io.in.bits.bits(2), -1)
  poke(c.io.in.bits.bits(3), -1)
  poke(c.io.in.bits.bits(4), -1)
  poke(c.io.in.bits.bits(5), -1)
  poke(c.io.in.bits.bits(6), -1)
  poke(c.io.in.bits.bits(7), -1)             // rate information
  poke(c.io.in.bits.bits(8), -1)
  poke(c.io.in.bits.bits(9), 1)             // reserved
  poke(c.io.in.bits.bits(10), 1)
  poke(c.io.in.bits.bits(11), -1)           // len 0
  poke(c.io.in.bits.bits(12), 1)
  poke(c.io.in.bits.bits(13), 1)            // len 1
  poke(c.io.in.bits.bits(14), 1)
  poke(c.io.in.bits.bits(15), -1)           // len 2
  poke(c.io.in.bits.bits(16), 1)
  poke(c.io.in.bits.bits(17), 1)            // len 3
  poke(c.io.in.bits.bits(18), -1)
  poke(c.io.in.bits.bits(19), -1)           // len 4
  poke(c.io.in.bits.bits(20), -1)
  poke(c.io.in.bits.bits(21), 1)            // len 5
  poke(c.io.in.bits.bits(22), 1)
  poke(c.io.in.bits.bits(23), -1)           // len 6
  poke(c.io.in.bits.bits(24), -1)
  poke(c.io.in.bits.bits(25), -1)           // len 7
  poke(c.io.in.bits.bits(26), -1)
  poke(c.io.in.bits.bits(27), -1)           // len 8
  poke(c.io.in.bits.bits(28), -1)
  poke(c.io.in.bits.bits(29), -1)           // len 9
  poke(c.io.in.bits.bits(30), -1)
  poke(c.io.in.bits.bits(31), -1)           // len 10
  poke(c.io.in.bits.bits(32), -1)
  poke(c.io.in.bits.bits(33), -1)           // len 11
  poke(c.io.in.bits.bits(34), -1)
  poke(c.io.in.bits.bits(35), -1)           // parity *
  poke(c.io.in.bits.bits(36), -1)
  poke(c.io.in.bits.bits(37), -1)           // tail 0
  poke(c.io.in.bits.bits(38), -1)
  poke(c.io.in.bits.bits(39), -1)           // tail 1
  poke(c.io.in.bits.bits(40), -1)
  poke(c.io.in.bits.bits(41), -1)           // tail 2
  poke(c.io.in.bits.bits(42), -1)
  poke(c.io.in.bits.bits(43), -1)           // tail 3
  poke(c.io.in.bits.bits(44), -1)
  poke(c.io.in.bits.bits(45), -1)           // tail 4
  poke(c.io.in.bits.bits(46), -1)
  poke(c.io.in.bits.bits(47), -1)           // tail 5
  expect(c.io.out_isHead, 0)
  expect(c.io.out_pktLatch, 0)
  expect(c.io.out_lenCnt, 1)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)

  step(1)
  poke(c.io.in.valid, 0)
  poke(c.io.in.bits.pktStart, 0)
  poke(c.io.in.bits.pktEnd, 0)
  expect(c.io.out_isHead, 0)
  expect(c.io.out_pktLatch, 1)
  expect(c.io.out_lenCnt, 1)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)

  step(1) // hdr cnt 1
  expect(c.io.out_isHead, 1)
  expect(c.io.out_pktLatch, 1)
  expect(c.io.out_lenCnt, 1)
  expect(c.io.out_en1, 0)   // wrong
  expect(c.io.out_en2, 0)
  //  expect(c.io.outHead(0), 1)
  //  expect(c.io.outHead(1), 1)
  //  expect(c.io.outHead(2), 1)
  //  expect(c.io.outHead(3), -1)
  //  expect(c.io.outHead(4), 1)
  //  expect(c.io.outHead(5), 1)
  //  expect(c.io.outHead(6), -1)
  //  expect(c.io.outHead(7), -1)

  step(58)
  //  step(1) // hdr cnt 3
  expect(c.io.out_isHead, 1)
  expect(c.io.out_pktLatch, 1)
  expect(c.io.out_lenCnt, 1)
  //  expect(c.io.out_header_rate(0),1)
  //  expect(c.io.out_header_rate(1),0)
  //  expect(c.io.out_header_rate(2),1)
  //  expect(c.io.out_header_rate(3),1)
  expect(c.io.out_header_rate(0),0)
  expect(c.io.out_header_rate(1),0)
  expect(c.io.out_header_rate(2),0)
  expect(c.io.out_header_rate(3),0)
  //  expect(c.io.out_header_len,26*8)
  expect(c.io.out_header_len,200)
  expect(c.io.out_hdrEnd, 0)
  expect(c.io.out_en2, 0)

  step(4)  // hdr cnt 23
  expect(c.io.out_isHead, 1)
  expect(c.io.out_pktLatch, 1)
  expect(c.io.out_lenCnt, 1)
  expect(c.io.out_hdrEnd, 1)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)

  step(1) // hdr cnt 24
  expect(c.io.out_isHead, 0)
  expect(c.io.out_pktLatch, 1)
  expect(c.io.out_lenCnt, 0)
  expect(c.io.out_hdrEnd, 0)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)

  poke(c.io.in.valid, 1)
  poke(c.io.in.bits.bits(0), 1)
  poke(c.io.in.bits.bits(1), 1)
  poke(c.io.in.bits.bits(2), 1)
  poke(c.io.in.bits.bits(3), -1)
  poke(c.io.in.bits.bits(4), 1)
  poke(c.io.in.bits.bits(5), 1)
  poke(c.io.in.bits.bits(6), -1)
  poke(c.io.in.bits.bits(7), -1)
  poke(c.io.in.bits.bits(8), -1)  // 0
  poke(c.io.in.bits.bits(9), 1)
  poke(c.io.in.bits.bits(10), 1)  // 00
  poke(c.io.in.bits.bits(11), -1)
  poke(c.io.in.bits.bits(12), -1) // 000
  poke(c.io.in.bits.bits(13), -1)
  poke(c.io.in.bits.bits(14), -1) // 0000
  poke(c.io.in.bits.bits(15), -1)
  poke(c.io.in.bits.bits(16), -1) // 00000
  poke(c.io.in.bits.bits(17), -1)
  poke(c.io.in.bits.bits(18), -1) // 000000
  poke(c.io.in.bits.bits(19), -1)
  poke(c.io.in.bits.bits(20), 1)
  poke(c.io.in.bits.bits(21), 1)
  poke(c.io.in.bits.bits(22), 1)
  poke(c.io.in.bits.bits(23), -1)
  poke(c.io.in.bits.bits(24), 1)
  poke(c.io.in.bits.bits(25), 1)
  poke(c.io.in.bits.bits(26), -1)
  poke(c.io.in.bits.bits(27), -1)
  poke(c.io.in.bits.bits(28), -1)
  poke(c.io.in.bits.bits(29), 1)
  poke(c.io.in.bits.bits(30), 1)
  poke(c.io.in.bits.bits(31), -1)
  poke(c.io.in.bits.bits(32), -1)
  poke(c.io.in.bits.bits(33), -1)
  poke(c.io.in.bits.bits(34), -1)
  poke(c.io.in.bits.bits(35), -1)
  poke(c.io.in.bits.bits(36), -1)
  poke(c.io.in.bits.bits(37), -1)
  poke(c.io.in.bits.bits(38), -1)
  poke(c.io.in.bits.bits(39), -1)
  poke(c.io.in.bits.bits(40), -1)
  poke(c.io.in.bits.bits(41), -1)           // tail 2
  poke(c.io.in.bits.bits(42), -1)
  poke(c.io.in.bits.bits(43), -1)           // tail 3
  poke(c.io.in.bits.bits(44), -1)
  poke(c.io.in.bits.bits(45), -1)           // tail 4
  poke(c.io.in.bits.bits(46), -1)
  poke(c.io.in.bits.bits(47), -1)           // tail 5
  expect(c.io.in.ready, 1)

  step(1)   // in -> PM
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)
  expect(c.io.out.valid, 0)

  step(1)   // PM -> TB
  poke(c.io.in.valid, 0)    // bit 1
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 0)
  expect(c.io.out_sp(2), 0)
  expect(c.io.out_sp(3), 0)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 0)

  step(1)    // bit 2
  poke(c.io.in.valid, 0)
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_en1, 1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 2)
  expect(c.io.out_sp(2), 0)
  expect(c.io.out_sp(3), 3)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 3
  expect(c.io.out_en1, 1)
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 2)
  expect(c.io.out_sp(2), 0)
  expect(c.io.out_sp(3), 2)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 4
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_sp(0), 1)
  expect(c.io.out_sp(1), 3)
  expect(c.io.out_sp(2), 1)
  expect(c.io.out_sp(3), 3)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 5
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 3)
  expect(c.io.out_sp(2), 1)
  expect(c.io.out_sp(3), 2)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 6
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 3)
  expect(c.io.out_sp(2), 1)
  expect(c.io.out_sp(3), 3)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 7
  expect(c.io.out_en1, 1)
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_sp(0), 1)
  expect(c.io.out_sp(1), 2)
  expect(c.io.out_sp(2), 1)
  expect(c.io.out_sp(3), 3)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 8
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 3)
  expect(c.io.out_sp(2), 0)
  expect(c.io.out_sp(3), 2)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 9
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 3)
  expect(c.io.out_sp(2), 0)
  expect(c.io.out_sp(3), 2)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 10
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 3)
  expect(c.io.out_sp(2), 0)
  expect(c.io.out_sp(3), 2)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 11
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 12
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 13
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 14
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 15
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out.valid, 1)
  expect(c.io.out.bits(0), 1)   // bit 1~5
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 1)
  expect(c.io.out.bits(3), 1)
  expect(c.io.out.bits(4), 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 16
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 17
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 18
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)

  step(1) // bit 19
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 20
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 1)
  expect(c.io.out.bits(0), 0)   // bit 6~10
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 0)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 21
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 22
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 23
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 24
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)

  step(1)
  expect(c.io.out.valid, 1)
  expect(c.io.out.bits(0), 1)   // bit 11~15
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 1)
  expect(c.io.out.bits(3), 1)
  expect(c.io.out.bits(4), 0)
  expect(c.io.out_en1, 0)

  step(1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 0)

  step(1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 0)

  step(1)
  expect(c.io.out.valid, 0)

  step(1)
  expect(c.io.out.valid, 0)

  //  step(1)
  //  expect(c.io.out.valid, 1)
  //  expect(c.io.out.bits(0), 0)   // bit 16~20
  //  expect(c.io.out.bits(1), 0)
  //  expect(c.io.out.bits(2), 0)
  //  expect(c.io.out.bits(3), 0)
  //  expect(c.io.out.bits(4), 0)
}

object FixedViterbiDecoderTester {
  def apply(params: FixedCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new ViterbiDecoder(params)) {
      c => new ViterbiDecoderUnitTester(c)
    }
  }
}
