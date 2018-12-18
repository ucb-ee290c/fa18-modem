package modem

import dsptools.DspTester

class ViterbiDecoderUnitTester_Hard[T <: chisel3.Data, U <: chisel3.Data](c: ViterbiDecoder[T, U]) extends DspTester(c) {
  poke(c.io.out.ready, 1)
  poke(c.io.restOut.ready, 1)
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
  poke(c.io.in.bits.bits(0), -1)      // rate (1st)
  poke(c.io.in.bits.bits(1), -1)
  poke(c.io.in.bits.bits(2), -1)      // rate (2nd)
  poke(c.io.in.bits.bits(3), -1)
  poke(c.io.in.bits.bits(4), -1)      // rate (3rd)
  poke(c.io.in.bits.bits(5), -1)
  poke(c.io.in.bits.bits(6), -1)      // rate (4th)
  poke(c.io.in.bits.bits(7), -1)
  poke(c.io.in.bits.bits(8), -1)      // Reserved
  poke(c.io.in.bits.bits(9), -1)
//  poke(c.io.in.bits.bits(9), 1)             // reserved
//  poke(c.io.in.bits.bits(10), 1)
//  poke(c.io.in.bits.bits(11), -1)           // len 0
//  poke(c.io.in.bits.bits(12), 1)
//  poke(c.io.in.bits.bits(13), 1)            // len 1
//  poke(c.io.in.bits.bits(14), 1)
//  poke(c.io.in.bits.bits(15), -1)           // len 2
//  poke(c.io.in.bits.bits(16), 1)
//  poke(c.io.in.bits.bits(17), 1)            // len 3
//  poke(c.io.in.bits.bits(18), -1)
//  poke(c.io.in.bits.bits(19), -1)           // len 4
//  poke(c.io.in.bits.bits(20), -1)
//  poke(c.io.in.bits.bits(21), 1)            // len 5
//  poke(c.io.in.bits.bits(22), 1)
  poke(c.io.in.bits.bits(10), 1)     // len 1
  poke(c.io.in.bits.bits(11), 1)
  poke(c.io.in.bits.bits(12), -1)     // len 2
  poke(c.io.in.bits.bits(13), -1)
  poke(c.io.in.bits.bits(14), -1)     // len 3
  poke(c.io.in.bits.bits(15), 1)
  poke(c.io.in.bits.bits(16), 1)      // len 4
  poke(c.io.in.bits.bits(17), -1)
  poke(c.io.in.bits.bits(18), -1)      // len 5
  poke(c.io.in.bits.bits(19), -1)
  poke(c.io.in.bits.bits(20), -1)     // len 6
  poke(c.io.in.bits.bits(21), -1)
  poke(c.io.in.bits.bits(22), -1)     // len 7
  poke(c.io.in.bits.bits(23), -1)
  poke(c.io.in.bits.bits(24), -1)     // len 8
  poke(c.io.in.bits.bits(25), -1)
  poke(c.io.in.bits.bits(26), -1)     // len 9
  poke(c.io.in.bits.bits(27), -1)
  poke(c.io.in.bits.bits(28), -1)     // len 10
  poke(c.io.in.bits.bits(29), -1)
  poke(c.io.in.bits.bits(30), -1)     // len 11
  poke(c.io.in.bits.bits(31), -1)
  poke(c.io.in.bits.bits(32), -1)     // len 12
  poke(c.io.in.bits.bits(33), -1)
  poke(c.io.in.bits.bits(34), -1)     // Parity
  poke(c.io.in.bits.bits(35), -1)
  poke(c.io.in.bits.bits(36), -1)     // zero-flush 1
  poke(c.io.in.bits.bits(37), -1)
  poke(c.io.in.bits.bits(38), -1)     // zero-flush 2
  poke(c.io.in.bits.bits(39), -1)
  poke(c.io.in.bits.bits(40), -1)     // zero-flush 3
  poke(c.io.in.bits.bits(41), -1)
  poke(c.io.in.bits.bits(42), -1)     // zero-flush 4
  poke(c.io.in.bits.bits(43), -1)
  poke(c.io.in.bits.bits(44), -1)     // zero-flush 5
  poke(c.io.in.bits.bits(45), -1)
  poke(c.io.in.bits.bits(46), -1)     // zero-flush 6
  poke(c.io.in.bits.bits(47), -1)
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
//  expect(c.io.out_header_len,200)
  expect(c.io.out_header_len,24)
  expect(c.io.out_hdrEnd, 0)
  expect(c.io.out_en2, 0)

  step(4)  // hdr cnt 23
  expect(c.io.out_isHead, 1)
  expect(c.io.out_pktLatch, 1)
  expect(c.io.out_lenCnt, 1)
  expect(c.io.out_hdrEnd, 1)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)

  step(1) // hdr cnt 24                         // end of 1 OFDM symbol
  expect(c.io.out_isHead, 0)
  expect(c.io.out_pktLatch, 1)
  expect(c.io.out_lenCnt, 0)
  expect(c.io.out_hdrEnd, 0)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)

  poke(c.io.in.valid, 1)
  poke(c.io.in.bits.bits(0), 1)   // 1, bit 1
  poke(c.io.in.bits.bits(1), 1)
  poke(c.io.in.bits.bits(2), 1)   // 10, bit 2
  poke(c.io.in.bits.bits(3), -1)
  poke(c.io.in.bits.bits(4), 1)   // 101, bit 3
  poke(c.io.in.bits.bits(5), 1)
  poke(c.io.in.bits.bits(6), -1)  // 1011, bit 4
  poke(c.io.in.bits.bits(7), -1)
  poke(c.io.in.bits.bits(8), -1)  // 10110, bit 5
  poke(c.io.in.bits.bits(9), 1)
  poke(c.io.in.bits.bits(10), 1)  // 101100, bit 6
  poke(c.io.in.bits.bits(11), -1)
  poke(c.io.in.bits.bits(12), -1) // 1011000, bit 7
  poke(c.io.in.bits.bits(13), -1)
  poke(c.io.in.bits.bits(14), -1) // 10110000, bit 8
  poke(c.io.in.bits.bits(15), -1)
  poke(c.io.in.bits.bits(16), -1) // 101100000, bit 9
  poke(c.io.in.bits.bits(17), -1)
  poke(c.io.in.bits.bits(18), -1) // 1011000000, bit 10
  poke(c.io.in.bits.bits(19), -1)
  poke(c.io.in.bits.bits(20), 1)  // 10110000001, bit 11
  poke(c.io.in.bits.bits(21), 1)
  poke(c.io.in.bits.bits(22), 1)  // 101100000010, bit 12
  poke(c.io.in.bits.bits(23), -1)
  poke(c.io.in.bits.bits(24), 1)  // 1011000000101, bit 13
  poke(c.io.in.bits.bits(25), 1)
  poke(c.io.in.bits.bits(26), -1) // 10110000001011, bit 14
  poke(c.io.in.bits.bits(27), -1)
  poke(c.io.in.bits.bits(28), -1) // 101100000010110, bit 15
  poke(c.io.in.bits.bits(29), 1)
  poke(c.io.in.bits.bits(30), 1)  // 1011000000101100, bit 16
  poke(c.io.in.bits.bits(31), -1)
  poke(c.io.in.bits.bits(32), -1) // 10110000001011000, bit 17
  poke(c.io.in.bits.bits(33), -1)
  poke(c.io.in.bits.bits(34), 1) // 101100000010110000, bit 18 - 1
  poke(c.io.in.bits.bits(35), 1)
  poke(c.io.in.bits.bits(36), 1) // 1011000000101100000, bit 19 - 0
  poke(c.io.in.bits.bits(37), 1)
  poke(c.io.in.bits.bits(38), -1) // 10110000001011000000, bit 20 - 1
  poke(c.io.in.bits.bits(39), 1) // tail 1
  poke(c.io.in.bits.bits(40), -1) // 101100000010110000000, bit 21 - 1
  poke(c.io.in.bits.bits(41), -1) // tail 2
  poke(c.io.in.bits.bits(42), 1) // 1011000000101100000000, bit 22 - 1
  poke(c.io.in.bits.bits(43), -1) // tail 3
  poke(c.io.in.bits.bits(44), -1) // 10110000001011000000000, bit 23 - 0
  poke(c.io.in.bits.bits(45), 1) // tail 4
  poke(c.io.in.bits.bits(46), 1) // 101100000010110000000000, bit 24 - 0
  poke(c.io.in.bits.bits(47), -1) // tail 5. total 24 bits (R = 1/2)
  expect(c.io.in.ready, 1)

  step(1)   // in -> PM
  expect(c.io.out_en1, 0)     // en 1 = dePunc en
  expect(c.io.out_en2, 0)     // en 2 = PathMetric en
  expect(c.io.out.valid, 0)

  step(1)   // PM -> TB
  poke(c.io.in.valid, 0)    // bit 1
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out_pm(0), 0)
  expect(c.io.out_pm(1), 16)
  expect(c.io.out_pm(2), 16)
  expect(c.io.out_pm(3), 16)
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
  expect(c.io.out_pm(0), 2)
  expect(c.io.out_pm(1), 16)
  expect(c.io.out_pm(2), 0)
  expect(c.io.out_pm(3), 17)
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
  expect(c.io.out_pm(0), 3)
  expect(c.io.out_pm(1), 1)
  expect(c.io.out_pm(2), 3)
  expect(c.io.out_pm(3), 1)
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
  expect(c.io.out_pm(0), 2)
  expect(c.io.out_pm(1), 2)
  expect(c.io.out_pm(2), 2)
  expect(c.io.out_pm(3), 2)
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
  expect(c.io.out_pm(0), 2)
  expect(c.io.out_pm(1), 3)
  expect(c.io.out_pm(2), 3)
  expect(c.io.out_pm(3), 2)
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
  expect(c.io.out_pm(0), 3)
  expect(c.io.out_pm(1), 2)
  expect(c.io.out_pm(2), 3)
  expect(c.io.out_pm(3), 4)
  expect(c.io.out_sp(0), 0)
  expect(c.io.out_sp(1), 3)
  expect(c.io.out_sp(2), 1)
  expect(c.io.out_sp(3), 3)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)    // bit 7, start first decoding
  expect(c.io.out_en1, 1)
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out_pm(0), 2)
  expect(c.io.out_pm(1), 4)
  expect(c.io.out_pm(2), 4)
  expect(c.io.out_pm(3), 4)
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
  expect(c.io.out_pm(1), 5)
  expect(c.io.out_pm(2), 4)
  expect(c.io.out_pm(3), 4)
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
  expect(c.io.out_pm(0), 2)
  expect(c.io.out_pm(1), 5)
  expect(c.io.out_pm(2), 4)
  expect(c.io.out_pm(3), 4)
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
  expect(c.io.out_pm(0), 2)
  expect(c.io.out_pm(1), 5)
  expect(c.io.out_pm(2), 4)
  expect(c.io.out_pm(3), 4)
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

  step(1) // bit 14, finish first decoding. move decodeReg to output
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
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out.valid, 0)

  step(1) // bit 19, finish first decoding. move decodeReg to output
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 20
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), 1)
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
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 23
  expect(c.io.out_bufData(0), -1)
  expect(c.io.out_bufData(1), 1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1) // bit 24, finish third decoding. move decodeReg to output
  expect(c.io.out_bufData(0), 1)
  expect(c.io.out_bufData(1), -1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 1)
  expect(c.io.out_en2, 1)

  step(1)
  expect(c.io.out.valid, 1)
  expect(c.io.out.bits(0), 1)   // bit 11~15
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 1)
  expect(c.io.out.bits(3), 1)
  expect(c.io.out.bits(4), 0)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 1)

  step(1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 0)
  expect(c.io.out_en2, 0)

  step(1)
  expect(c.io.out.valid, 0)
  expect(c.io.out_en1, 0)

  step(1)
  expect(c.io.out.valid, 0)

  step(1)
  expect(c.io.out.valid, 0)
  expect(c.io.restOut.valid, 0)

  step(1)
  expect(c.io.out.valid, 1)
  expect(c.io.out.bits(0), 0)     // bit 16~20
  expect(c.io.out.bits(1), 0)
  expect(c.io.out.bits(2), 1)
  expect(c.io.out.bits(3), 0)
  expect(c.io.out.bits(4), 1)
  expect(c.io.restOut.valid, 1)
  expect(c.io.restOut.bits(0), 1) // bit 21 = 1
  expect(c.io.restOut.bits(1), 1) // bit 22 = 1
  expect(c.io.restOut.bits(2), 0) // bit 23 = 0
  expect(c.io.restOut.bits(3), 0) // bit 24 = 0
  expect(c.io.restOut.bits(4), 0)
  expect(c.io.restOut.bits(5), 0)
  expect(c.io.restOut.bits(6), 0)

}

object HardViterbiDecoderTester {
  def apply(params: HardCoding): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new ViterbiDecoder(params)) {
      c => new ViterbiDecoderUnitTester_Hard(c)
    }
  }
}
