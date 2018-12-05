package modem

import breeze.math.Complex
import org.scalatest.{FlatSpec, Matchers}

case class PacketDetectTestVectors() {
  def alternateHighPwr(n: Int): Seq[Complex] = {
    var out = Seq[Complex]()
    for (i <- 0 until n) {
      out = out :+ Complex(1, 1 - 2*(i%2))
    }
    out
  }
  def alternateLowPwr(n: Int): Seq[Complex] = {
    alternateHighPwr(n).map(c => c * 0.1)
  }
  val tvNoPkt: Seq[Complex] = alternateLowPwr(64)
  // Has power but not correlated
  val tvPwrOnly: Seq[Complex] =
    Seq.fill(10)(Complex(0, 0)) ++ Seq.fill(15)(Complex(1, -1)) ++ Seq.fill(15)(Complex(-1, 1)) ++
    Seq.fill(15)(Complex(1, -1)) ++ Seq.fill(15)(Complex(-1, 1)) ++ Seq.fill(15)(Complex(0, 0))
  val tvPwrOnlyOut: Seq[Complex] =
    Seq.fill(15)(Complex(1, -1)) ++ Seq.fill(15)(Complex(-1, 1)) ++
    Seq.fill(15)(Complex(1, -1)) ++ Seq.fill(15)(Complex(-1, 1))
  // Has power and correlation
  val tvPwrCorr: Seq[Complex] =
    Seq.fill(10)(Complex(0, 0)) ++ Seq.fill(8)(Complex(1, -1)) ++ Seq.fill(8)(Complex(-1, 1)) ++
    Seq.fill(8)(Complex(1, -1)) ++ Seq.fill(8)(Complex(-1, 1)) ++ Seq.fill(8)(Complex(1, -1)) ++
    Seq.fill(8)(Complex(-1, 1)) ++ Seq.fill(8)(Complex(1, -1)) ++ Seq.fill(16)(Complex(0, 0))
  val tvPwrCorrOut: Seq[Complex] =
    Seq.fill(8)(Complex(1, -1)) ++ Seq.fill(8)(Complex(-1, 1)) ++ Seq.fill(8)(Complex(1, -1)) ++
    Seq.fill(8)(Complex(-1, 1)) ++ Seq.fill(8)(Complex(1, -1)) ++ Seq.fill(8)(Complex(-1, 1)) ++
    Seq.fill(8)(Complex(1, -1))
}


class PacketDetectSpec extends FlatSpec with Matchers {
  val vecs = PacketDetectTestVectors()
  behavior of "FixedPacketDetect"

  val noCorrParams = FixedPacketDetectParams(
    iqWidth = 16,
    powerThreshWindow = 4,
    correlationThresh = false
  )
  it should "detect power" in {
    val trials = Seq(IQ(vecs.tvNoPkt, None),
                     IQ(vecs.tvPwrOnly, Option(vecs.tvPwrOnlyOut)),
                     IQ(vecs.tvPwrCorr, Option(vecs.tvPwrCorrOut)))
    FixedPacketDetectTester(noCorrParams, trials) should be (true)
  }

  val corrParams = FixedPacketDetectParams(
    iqWidth = 16,
    powerThreshWindow = 4,
    correlationThresh = true
  )
  // it should "detect power and correlation" in {
  //   val trials = Seq(IQ(vecs.tvNoPkt, None))
  //     // IQ(vecs.tvPwrOnly, None),
  //     // IQ(vecs.tvPwrCorr, Option(vecs.tvPwrCorrOut)))
  //   FixedPacketDetectTester(corrParams, trials) should be (true)
  // }

//  behavior of "RealPacketDetect"
//
//  val realNoCorrParams = new PacketDetectParams[DspReal] {
//    val protoIQ = DspComplex(DspReal())
//    val powerThreshVal: Double = 0.75 // Power threshold
//    val powerThreshWindow: Int = 4 // Number of samples greater than power in a row before triggering
//    val correlationThresh: Boolean = false
//    val correlationThreshVal: Double = 0.75
//    val correlationWindow: Int = 4 // Number of strided correlations to sum
//    val correlationStride: Int = 16 // Stride between correlated samples
//  }
//  it should "detect power for reals" in {
//    val trials = Seq(IQ(vecs.tvNoPkt, None),
//      IQ(vecs.tvPwrOnly, Option(vecs.tvPwrOnlyOut)),
//      IQ(vecs.tvPwrCorr, Option(vecs.tvPwrCorrOut)))
//    RealPacketDetectTester(realNoCorrParams, trials) should be (true)
//  }
//
//  val realCorrParams = new PacketDetectParams[DspReal] {
//    val protoIQ = DspComplex(DspReal())
//    val powerThreshVal: Double = 0.75 // Power threshold
//    val powerThreshWindow: Int = 4 // Number of samples greater than power in a row before triggering
//    val correlationThresh: Boolean = true
//    val correlationThreshVal: Double = 0.75
//    val correlationWindow: Int = 4 // Number of strided correlations to sum
//    val correlationStride: Int = 16 // Stride between correlated samples
//  }
//  it should "detect power and correlation for reals" in {
//    val trials = Seq(IQ(vecs.tvNoPkt, None),
//                    IQ(vecs.tvPwrOnly, None),
//                    IQ(vecs.tvPwrCorr, Option(vecs.tvPwrCorrOut)))
//    RealPacketDetectTester(realCorrParams, trials) should be (true)
//  }

}
