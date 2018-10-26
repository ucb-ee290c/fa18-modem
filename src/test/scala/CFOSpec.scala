package cfo

import breeze.math.Complex
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

case class TestVectors() {
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
    Seq.fill(8)(Complex(1, -1)) ++ Seq.fill(8)(Complex(-1, 1)) ++
    Seq.fill(8)(Complex(1, -1)) ++ Seq.fill(8)(Complex(-1, 1)) ++ Seq.fill(8)(Complex(1, -1)) ++
    Seq.fill(8)(Complex(-1, 1)) ++ Seq.fill(8)(Complex(1, -1))
}

case class FixedCFOCorrectionParams(
  // width of I and Q
  iqWidth: Int,
  // Correct for gain
  correctGain: Boolean = true,
  // Just have one iteration per pipeline stage
  stagesPerCycle: Int = 1,
  // ST Field overall length
  stLength: Int = 10 * 16,
  // LT Field overall length,
  ltLength: Int = 2 * 64,
  // Preamble based?
  preamble: Boolean = true
) extends PacketDetectParams[FixedPoint] {
  //PacketBundleParams fields
  // prototype for iq
  // binary point is iqWidth-3 to allow for some inflation
  val protoIQ = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP))
  val width = 1
  // CordicParams fields
  val protoXY = FixedPoint(iqWidth.W, (iqWidth-3).BP)
  val protoZ = FixedPoint(iqWidth.W, (iqWidth-3).BP)
  val minNumber = math.pow(2.0, -(zWidth-2))
  // number of cordic stages
  private var n = 0
  while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
    n += 1
  }
  val nStages = n
  // CFOParams fields
}

class CFOCorrectionSpec extends FlatSpec with Matchers {
  val vecs = TestVectors()
  behavior of "FixedCFOCorrection"

  val noCorrParams = FixedCFOCorrectionParams(
    iqWidth = 16
  )
  it should "correct rotation" in {
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
  it should "detect power and correlation" in {
    val trials = Seq(IQ(vecs.tvNoPkt, None),
      IQ(vecs.tvPwrOnly, None),
      IQ(vecs.tvPwrCorr, Option(vecs.tvPwrCorrOut)))
    FixedPacketDetectTester(corrParams, trials) should be (true)
  }

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
