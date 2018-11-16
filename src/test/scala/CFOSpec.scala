package modem

import breeze.math.Complex
import dsptools.numbers._
import chisel3._
import chisel3.experimental.FixedPoint
import org.scalatest.{FlatSpec, Matchers}

case class CFOTestVectors() {
  val stfTV = IEEE80211.stf
  val ltfTV = IEEE80211.ltf

  val rawTFTV = stfTV ++ ltfTV
  val cleanTV = IEEE80211.addCFO(in = rawTFTV, cfo = 0.0, sampleRate = 20.0e6)
  val cfoTV = IEEE80211.addCFO(in = rawTFTV, cfo = 0.2, sampleRate = 20.0e6)
}

//case class FixedDecimationParams(
  //// width of I and Q
  //iqWidth: Int,
  //// Amount to decimate by
  //nDecimation: Int
//) extends DecimatorParams[FixedPoint] {
  ////PacketBundleParams fields
  //// prototype for iq
  //// binary point is iqWidth-3 to allow for some inflation
  //val protoIQ = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP))
  //val width = 1
//}

class CFOEstimationSpec extends FlatSpec with Matchers {
  val vecs = CFOTestVectors()
  behavior of "Estimate CFO"

  val fixedCFOParams = FixedCFOParams(
    width = 16
  )
  it should "detect no offset" in {
    val trials = Seq(IQ(vecs.cleanTV, None))
    FixedCFOEstimationTester(fixedCFOParams, trials) should be (true)
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
