package modem

import breeze.math.Complex
import dsptools.numbers._
import chisel3._
import chisel3.experimental.FixedPoint
import org.scalatest.{FlatSpec, Matchers}

case class CFOTestVectors() {
  val stfTV = IEEE80211.stf
  val ltfTV = IEEE80211.ltf
  val r = new scala.util.Random
  val randData = Seq.fill(20){Complex(2*(r.nextFloat)-1, 2*(r.nextFloat)-1)}

  val rawTFTV = stfTV ++ ltfTV ++ randData
  val cleanTV = IEEE80211.addCFO(in = rawTFTV, cfo = 0.0, sampleRate = 20.0e6)
  val cfoTV = IEEE80211.addCFO(in = rawTFTV, cfo = 6666, sampleRate = 20.0e6)
  val cfoSTV = IEEE80211.addCFO(in = stfTV, cfo = 5000, sampleRate = 20.0e6)
  val cfoLTV = IEEE80211.addCFO(in = stfTV, cfo = 100, sampleRate = 20.0e6)
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

  val fixedCFOParams = FixedCFOParams(iqWidth = 32)

  //it should "detect no offset" in {
    //val trials = Seq(IQ(vecs.cleanTV, None))
    //FixedCFOEstimationTester(fixedCFOParams, trials) should be (true)
  //}

  //it should "detect offset of 0.2" in {
    //val trials = Seq(IQ(vecs.cfoTV, None))
    //FixedCFOEstimationTester(fixedCFOParams, trials) should be (true)
  //}

  it should "detect offset of 0.2" in {
    val trials = Seq(IQ(vecs.cfoTV, None))
    FixedCFOCorrectionTester(fixedCFOParams, trials) should be (true)
  }
}

//class COESpec extends FlatSpec with Matchers {
  //val vecs = CFOTestVectors()
  //behavior of "Coarse Estimation"

  //val fixedCFOParams = FixedCFOParams(iqWidth = 32)

  ////it should "detect no offset" in {
    ////val trials = Seq(IQ(vecs.cleanTV, None))
    ////FixedCFOEstimationTester(fixedCFOParams, trials) should be (true)
  ////}

  //it should "detect offset of 5kHz" in {
    //val trials = Seq(IQ(vecs.cfoSTV, None))
    //FixedCOETester(fixedCFOParams, trials, 5000) should be (true)
  //}
//}

//class FOESpec extends FlatSpec with Matchers {
  //val vecs = CFOTestVectors()
  //behavior of "Fine Estimation"

  //val fixedCFOParams = FixedCFOParams(iqWidth = 32)

  ////it should "detect no offset" in {
    ////val trials = Seq(IQ(vecs.cleanTV, None))
    ////FixedCFOEstimationTester(fixedCFOParams, trials) should be (true)
  ////}

  //it should "detect offset of 100Hz" in {
    //val trials = Seq(IQ(vecs.cfoLTV, None))
    //FixedFOETester(fixedCFOParams, trials, 100) should be (true)
  //}
//}
