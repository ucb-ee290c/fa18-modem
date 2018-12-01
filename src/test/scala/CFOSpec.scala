package modem

import breeze.math.Complex
import dsptools.numbers._
import chisel3._
import chisel3.experimental.FixedPoint
import org.scalatest.{FlatSpec, Matchers}

case class CFOTestVectors(randcfo: Int = 50000) {
  val stfTV = IEEE80211.stf
  val ltfTV = IEEE80211.ltf
  val r = new scala.util.Random
  //val randData = Seq.fill(20){Complex(2*(r.nextFloat)-1, 2*(r.nextFloat)-1)}
  val randData = Seq.fill(20){Complex(0.0, 0.0)}

  val rawTFTV = stfTV ++ ltfTV ++ randData
  val cleanTV = IEEE80211.addCFO(in = rawTFTV, cfo = 0.0, sampleRate = 20.0e6)
  val cfoTV = IEEE80211.addCFO(in = rawTFTV, cfo = randcfo, sampleRate = 20.0e6)
  val cfoSTV = IEEE80211.addCFO(in = stfTV, cfo = 100010, sampleRate = 20.0e6)
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

  it should "detect offset" in {
    val trials = Seq(IQ(vecs.cfoTV, None))
    val reals = Seq(IQ(vecs.cleanTV, None))
    FixedCFOEstimationTester(fixedCFOParams, trials, reals, 3) should be (true)
  }
}

class CFOCorrectionSpec extends FlatSpec with Matchers {
  val vecs = CFOTestVectors()
  behavior of "Estimate CFO"

  val fixedCFOParams = FixedCFOParams(iqWidth = 16)

  it should "detect no offset" in {
    val trials = Seq(IQ(vecs.cleanTV, None))
    val reals = Seq(IQ(vecs.cleanTV, None))
    FixedCFOCorrectionTester(fixedCFOParams, trials, reals, 0, 2) should be (true)
  }

  it should "detect offset of 0.2" in {
    val trials = Seq(IQ(vecs.cfoTV, None))
    val reals = Seq(IQ(vecs.cleanTV, None))
    FixedCFOCorrectionTester(fixedCFOParams, trials, reals, 50000, 2) should be (true)
  }
}
class COESpec extends FlatSpec with Matchers {
  val vecs = CFOTestVectors()
  behavior of "Coarse Estimation"

  val fixedCFOParams = FixedCFOParams(iqWidth = 32)

  //it should "detect no offset" in {
    //val trials = Seq(IQ(vecs.cleanTV, None))
    //FixedCFOEstimationTester(fixedCFOParams, trials) should be (true)
  //}

  it should "detect offset of 5kHz" in {
    val trials = Seq(IQ(vecs.cfoSTV, None))
    FixedCOETester(fixedCFOParams, trials, 100010) should be (true)
  }
}

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
