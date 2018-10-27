package modem

import breeze.math.Complex
import breeze.linalg._
import breeze.numerics._
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

case class EqualizerTestVectors() {
  def buildPacket(nSymbols: Int, impairment: DenseVector[Complex]) = {
    val data = DenseMatrix.rand(64, nSymbols).map(x => if(x < 0.5) Complex(1,0) else Complex(0,0))
    var bidata = data map (x => 1 - 2*x)
    bidata.slice(0,1) := Complex(0,0)
    val LTF = IEEE80211.ltfFreq
    val reference = DenseMatrix.vertcat(LTF.asDenseMatrix, LTF.asDenseMatrix, bidata)
    val chan = reference * impairment
    (chan, reference)
  }

  def fadingChannel(carrier: Int, gain: Complex) = {
    var chan = DenseVector.fill(64){Complex(1,0)}
    chan(carrier) *= gain
    chan
  }

  val cleanChan = DenseVector.fill(64){Complex(1,1)}
  val gainChan = DenseVector.fill(64){Complex(0.5,0)}
  val phaseChan = DenseVector.fill(64){Complex(0.707107, 0.707107)}
  val tvClean1 = buildPacket(1, cleanChan)
  val tvClean2 = buildPacket(2, cleanChan)
  val tvHalfGain = buildPacket(2, gainChan)
  val tvRotate = buildPacket(2, phaseChan)
  val tvFade = buildPacket(2, fadingChannel(20, Complex(0.25, 0.25)))
  val tvList = List(tvClean1, tvClean2, tvHalfGain, tvRotate, tvFade)
}

class EqualizerSpec extends FlatSpec with Matchers {
  val vecs = EqualizerTestVectors()
  behavior of "FixedEqualizer"

  val eqParams = FixedEqualizerParams(
    width=16,
    mu=0.25
    pilots=Seq(5, 21, 43, 59),
    carrierMask=Seq.fill(1)(false) ++ Seq.fill(27)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(5)(false) ++ Seq.fill(27)(true),
    nSubcarriers=64
  )
  it should "pass data" in {
    val trials = Seq(IQWide(vecs.tvClean1(0), Option(vecs.tvClean1(1))),
                     IQWide(vecs.tvClean2(0), Option(vecs.tvClean2(1))))
    FixedEqualizerTester(eqParams, trials) should be (true)
  }

  it should "correct gain" in {
    val trials = Seq(IQWide(vecs.tvHalfGain(0), Option(vecs.tvHalfGain(1))))
    FixedEqualizerTester(eqParams, trials) should be (true)
  }

  it should "correct phase" in {
    val trials = Seq(IQWide(vecs.tvRotate(0), Option(vecs.tvRotate(1))))
    FixedEqualizerTester(eqParams, trials) should be (true)
  }

  it should "correct fading" in {
    val trials = Seq(IQWide(vecs.tvFade(0), Option(vecs.tvFade(1))))
    FixedEqualizerTester(eqParams, trials) should be (true)
  }
}
