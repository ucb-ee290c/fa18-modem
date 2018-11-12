package modem

import breeze.math.Complex
import org.scalatest.{FlatSpec, Matchers}

case class EqualizerTestVectors() {
  val c1 = Complex.one
  val c0 = Complex.zero
  val tvClean = Seq(
    Seq(Seq( c0,  c1, -c1, -c1,  c1,  c1, -c1,
          c1, -c1,  c1, -c1, -c1, -c1, -c1,
         -c1,  c1,  c1, -c1, -c1,  c1, -c1,
          c1, -c1,  c1,  c1,  c1,  c1,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  c0,  c1,  c1, -c1, -c1,
          c1,  c1, -c1,  c1, -c1,  c1,  c1,
          c1,  c1,  c1,  c1, -c1, -c1,  c1,
          c1, -c1,  c1, -c1,  c1,  c1,  c1,
          c1),
        Seq( c0,  c1, -c1, -c1,  c1,  c1, -c1,
          c1, -c1,  c1, -c1, -c1, -c1, -c1,
         -c1,  c1,  c1, -c1, -c1,  c1, -c1,
          c1, -c1,  c1,  c1,  c1,  c1,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  c0,  c1,  c1, -c1, -c1,
          c1,  c1, -c1,  c1, -c1,  c1,  c1,
          c1,  c1,  c1,  c1, -c1, -c1,  c1,
          c1, -c1,  c1, -c1,  c1,  c1,  c1,
          c1),
        Seq( c0,  c1,  c1, -c1, -c1,  c1, -c1,
          c1,  c1, -c1,  c1,  c1,  c1,  c1,
          c1,  c1,  c1, -c1,  c1,  c1,  c1,
          c1, -c1, -c1,  c1,  c1, -c1,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  c0, -c1,  c1, -c1,  c1,
         -c1, -c1, -c1, -c1, -c1,  c1, -c1,
          c1, -c1,  c1, -c1, -c1,  c1, -c1,
          c1, -c1,  c1,  c1,  c1, -c1, -c1,
          c1)),
    Seq(Seq( c0,  c1,  c1, -c1, -c1,  c1, -c1,
          c1,  c1, -c1,  c1,  c1,  c1,  c1,
          c1,  c1,  c1, -c1,  c1,  c1,  c1,
          c1, -c1, -c1,  c1,  c1, -c1,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  c0, -c1,  c1, -c1,  c1,
         -c1, -c1, -c1, -c1, -c1,  c1, -c1,
          c1, -c1,  c1, -c1, -c1,  c1, -c1,
          c1, -c1,  c1,  c1,  c1, -c1, -c1,
          c1)))

}

class EqualizerSpec extends FlatSpec with Matchers {
  val vecs = EqualizerTestVectors()
  behavior of "FixedEqualizer"

  val eqParams = FixedEqualizerParams(
    width=16,
    mu=0.25,
    pilots=Seq(5, 21, 43, 59),
    carrierMask=Seq.fill(1)(false) ++ Seq.fill(27)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(5)(false) ++ Seq.fill(27)(true),
    nSubcarriers=64
  )
  it should "pass data" in {
    val trials = Seq(IQWide(vecs.tvClean(0), Option(vecs.tvClean(1))))
    FixedEqualizerTester(eqParams, trials) should be (true)
  }

  // it should "correct gain" in {
  //   val trials = Seq(IQWide(vecs.tvHalfGain(0), Option(vecs.tvHalfGain(1))))
  //   FixedEqualizerTester(eqParams, trials) should be (true)
  // }

  // it should "correct phase" in {
  //   val trials = Seq(IQWide(vecs.tvRotate(0), Option(vecs.tvRotate(1))))
  //   FixedEqualizerTester(eqParams, trials) should be (true)
  // }

  // it should "correct fading" in {
  //   val trials = Seq(IQWide(vecs.tvFade(0), Option(vecs.tvFade(1))))
  //   FixedEqualizerTester(eqParams, trials) should be (true)
  // }
}
