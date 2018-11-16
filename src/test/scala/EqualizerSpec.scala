package modem

import breeze.math.Complex
import scala.math
import org.scalatest.{FlatSpec, Matchers}

case class EqualizerTestVectors() {
  val c1 = Complex.one
  val c01 = Complex(0,1)
  val c0 = Complex.zero
  val cHalf = c1 / 2
  val cRoot2 = Complex(1/math.sqrt(2), 1/math.sqrt(2))
  val cRoot2H = cRoot2.conjugate
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
          c1)))

  val tvHalfGain = Seq(Seq(Seq(c0,  cHalf, -cHalf, -cHalf,  cHalf,  cHalf,
         -cHalf,  cHalf, -cHalf,  cHalf, -cHalf, -cHalf,
         -cHalf, -cHalf, -cHalf,  cHalf,  cHalf, -cHalf,
         -cHalf,  cHalf, -cHalf,  cHalf, -cHalf,  cHalf,
          cHalf,  cHalf,  cHalf,  c0,  c0,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  cHalf,  cHalf, -cHalf, -cHalf,
          cHalf,  cHalf, -cHalf,  cHalf, -cHalf,  cHalf,
          cHalf,  cHalf,  cHalf,  cHalf,  cHalf, -cHalf,
         -cHalf,  cHalf,  cHalf, -cHalf,  cHalf, -cHalf,
          cHalf,  cHalf,  cHalf,  cHalf),
        Seq( c0,  cHalf, -cHalf, -cHalf,  cHalf,  cHalf,
         -cHalf,  cHalf, -cHalf,  cHalf, -cHalf, -cHalf,
         -cHalf, -cHalf, -cHalf,  cHalf,  cHalf, -cHalf,
         -cHalf,  cHalf, -cHalf,  cHalf, -cHalf,  cHalf,
          cHalf,  cHalf,  cHalf,  c0,  c0,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  cHalf,  cHalf, -cHalf, -cHalf,
          cHalf,  cHalf, -cHalf,  cHalf, -cHalf,  cHalf,
          cHalf,  cHalf,  cHalf,  cHalf,  cHalf, -cHalf,
         -cHalf,  cHalf,  cHalf, -cHalf,  cHalf, -cHalf,
          cHalf,  cHalf,  cHalf,  cHalf),
        Seq( c0,  cHalf, -cHalf, -cHalf, -cHalf, -cHalf,
         -cHalf,  cHalf,  cHalf,  cHalf,  cHalf, -cHalf,
          cHalf, -cHalf, -cHalf,  cHalf, -cHalf, -cHalf,
          cHalf, -cHalf,  cHalf, -cHalf,  cHalf,  cHalf,
          cHalf,  cHalf, -cHalf,  c0,  c0,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  cHalf, -cHalf, -cHalf,  cHalf,
          cHalf,  cHalf, -cHalf, -cHalf,  cHalf,  cHalf,
         -cHalf, -cHalf,  cHalf,  cHalf, -cHalf, -cHalf,
          cHalf, -cHalf,  cHalf,  cHalf,  cHalf,  cHalf,
          cHalf, -cHalf,  cHalf,  cHalf),
        Seq( c0, -cHalf,  cHalf, -cHalf, -cHalf, -cHalf,
         -cHalf,  cHalf,  cHalf,  cHalf, -cHalf,  cHalf,
          cHalf,  cHalf,  cHalf,  cHalf, -cHalf, -cHalf,
         -cHalf, -cHalf, -cHalf,  cHalf, -cHalf, -cHalf,
          cHalf, -cHalf, -cHalf,  c0,  c0,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0, -cHalf, -cHalf,  cHalf,  cHalf,
         -cHalf, -cHalf,  cHalf,  cHalf,  cHalf, -cHalf,
          cHalf, -cHalf, -cHalf, -cHalf, -cHalf,  cHalf,
          cHalf,  cHalf,  cHalf, -cHalf, -cHalf,  cHalf,
          cHalf,  cHalf,  cHalf, -cHalf)),
    Seq(Seq( c0,  c1, -c1, -c1, -c1, -c1, -c1,
          c1,  c1,  c1,  c1, -c1,  c1, -c1,
         -c1,  c1, -c1, -c1,  c1, -c1,  c1,
         -c1,  c1,  c1,  c1,  c1, -c1,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  c0,  c1, -c1, -c1,  c1,
          c1,  c1, -c1, -c1,  c1,  c1, -c1,
         -c1,  c1,  c1, -c1, -c1,  c1, -c1,
          c1,  c1,  c1,  c1,  c1, -c1,  c1,
          c1),
        Seq( c0, -c1,  c1, -c1, -c1, -c1, -c1,
          c1,  c1,  c1, -c1,  c1,  c1,  c1,
          c1,  c1, -c1, -c1, -c1, -c1, -c1,
          c1, -c1, -c1,  c1, -c1, -c1,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  c0, -c1, -c1,  c1,  c1,
         -c1, -c1,  c1,  c1,  c1, -c1,  c1,
         -c1, -c1, -c1, -c1,  c1,  c1,  c1,
          c1, -c1, -c1,  c1,  c1,  c1,  c1,
         -c1)))

  val tvRotate = Seq(Seq(Seq( c0, cRoot2,  -c01,
        cRoot2H, -c1, -cRoot2,
         c01, cRoot2H, -c1,
        cRoot2,  -c01, cRoot2H,
        c1, cRoot2,  c01,
        cRoot2H, c1, -cRoot2,
         -c01, -cRoot2H, c1,
        -cRoot2,  c01, cRoot2H,
        c1, cRoot2,  c01,
         c0,  c0,  c0,
         c0,  c0,  c0,
         c0,  c0,  c0,
         c0,  c0,  -c01,
        cRoot2H, -c1, -cRoot2,
         c01, -cRoot2H, c1,
        -cRoot2,  c01, cRoot2H,
        c1, cRoot2,  c01,
        -cRoot2H, -c1, cRoot2,
         c01, cRoot2H, c1,
        -cRoot2,  c01, cRoot2H,
        -c1, -cRoot2,  -c01,
        cRoot2H),
       Seq( c0, cRoot2,  -c01,
        cRoot2H, -c1, -cRoot2,
         c01, cRoot2H, -c1,
        cRoot2,  -c01, cRoot2H,
        c1, cRoot2,  c01,
        cRoot2H, c1, -cRoot2,
         -c01, -cRoot2H, c1,
        -cRoot2,  c01, cRoot2H,
        c1, cRoot2,  c01,
         c0,  c0,  c0,
         c0,  c0,  c0,
         c0,  c0,  c0,
         c0,  c0,  -c01,
        cRoot2H, -c1, -cRoot2,
         c01, -cRoot2H, c1,
        -cRoot2,  c01, cRoot2H,
        c1, cRoot2,  c01,
        -cRoot2H, -c1, cRoot2,
         c01, cRoot2H, c1,
        -cRoot2,  c01, cRoot2H,
        -c1, -cRoot2,  -c01,
        cRoot2H),
       Seq( c0, cRoot2,
          c01, cRoot2H,
         -c1, cRoot2,
          c01, cRoot2H,
         -c1, -cRoot2,
         -c01, -cRoot2H,
         -c1, -cRoot2,
         -c01, -cRoot2H,
         -c1, cRoot2,
         -c01, -cRoot2H,
          c1, cRoot2,
          c01, -cRoot2H,
          c1, cRoot2,
         c01,  c0,
          c0,  c0,
          c0,  c0,
          c0,  c0,
          c0,  c0,
          c0,  c0,
         -c01, -cRoot2H,
          c1, -cRoot2,
         c01, -cRoot2H,
          c1, cRoot2,
         -c01, cRoot2H,
          c1, cRoot2,
          -c01, cRoot2H,
          c1, -cRoot2,
          c01, cRoot2H,
          c1, cRoot2,
          -c01, -cRoot2H,
         -c1, cRoot2,
         -c01, cRoot2H)),
    Seq(Seq( c0,  c1,  c1, -c1,  c1, -c1, -c1,
          c1, -c1, -c1, -c1,  c1,  c1,  c1,
          c1, -c1, -c1,  c1, -c1,  c1, -c1,
         -c1, -c1, -c1,  c1,  c1,  c1,  c0,
          c0,  c0,  c0,  c0,  c0,  c0,  c0,
          c0,  c0,  c0,  c1, -c1,  c1, -c1,
          c1,  c1, -c1, -c1,  c1,  c1,  c1,
          c1, -c1, -c1, -c1,  c1, -c1,  c1,
          c1,  c1, -c1,  c1,  c1, -c1,  c1,
          c1)))

}

class EqualizerSpec extends FlatSpec with Matchers {
  val vecs = EqualizerTestVectors()
  behavior of "FixedEqualizer"

  val eqParams = FixedEqualizerParams(
    width=16,
    mu=0.25,
    pilots=Seq(5, 21, 43, 59),
    carrierMask=Seq.fill(1)(false) ++ Seq.fill(26)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(6)(false) ++ Seq.fill(27)(true),
    nSubcarriers=64
  )
  it should "pass data" in {
    val trials = Seq(IQWide(vecs.tvClean(0), Option(vecs.tvClean(1))))
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

}

class ChannelInverterSpec extends FlatSpec with Matchers {
  val input = Seq(Complex(1,0), Complex(-1,0), Complex(0,1), Complex(0,-1), Complex(0.5, 0), Complex(2, 0), Complex(0.5, 0.5), Complex(1.5, 1.5))
  val output = Seq(Complex(1,0), Complex(-1,0), Complex(0,-1), Complex(0,1), Complex(2, 0), Complex(0.5, 0), Complex(1, -1), Complex(1.0/3, -1.0/3))
  behavior of "ChannelInverter"

  val params = FixedEqualizerParams(
    width=16,
    mu=0.25,
    pilots=Seq(5, 21, 43, 59),
    carrierMask=Seq.fill(1)(false) ++ Seq.fill(27)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(5)(false) ++ Seq.fill(27)(true),
    nSubcarriers=64
  )
  it should "invert" in {
    val trials = Seq(IQNarrow(input, Option(output)))
    ChannelInverterTester(params, trials) should be (true)
  }
}