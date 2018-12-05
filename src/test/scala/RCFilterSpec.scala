package modem

import breeze.math.Complex
import org.scalatest.{FlatSpec, Matchers}

case class RCFilterTestVectors() {
  val impulse: Seq[Complex] = Seq.fill(1)(Complex(1, 0))
  val jImpulse: Seq[Complex] = Seq.fill(1)(Complex(0, 1))
  val doubleImpulse: Seq[Complex] = Seq.fill(2)(Complex(1, 0))
}


class RCFilterSpec extends FlatSpec with Matchers {
  val vecs = RCFilterTestVectors()
  behavior of "FixedRCFilter"

  val params = FixedRCFilterParams(
    dataWidth = 16,
    binaryPoint = 13,
    alpha = 0.21,
    sampsPerSymbol = 4,
    symbolSpan = 3
  )
  val taps = RCTaps(params)
  val impResponse = (taps.tail.reverse ++ taps).map{x => Complex(x, 0)} ++ Seq.fill(3)(Complex(0.0, 0.0))
  val jImpResponse = (taps.tail.reverse ++ taps).map{x => Complex(0, x)} ++ Seq.fill(3)(Complex(0.0, 0.0))
  val doubleImpResponse = impResponse.slice(0, 4) ++
                          (impResponse.drop(4) zip impResponse.take(impResponse.length - 4) map {case (x,y) => x + y}) ++
                          Seq.fill(4)(Complex(0.0, 0.0))
  println(s"slice len ${impResponse.slice(0,4).length}")
  println(s"slice len ${impResponse.slice(impResponse.length-5,impResponse.length-1).length}")

  it should "have an impulse response" in {
    val trials = Seq(RCIQ(vecs.impulse, impResponse),
                     RCIQ(vecs.jImpulse, jImpResponse))
    FixedRCFilterTester(params, trials) should be (true)
  }

  it should "handle two impulses" in {
    val trials = Seq(RCIQ(vecs.doubleImpulse, doubleImpResponse))
    FixedRCFilterTester(params, trials) should be (true)
  }

  val rectParams = FixedRCFilterParams(
    dataWidth = 16,
    binaryPoint = 13,
    alpha = 0.0,
    sampsPerSymbol = 1,
    symbolSpan = 1
  )
  val rectTaps = RCTaps(rectParams)
  val rectResponse = (rectTaps.tail.reverse ++ rectTaps).map{x => Complex(x, 0)}
  it should "act as identity" in {
    val trials = Seq(RCIQ(vecs.impulse, rectResponse))
    FixedRCFilterTester(rectParams, trials) should be (true)
  }
}
