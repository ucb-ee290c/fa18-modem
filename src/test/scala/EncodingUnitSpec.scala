package modem

import org.scalatest.{FlatSpec, Matchers}

class EncodingUnitSpec extends FlatSpec with Matchers {
  behavior of "Encoding UnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 7,
    O = 6,
    D = 36,
    genPolynomial = List(7, 5), // generator polynomial
    punctureEnable = true,
    punctureMatrix = List(6, 5), // Puncture Matrix
    CodingScheme = 0,
    fbPolynomial = List(0),
    tailBitingEn = false,
    tailBitingScheme = 0,
    softDecision = false
  )
  it should "Convolutional Encoding" in {
    val inSeq1  = Seq(1, 0, 1, 1, 0, 0)
    val baseTrial = EncodingInOut(inBit=1, stateIn = 0)
    val trials = inSeq1.map { case(a) => baseTrial.copy(inBit = a)}

    FixedEncodingTester(params, trials) should be (true)
  }
}
