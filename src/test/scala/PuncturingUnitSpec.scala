package modem

import org.scalatest.{FlatSpec, Matchers}

class PuncturingUnitSpec extends FlatSpec with Matchers {
  behavior of "PuncturingUnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 7,
    O = 6,
    D = 36,
    H = 24,
    genPolynomial = List(7, 5), // generator polynomial
    punctureEnable = true,
    punctureMatrix = List(6, 5), // Puncture Matrix
    CodingScheme = 0,
    fbPolynomial = List(0),
    tailBitingEn = false,
    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = false
  )
  it should "puncturing code" in {

    FixedPuncturingTester(params) should be (true)
  }
}
