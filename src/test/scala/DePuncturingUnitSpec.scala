package modem

import org.scalatest.{FlatSpec, Matchers}

class DePuncturingUnitSpec extends FlatSpec with Matchers {
  behavior of "DePuncturingUnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 7,
    O = 10,
    D = 36,
    H = 5,
    genPolynomial = List(7, 5), // generator polynomial
    tailBitingEn = false,
    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = true,
    FFTPoint = 64
  )
  it should "de-puncturing code" in {
    FixedDePuncturingTester(params) should be (true)
  }
}
