package modem

import org.scalatest.{FlatSpec, Matchers}

class HeaderUnitSpec_Fixed extends FlatSpec with Matchers {
  behavior of "HeaderUnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 3,
    O = 6,
    D = 4,
    H = 24,
//    H = 6,
    genPolynomial = List(7, 6), // generator polynomial
//    genPolynomial = List(7, 5),
    tailBitingEn = false,
    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = true
  )
  it should "Extract Header information" in {

    FixedHeaderTester(params) should be (true)
  }
}
