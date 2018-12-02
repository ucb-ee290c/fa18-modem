package modem

import org.scalatest.{FlatSpec, Matchers}

class HeaderUnitSpec extends FlatSpec with Matchers {
  behavior of "HeaderUnitSpec"

  val params = HardCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 3,
//    O = 6,
    D = 4,
    H = 24,
//    H = 6,
    genPolynomial = List(7, 6), // generator polynomial
    tailBitingEn = false,
//    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = false,
    FFTPoint = 64
  )
  it should "Extract Header information" in {

    HardHeaderTester(params) should be (true)
  }
}
