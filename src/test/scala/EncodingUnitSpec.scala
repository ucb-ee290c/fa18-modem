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
    H = 24,
    genPolynomial = List(7, 5), // generator polynomial
    tailBitingEn = false,
    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = false
  )
  it should "Convolutional Encoding" in {

    FixedEncodingTester(params) should be (true)
  }
}
