package modem

import org.scalatest.{FlatSpec, Matchers}

class ConvCodingUnitSpec extends FlatSpec with Matchers {
  behavior of "ConvCodingUnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 7,
    O = 6,
    D = 36,
    H = 24,
    genPolynomial = List(7, 5), // generator polynomial
    punctureEnable = false,
    punctureMatrix = List(6, 5), // Puncture Matrix
    CodingScheme = 0,
    fbPolynomial = List(0),
    tailBitingEn = true,
    tailBitingScheme = 0,
    softDecision = false
  )
  it should "Convolution code" in {
    FixedConvCodingTester(params) should be (true)
  }
}
