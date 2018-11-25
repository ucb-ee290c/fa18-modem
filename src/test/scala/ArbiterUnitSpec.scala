package modem

import org.scalatest.{FlatSpec, Matchers}

class ArbiterUnitSpec extends FlatSpec with Matchers {
  behavior of "ArbiterUnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 3,
    O = 6,
    D = 4,
    H = 24,
    genPolynomial = List(7, 6), // generator polynomial
    punctureEnable = false,
    punctureMatrix = List(6, 5), // Puncture Matrix
    CodingScheme = 0,
    fbPolynomial = List(0),
    tailBitingEn = false,
    tailBitingScheme = 0,
    softDecision = false
  )
  it should "check Header in || data in" in {

    FixedArbiterTester(params) should be (true)
  }
}
