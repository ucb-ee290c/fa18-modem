package modem

import org.scalatest.{FlatSpec, Matchers}

class ArbiterUnitSpec extends FlatSpec with Matchers {
  behavior of "ArbiterUnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 3,
//    O = 6,
    D = 4,
    H = 4,
    genPolynomial = List(7, 6), // generator polynomial
    tailBitingEn = false,
//    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = false,
    FFTPoint = 4
  )
  it should "check Header in || data in" in {

    FixedArbiterTester(params) should be (true)
  }
}
