package modem

import org.scalatest.{FlatSpec, Matchers}

class ConvCodingUnitSpec extends FlatSpec with Matchers {
  behavior of "ConvCodingUnitSpec"

  val params = TxCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 7,
//    O = 6,
    D = 36,
    H = 24,
    genPolynomial = List(7, 5), // generator polynomial
    tailBitingEn = true,
//    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = false,
    FFTPoint = 64
  )
  it should "Convolution code" in {
    FixedConvCodingTester(params) should be (true)
  }
}
