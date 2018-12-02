package modem

import org.scalatest.{FlatSpec, Matchers}

class ViterbiDecoderUnitSpec extends FlatSpec with Matchers {
  behavior of "Viterbi Decoder"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 2,
    O = 48,
    D = 5,
    H = 24,
    genPolynomial = List(7, 6), // generator polynomial
    tailBitingEn = false,
    tailBitingScheme = 0,
    protoBitsWidth = 16,
    bitsWidth = 48,
    softDecision = true,
    FFTPoint = 64
  )
  it should "Traceback" in {

    FixedViterbiDecoderTester(params) should be (true)
  }
}
