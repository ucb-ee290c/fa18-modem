package modem

import org.scalatest.{FlatSpec, Matchers}

class ViterbiDecoderUnitSpec extends FlatSpec with Matchers {
  behavior of "Viterbi Decoder"

  val params = HardCoding(
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
    softDecision = false
  )
  it should "Traceback" in {

    HardViterbiDecoderTester(params) should be (true)
  }
}
