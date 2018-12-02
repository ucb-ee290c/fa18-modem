package modem

import org.scalatest.{FlatSpec, Matchers}

class PathMetricUnitSpec extends FlatSpec with Matchers {
  behavior of "Path Metric UnitSpec"

  it should "Calculate Path Metric" in {

    val params = FixedCoding(
      k = 1,
      n = 2,
      K = 3,
      L = 100,
      O = 6,
      D = 36,
      H = 24,
      genPolynomial = List(7, 6), // generator polynomial
      tailBitingEn = false,
      tailBitingScheme = 0,
      protoBitsWidth = 16,
      bitsWidth = 48,
      softDecision = true,
      FFTPoint = 64
    )

    FixedPathMetricTester(params) should be (true)
  }
}
