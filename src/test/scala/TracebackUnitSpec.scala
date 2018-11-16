package modem

import org.scalatest.{FlatSpec, Matchers}

class TracebackUnitSpec extends FlatSpec with Matchers {
  behavior of "TracebackUnitSpec"

  val params = FixedCoding(
    k = 1,
    n = 2,
    K = 3,
    L = 3,
    O = 6,
    D = 4,
    genPolynomial = List(7, 6), // generator polynomial
    punctureEnable = true,
    punctureMatrix = List(6, 5), // Puncture Matrix
    CodingScheme = 0,
    fbPolynomial = List(0),
    tailBitingEn = false,
    tailBitingScheme = 0,
    softDecision = false
  )
  it should "Traceback" in {

    FixedTracebackTester(params) should be (true)
  }
}