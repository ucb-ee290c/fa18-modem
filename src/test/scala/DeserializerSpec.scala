package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.linalg.{randomDouble}

class FixedDeserializerSpec extends FlatSpec with Matchers {
  behavior of "FixedDeserializer"

  val base_params = FixedDeserializerParams(
    dataWidth = 5,
    deserRatio = 2,
    maxVal = 1,
  )

  it should f"deserialize by 2" in {
    val inp = Seq.fill(12) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    FixedDeserializerTester(base_params, inp) should be (true)
  }

  it should f"deserialize by 64" in {
    val inp = Seq.fill(64 * 4) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    val params = base_params.copy(deserRatio = 64)
    FixedDeserializerTester(params, inp) should be (true)
  }
}
