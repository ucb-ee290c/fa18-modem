package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.linalg.{randomDouble}

class SerDesSpec extends FlatSpec with Matchers {
  behavior of "FixedSerDes"

  val base_params = FixedSerDesParams(
    dataWidth = 5,
    ratio = 2,
    maxVal = 1,
  )
  val base_params1 = UIntBitsSerDesParams(
    dataWidth = 5,
    bitsWidth = 48,
    ratio = 2,
    maxVal = 1,
  )


  it should f"serialize/deserialize by 2" in {
    val inp = Seq.fill(12) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    FixedSerializerTester(base_params, inp) should be (true)
    FixedDeserializerTester(base_params, inp) should be (true)
    FixedDesSerTester(base_params, inp) should be (true)
  }

  it should f"serialize/deserialize by 64" in {
    val inp = Seq.fill(64 * 4) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    val params = base_params.copy(ratio = 64)
    FixedSerializerTester(params, inp) should be (true)
    FixedDeserializerTester(params, inp) should be (true)
    FixedDesSerTester(params, inp) should be (true)
  }
  it should f"serialize by 48" in {
    val inp = Seq.fill(48 * 2) { 1 } ++ Seq.fill(48 * 2) { 0 }
    val params = base_params1.copy(ratio = 48)
    UIntSerializerTester(params, inp) should be (true)
    
  }

}
