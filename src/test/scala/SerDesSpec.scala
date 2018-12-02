package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.Complex
import breeze.linalg.{randomDouble, randomInt}

class SerDesSpec extends FlatSpec with Matchers {
  behavior of "FixedPacketSerDes"

  val fixed_base_params = FixedPacketSerDesParams(
    dataWidth = 5,
    ratio = 2,
    binPoint = 3,
  )

  it should f"serialize/deserialize by 2" in {
    val inp = Seq.fill(12) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    FixedPacketSerializerTester(fixed_base_params, inp) should be (true)
    FixedPacketDeserializerTester(fixed_base_params, inp) should be (true)
    FixedPacketDesSerTester(fixed_base_params, inp) should be (true)
  }

  it should f"serialize/deserialize by 64" in {
    val inp = Seq.fill(64 * 4) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    val params = fixed_base_params.copy(ratio = 64)
    FixedPacketSerializerTester(params, inp) should be (true)
    FixedPacketDeserializerTester(params, inp) should be (true)
    FixedPacketDesSerTester(params, inp) should be (true)
  }

  behavior of "UIntBitsSerializer"
  it should f"serialize by 48" in {
    val inp: IndexedSeq[Int] = randomInt(96, (0, 31)).toScalaVector
    val params = UIntBitsSerDesParams(dataWidth = 5, ratio = 48)
    UIntBitsSerializerTester(params, inp) should be (true)
  }

}
