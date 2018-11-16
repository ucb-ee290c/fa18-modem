package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{randomDouble}

class FixedRXSpec extends FlatSpec with Matchers {
  behavior of "FixedRX"

  // These are  bogus placeholder numbers
  val iqWidth = 5
  val binPoint = iqWidth - 3
  val bitsWidth = 48
  val prefixLength = 16
  val symbolLength = 64

  val fixedIQParams = new IQBundleParams[FixedPoint]{
    val protoIQ: DspComplex[T] = DspComplex(FixedPoint(iqWidth.W, binPoint.BP))
  }

  val fixedPktDetectParams = FixedPacketDetectParams(iqWidth = iqWidth)

  val fixedEqualizerParams = FixedEqualizerParams(width = iqWidth)

  val fixedCFOParams = FixedCFOParams(width = iqWidth, stagesPerCycle = 5)

  val fixedCPParams = new CyclicPrefixParams[FixedPoint]{
    val protoIQ = DspComplex(FixedPoint(iqWidth.W, binPoint.BP))
    val prefixLength = prefixLength
    val symbolLength = symbolLength
  }

  val fixedFFTParams = FixedFFTParams(dataWidth = iqWidth, binPoint = binPoint, twiddleWidth = iqWidth)

  val hardBitsBundleParams = new BitsBundleParams[Bool]{
    val bitsWidth: Int = bitsWidth
    val protoBits: Bool() = Bool()
  }

  val hardDemodParams = HardDemodParams(width = iqWidth, bitsWidth = bitsWidth)

  val hardViterbiParams = FixedCoding()

  it should "receive ofdm" in {
    val trials = Seq(1)
    FixedRXTester(
      fixedIQParams,
      fixedPktDetectParams,
      fixedEqualizerParams,
      fixedCFOParams,
      fixedCPParams,
      fixedFFTParams,
      hardBitsBundleParams,
      hardDemodParams,
      hardViterbiParams,
      trials) should be (true)
  }
}
