package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{randomDouble}

class FixedModemSpec extends FlatSpec with Matchers {
  behavior of "FixedModem"

  // These are  bogus placeholder numbers
  val iqWidth = 5
  val binPoint = iqWidth - 3
  val bitsWidth = 48

  val fixedIQParams = new IQBundleParams[FixedPoint](
      val protoIQ: DspComplex[T] = DspComplex(FixedPoint(iqWidth.W, binPoint.BP))
  )

  val fixedPktDetectParams = FixedPacketDetectParams(iqWidth = iqWidth)

  val fixedEqualizerParams = FixedEqualizerParams(width = iqWidth)

  val fixedCFOParams = FixedCFOParams(width = iqWidth, stagesPerCycle = 5)

  val fixedFFTParams = FixedFFTParams(dataWidth = iqWidth, twiddleWidth = 3)

  val hardBitsBundleParams = new BitsBundleParams[Bool()](
    val bitsWidth: Int = bitsWidth
    val protoBits: Bool() = Bool()
  )

  val hardDemodParams = HardDemodParams(width = iqWidth, bitsWidth = bitsWidth)

  val hardViterbiParams = FixedCoding()
}
