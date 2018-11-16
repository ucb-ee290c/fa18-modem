package modemRead

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{randomDouble}

class FixedModemSpec extends FlatSpec with Matchers {
  behavior of "FixedModem"

  // These are  bogus placeholder numbers
  val dataWidth = 5
  val binPoint = 2

  val fixedIQParams = new IQBundleParams[FixedPoint](
      val protoIQ: DspComplex[T] = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
  )

  val fixedPktDetectParams

  val fixedEqualizerParams

  val fixedCFOParams

  val fixedFFTParams = FixedFFTParams(dataWidth = 5, twiddleWidth = 3)

  val fixedBitsBundleParams

  val fixedDemondParams

  val fixedViterbiParams
}
