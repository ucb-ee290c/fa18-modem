package modem

import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{randomDouble}

class FixedRXSpec extends FlatSpec with Matchers {
  behavior of "FixedRX"

  val trials = Seq(DspComplex(1.U,0.U))
  // These are  bogus placeholder numbers
  val iqWidth = 5
  val binPoint = iqWidth - 3
  val numPoints = 64
  val bitsWidth = 48
  val prfxLength = 16
  val symbLength = 64

  val fixedIQParams = new IQBundleParams[FixedPoint]{
    val protoIQ: DspComplex[FixedPoint] = DspComplex(FixedPoint(iqWidth.W, binPoint.BP))
  }

  val fixedPktDetectParams = FixedPacketDetectParams(iqWidth = iqWidth)

  val fixedEqualizerParams = FixedEqualizerParams(width = iqWidth)

  val fixedCFOParams = FixedCFOParams(width = iqWidth, stagesPerCycle = 5)

  val fixedCPParams = new CyclicPrefixParams[FixedPoint]{
    val protoIQ = DspComplex(FixedPoint(iqWidth.W, binPoint.BP))
    val prefixLength = prfxLength
    val symbolLength = symbLength
  }

  val fixedFFTParams = FixedFFTParams(dataWidth = iqWidth, binPoint = binPoint, numPoints = numPoints, twiddleWidth = iqWidth)

  // val hardBitsBundleParams = new BitsBundleParams[UInt]{
  //   val bitsWidth: Int = bitsWidth
  //   val protoBits: UInt = UInt(1.W)
  // }

  val hardBitsBundleParams = BitsBundleParams(bitsWidth = bitsWidth, protoBits = UInt(1.W))

  val hardDemodParams = HardDemodParams(width = iqWidth, bitsWidth = bitsWidth)

  val hardViterbiParams = FixedCoding()

  it should "receive ofdm" in {
    //val trials = Seq(1)
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
