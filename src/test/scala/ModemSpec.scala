package modem

import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{randomDouble}
import 

case class testVecs(){
  val ssv = SSVConverter
  val txInfo = ssv.toIntSeq("./src/test/scala/TX/OFDM_TX_bit_symbols_Len.txt")
  val modType = txInfo(2)

  if (modType == 0){
    // QPSK
    val bits = ssv.toInSeq("./src/test/scala/TX/OFDM_TX_bit_symbols.txt")
  } else if (modType == 1){
    // BPSK
    val bits = ssv.toInSeq("./src/test/scala/TX/OFDM_TX_bit_symbols.txt")
  } else if (modType == 2){
    // 16QAM
    val bits = ssv.toInSeq("./src/test/scala/TX/OFDM_TX_bit_symbols.txt")
  } else if (modType == 3){
    // 64QAM
    val bits = ssv.toInSeq("./src/test/scala/TX/OFDM_TX_bit_symbols.txt")
  }
  
  val groupedTXBits = bits.grouped(48)
  

class FixedTXSpec


class FixedRXSpec extends FlatSpec with Matchers {
  behavior of "FixedRX"

  val trials = Seq(DspComplex(1.U,0.U))
  // These are  bogus placeholder numbers
  val iqWidth = 16
  val binPoint = 13
  val numPoints = 64
  val bitsWidth = 48
  val prfxLength = 16
  val symbLength = 64

  val fixedIQParams = new IQBundleParams[FixedPoint]{
    val protoIQ: DspComplex[FixedPoint] = DspComplex(FixedPoint(iqWidth.W, binPoint.BP))
  }

  val fixedPktDetectParams = FixedPacketDetectParams(iqWidth = iqWidth)

  val fixedCyclicPrefixParams = new CyclicPrefixParams[FixedPoint] {
    val protoIQ = fixedIQParams.protoIQ
    val prefixLength = numPoints / 4
    val symbolLength = numPoints
  }

  val fixedEqualizerParams = FixedEqualizerParams(width = iqWidth, binaryPoint = binPoint)

  val fixedCFOParams = FixedCFOParams(iqWidth = iqWidth, stagesPerCycle = 5)

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

  val hardBitsBundleParams = BitsBundleParams(width = bitsWidth, proto = SInt(2.W))

  val hardDemodParams = HardDemodParams(width = 64, dataWidth = iqWidth, dataBinaryPoint = binPoint, bitsWidth = bitsWidth, hsmod=1)

  val hardViterbiParams = HardCoding()

  val rxParams = new RXParams[FixedPoint, SInt, UInt] {
    val iqBundleParams = fixedIQParams
    val pktDetectParams = fixedPktDetectParams
    val cyclicPrefixParams = fixedCyclicPrefixParams
    val equalizerParams = fixedEqualizerParams
    val cfoParams = fixedCFOParams
    val fftParams = fixedFFTParams
    val bitsBundleParams = hardBitsBundleParams
    val demodParams = hardDemodParams
    val viterbiParams = hardViterbiParams
  }

  it should "receive ofdm" in {
    //val trials = Seq(1)
    FixedRXTester(
      rxParams,
      trials) should be (true)
  }
}
