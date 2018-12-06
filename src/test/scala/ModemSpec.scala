package modem

import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex, pow}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{randomDouble}
import

case class testTXVecs(){
  val ssv = SSVConverter
  val csv = CSVConverter
  val txInfo = ssv.toIntSeq("./src/test/scala/TX/OFDM_TX_bit_symbols_Len.txt")
  val modType = txInfo(2)
  // val signalVec = BigInt("d30080",16).toString(2).split("").map{_.toInt}
  // val serviceVec = BigInt("f0f0f0",16).toString(2).split("").map{_.toInt}
  val signalVec = BigInt("d30080",16).toInt
  val serviceVec = BigInt("f0f0f0",16).toInt
  val bits = ssv.toInSeq("./src/test/scala/TX/OFDM_TX_bit_symbols.txt")

  if (modType == 0){
    // QPSK
    val mod_ctrl = 1
  } else if (modType == 1){
    // BPSK
    val mod_ctrl = 0
  } else if (modType == 2){
    // 16QAM
    val mod_ctrl = 2
  } else if (modType == 3){
    // 64QAM
    // val mod_ctrl = 3
  }

  val expectIQ = csv.toComplex("./src/test/scala/TX/OFDM_TX_sim_out_i.txt", "./src/test/scala/TX/OFDM_TX_sim_out_q.txt")
  val txbits = signalVec :+ serviceVec :+ bits

  val isHeadVec = 1 +: Seq.fill(txbits.length/24 - 1)(0)
  val puncMatrix = BigInt("d",16).toString(2).split("").map{_.toInt}

  val groupedTXBits = txbits.grouped(24)
  val groupedTXInts = groupedTXBits.map{x => x.reverse.zipWithIndex.map{case(x,idx) => pow(2,idx) * x}.reduce{_+_}}
}


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
