package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint
import dsptools.numbers._

trait TXParams[T<:Data] {
    val iqBundleParams: IQBundleParams[T]
}

object FinalTxParams {
    def apply(width: Int): TXParams[FixedPoint] = {
        val fixedIQ = DspComplex(FixedPoint(width.W, (width-3).BP))
        val txParams = new TXParams[FixedPoint] {
            val iqBundleParams = IQBundleParams(fixedIQ)
        }
        txParams
    }
}

trait RXParams[T<:Data, U<:Data, V<:Data] {
  val iqBundleParams: IQBundleParams[T]
  val pktDetectParams: PacketDetectParams[T]
  val cyclicPrefixParams: CyclicPrefixParams[T]
  val equalizerParams: EqualizerParams[T]
  val cfoParams: CFOParams[T]
  val fftParams: FFTParams[T]
  val bitsBundleParams: BitsBundleParams[U]
  val demodParams: DemodulationParams[T,U]
  val viterbiParams: CodingParams[V]
}

object FinalRxParams {
    def apply(width: Int): RXParams[FixedPoint, UInt, UInt] = {
        val fixedIQ = DspComplex(FixedPoint(width.W, (width-3).BP))
        val rxParams = new RXParams[FixedPoint, UInt, UInt] {
            val iqBundleParams = IQBundleParams(fixedIQ)
            val pktDetectParams = FixedPacketDetectParams(width)
            val cyclicPrefixParams = new CyclicPrefixParams[FixedPoint] {
                val protoIQ = fixedIQ
                val prefixLength = 16
                val symbolLength = 64
            }
            val equalizerParams = FixedEqualizerParams(width)
            val cfoParams = FixedCFOParams(width=1, iqWidth=width, stLength=160,
                                           ltLength=160, preamble=true, stagesPerCycle=1)
            val fftParams = FixedFFTParams(dataWidth = width, twiddleWidth = width,
                                           numPoints = 2, binPoint = 3)
            val bitsBundleParams = BitsBundleParams(48, UInt(1.W))
            val demodParams = HardDemodParams(width=64, datawidth=width, bitsWidth=48,
                                              Nbpsc=1, Ncbps=48, hsmod=1)
            val viterbiParams = FixedCoding()
        }
        rxParams
    }
}