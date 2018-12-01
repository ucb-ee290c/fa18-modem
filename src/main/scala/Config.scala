package modem

import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.numbers._

trait TXParams[T<:Data, U<:Data] {
    val iqBundleParams: IQBundleParams[T]
    val cyclicPrefixParams: CyclicPrefixParams[T]
    val ifftParams: FFTParams[T]
    val firParams: RCFilterParams[T]
    val modulatorParams: ModFFTParams[T,U]
    val encoderParams: CodingParams[U]
    val serParams: BitsSerDesParams[U]
}

object FinalTxParams {
    def apply(width: Int, nfft: Int, nBitPerSymbol: Int): TXParams[FixedPoint, UInt] = {
        val fixedIQ = DspComplex(FixedPoint(width.W, (width-3).BP))
        val txParams = new TXParams[FixedPoint, UInt] {
            val iqBundleParams = IQBundleParams(fixedIQ)
            val cyclicPrefixParams = new CyclicPrefixParams[FixedPoint] {
                val protoIQ = fixedIQ
                val prefixLength = nfft/4
                val symbolLength = nfft
            }
	    val serParams = UIntBitsSerDesParams( dataWidth = 1,bitsWidth = 48,ratio = 48,maxVal = 1)

            val ifftParams = FixedFFTParams(dataWidth = width, twiddleWidth = width,
                                           numPoints = nfft, binPoint = width - 3)
            val modulatorParams = FixedModFFTParams(
                dataWidth=width,
		bitsWidth=48,
                twiddleWidth=width,
                numPoints=nfft,
                Ncbps=48,
                Nbpsc=1,
                binPoints=width-3
            )
            val firParams = FixedRCFilterParams(
                dataWidth = width,
                binaryPoint = width-3,
                alpha = 0.2,
                sampsPerSymbol = 4,
                symbolSpan = 2
            )
            val encoderParams = HardEncoding()
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
  val viterbiParams: CodingParams[T]
}

object FinalRxParams {
    def apply(width: Int, nfft: Int, nBitPerSymbol: Int): RXParams[FixedPoint, SInt, UInt] = {
        val fixedIQ = DspComplex(FixedPoint(width.W, (width-3).BP))
        val rxParams = new RXParams[FixedPoint, SInt, UInt] {
            val iqBundleParams = IQBundleParams(fixedIQ)
            val pktDetectParams = FixedPacketDetectParams(width)
            val cyclicPrefixParams = new CyclicPrefixParams[FixedPoint] {
                val protoIQ = fixedIQ
                val prefixLength = nfft/4
                val symbolLength = nfft
            }
            val equalizerParams = FixedEqualizerParams(width, binaryPoint=width-3,
                carrierMask=Seq.fill(1)(false) ++ Seq.fill(26)(true)  ++ Seq.fill(5)(false) ++ Seq.fill(6)(false) ++ Seq.fill(27)(true),
                nSubcarriers=nfft)
            val cfoParams = FixedCFOParams(width=1, iqWidth=width, stLength=160,
                                           ltLength=160, preamble=true, stagesPerCycle=1)
            val fftParams = FixedFFTParams(dataWidth = width, twiddleWidth = width,
                                           numPoints = nfft, binPoint = width - 3)
            val bitsBundleParams = BitsBundleParams(nBitPerSymbol, SInt(2.W))
            val demodParams = HardDemodParams(width=nfft, dataWidth=width, dataBinaryPoint=width - 3, bitsWidth=nBitPerSymbol, hsmod=1)
            val viterbiParams = FixedCoding()
        }
        rxParams
    }
}
