package modem

import chisel3._

trait TXParams[T<:Data] {
    val width: Int
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

case class FinalRxParams {
    def apply(width: Int): RXParams[FixedPoint, UInt, UInt] = {
        val protoIQ = DspComplex(FixedPoint(width.W, (width-3).BP))
        val rxParams = new RXParams[FixedPoint, UInt, UInt] {
            val iqBundleParams = IQBundleParams(protoIQ)
            val pktDetectParams = FixedPacketDetectParams(width)
            val cyclicPrefixParams = new CyclicPrefixParams[FixedPoint] {
                val protoIQ = protoIQ
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