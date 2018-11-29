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