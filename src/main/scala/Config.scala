package modem

import chisel3._
import dsptools.numbers._


/**
  * Bundle type for IQ data
  */
trait IQBundleParams[T <: Data] {
  val protoIQ: DspComplex[T]
}
object IQBundleParams {
  def apply[T <: Data](proto: DspComplex[T]): IQBundleParams[T] = new IQBundleParams[T] { val protoIQ = proto }
}
/**
 * Bundle type for packetized IQ data
 */
trait PacketBundleParams[T <: Data] {
  val width: Int
  val protoIQ: DspComplex[T]
}
object PacketBundleParams {
  def apply[T <: Data](size: Int, proto: DspComplex[T]): PacketBundleParams[T] = {
     new PacketBundleParams[T] {
      val width = size
      val protoIQ = proto
    }
  }
}
