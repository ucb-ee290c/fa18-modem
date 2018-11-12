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

class IQBundle[T <: Data](params: IQBundleParams[T]) extends Bundle {
  val iq: DspComplex[T] = params.protoIQ.cloneType

  override def cloneType: this.type = IQBundle(params).asInstanceOf[this.type]
}
object IQBundle {
  def apply[T <: Data](params: IQBundleParams[T]): IQBundle[T] = new IQBundle[T](params)
  def apply[T <: Data](proto: DspComplex[T]): IQBundle[T] = new IQBundle[T](IQBundleParams[T](proto))
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

class PacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val iq: Vec[DspComplex[T]] = Vec(params.width, params.protoIQ)

  override def cloneType: this.type = PacketBundle(params).asInstanceOf[this.type]
}
object PacketBundle {
  def apply[T <: Data](params: PacketBundleParams[T]): PacketBundle[T] = new PacketBundle[T](params)
  def apply[T <: Data](size: Int, proto: DspComplex[T]): PacketBundle[T] = new PacketBundle[T](PacketBundleParams[T](size, proto))
}


 class SerialPacketBundle[T <: Data](val params: PacketBundleParams[T]) extends Bundle {
   val pktStart: Bool = Bool()
   val pktEnd: Bool = Bool()
   val iq: DspComplex[T] = params.protoIQ
 }
 object SerialPacketBundle {
   def apply[T <: Data](params: PacketBundleParams[T]): SerialPacketBundle[T] = new SerialPacketBundle(params)
 }
