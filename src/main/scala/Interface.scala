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

trait DemodBundleParams[T<:Data]{
  val decodeWidth: Int
  val protoBits: T
}
object DemodBundleParams {
  def apply[T <: Data](width: Int, proto: T): DemodBundleParams[T] = new DemodBundleParams[T] { val decodeWidth = width; val protoBits = proto }
}


//=======================================================================================================================================================
/**
  * Bundle type for IQ data
  */
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
class PacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val iq: Vec[DspComplex[T]] = Vec(params.width, params.protoIQ.cloneType)

  override def cloneType: this.type = PacketBundle(params).asInstanceOf[this.type]
}
object PacketBundle {
  def apply[T <: Data](params: PacketBundleParams[T]): PacketBundle[T] = new PacketBundle[T](params)
  def apply[T <: Data](size: Int, proto: DspComplex[T]): PacketBundle[T] = new PacketBundle[T](PacketBundleParams[T](size, proto))
}
/**
 * Bundle type for codewords from demod/deinterleaver
 */
class DemodBundle[T<:Data](params: DemodBundleParams[T]) extends Bundle{
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val bits : Vec[T] = Vec(params.decodeWidth, params.protoBits.cloneType)

  override def cloneType: this.type = DemodBundle(params).asInstanceOf[this.type]
}
object DemodBundle {
  def apply[T<:Data](params: DemodBundleParams[T]): DemodBundle[T] = new DemodBundle[T](params)
  def apply[T<:Data](decodeWidth: Int, protoBits: T): DemodBundle[T] = new DemodBundle[T](DemodBundleParams[T](decodeWidth, protoBits))
}
