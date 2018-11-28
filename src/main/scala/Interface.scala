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

trait BitsBundleParams[T<:Data] {
  val bitsWidth: Int
  val protoBits: T
}
object BitsBundleParams {
   def apply[T <: Data](width: Int, proto: T): BitsBundleParams[T] = new BitsBundleParams[T] { val bitsWidth = width; val protoBits = proto }
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
 * Bundle type for serialized PacketBundle
 */
class SerialPacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val iq: DspComplex[T] = params.protoIQ.cloneType

  override def cloneType: this.type = SerialPacketBundle(params).asInstanceOf[this.type]
}
object SerialPacketBundle {
  def apply[T <: Data](params: PacketBundleParams[T]): SerialPacketBundle[T] = new SerialPacketBundle(params)
}
/**
 * Bundle type for deserialized PacketBundle
 */
class DeserialPacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val iq: Vec[DspComplex[T]] = Vec(params.width, params.protoIQ.cloneType)

  override def cloneType: this.type = DeserialPacketBundle(params).asInstanceOf[this.type]
}
object DeserialPacketBundle {
  def apply[T <: Data](params: PacketBundleParams[T]): PacketBundle[T] = new PacketBundle[T](params)
  def apply[T <: Data](size: Int, proto: DspComplex[T]): PacketBundle[T] = new PacketBundle[T](PacketBundleParams[T](64, proto))
}
/**
 * Bundle type for codewords from demod/deinterleaver
 */
class BitsBundle[T<:Data](params: BitsBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  //SInt(2.W)
  val bits : Vec[T] = Vec(params.bitsWidth, params.protoBits.cloneType)
  override def cloneType: this.type = BitsBundle(params).asInstanceOf[this.type]
}
object BitsBundle {
  def apply[T<:Data](params: BitsBundleParams[T]): BitsBundle[T] = new BitsBundle[T](params)
  def apply[T<:Data](bitsWidth: Int, protoBits: T): BitsBundle[T] = new BitsBundle[T](BitsBundleParams[T](bitsWidth, protoBits))
  //def apply[T<:Data](bitsWidth: Int): BitsBundle[T] = new BitsBundle[T](BitsBundleParams[T](bitsWidth))

}
