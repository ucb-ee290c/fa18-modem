package modem

import chisel3._
import chisel3.core.IO
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem


/**
 * Base class for PacketDetect parameters
 * Type generic
 */
trait PacketDetectParams[T <: Data] {
  val protoIQ: T
  val powerThresh: Boolean
  val correlationThresh: Boolean
  val powerThreshVal: Double
  val correlationThreshVal: Double
}

/**
 * PacketDetect parameters for fixed-point data
 */
case class FixedPacketDetectParams(
  // width of I and Q
  iqWidth: Int,
  // enable power thresholding?
  powerThresh: Boolean = true,
  // enable correlation thresholding?
  correlationThresh: Boolean = false
) extends PacketDetectParams[FixedPoint] {
  // prototype for iq
  // binary point is iqWidth-2 to allow for some inflation
  val protoIQ = FixedPoint(iqWidth.W, (iqWidth-2).BP)
  val powerThreshVal = 0.75
  val correlationThreshVal = 0.9
}

/**
  * Bundle type that describes the input, state, and output of PacketDetect
  */
class PacketDetectBundle[T <: Data](params: PacketDetectParams[T]) extends Bundle {
  val iq: T = params.protoIQ.cloneType

  override def cloneType: this.type = PacketDetectBundle(params).asInstanceOf[this.type]
}
object PacketDetectBundle {
  def apply[T <: Data](params: PacketDetectParams[T]): PacketDetectBundle[T] = new PacketDetectBundle[T](params)
}

/**
  * Bundle type as IO for packet detect modules
  */
class PacketDetectIO[T <: Data](params: PacketDetectParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketDetectBundle(params)))
  val out = Decoupled(PacketDetectBundle(params))

  val pktStart = Output(Bool())

  override def cloneType: this.type = PacketDetectIO(params).asInstanceOf[this.type]
}
object PacketDetectIO {
  def apply[T <: Data](params: PacketDetectParams[T]): PacketDetectIO[T] =
    new PacketDetectIO(params)
}

/**
  * PacketDetector
  * Takes IQ samples and uses some combination of power and correlation thresholding to detect the start and end
  * of packets
  */
class PacketDetect[T <: Data : Real : BinaryRepresentation](params: PacketDetectParams[T]) {
  val io = IO(PacketDetectIO(params))
  val dataShift = ShiftRegister(io.in.bits, n=16, resetData=Real[T].zero, en=io.in.valid)
  val validShift = ShiftRegister(io.in.valid, n=16, resetData=false.B)
  
}


