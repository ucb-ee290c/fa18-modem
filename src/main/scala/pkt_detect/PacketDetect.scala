package modem

import chisel3._
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


