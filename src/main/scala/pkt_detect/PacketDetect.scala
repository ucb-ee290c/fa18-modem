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
  val powerThreshVal: Double // Power threshold
  val powerThreshWindow: Int // Number of samples greater than power in a row before triggering
  val correlationThresh: Boolean
  val correlationThreshVal: Double
  val correlationWindow: Int // Number of strided correlations to sum
  val correlationStride: Int // Stride between correlated samples
}

/**
 * PacketDetect parameters for fixed-point data
 */
case class FixedPacketDetectParams(
  // width of I and Q
  iqWidth: Int,
  powerThreshWindow: Int = 4,
  // enable correlation thresholding?
  correlationThresh: Boolean = false
) extends PacketDetectParams[FixedPoint] {
  // prototype for iq
  // binary point is iqWidth-2 to allow for some inflation
  val protoIQ = FixedPoint(iqWidth.W, (iqWidth-2).BP)
  val powerThreshVal = 0.75
  val correlationThreshVal = 0.75
  val correlationWindow = 4
  val correlationStride = 16
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
  val windowSize = params.correlationStride + params.correlationWindow
  val dataVec = Vec.fill(windowSize)(RegInit(Real[T].zero)).foldLeft(io.in.bits){ case (a, b) => a := b; b }
  val validShift = ShiftRegister(io.in.valid, n=windowSize, resetData=false.B)

  // Correlation Threshold
  val corrComp := Wire(false.B)
  if (params.correlationThresh) {
    // Squared magnitude of sum of correlations
    val corrNumTmp = (0 until params.correlationWindow).map(i => dataVec(i) * dataVec(i + params.correlationStride).conjugate).reduce(_ + _)
    val corrNumVal = (corrNumTmp * corrNumTmp.conjugate).real
    // Squared magnitude
    val corrDenomVal = (0 until params.correlationWindow).map(i => dataVec(i) * dataVec(i).conjugate).reduce(_ + _).real
    if (params.correlationThreshVal == 0.75) {
      // Compare 0.75 numerate to denominator for the
      corrComp := ((corrNumVal >> 1) + (corrNumVal >> 2)) > corrDenomVal * corrDenomVal
    } else {
      assert(false, "Correlation thresholds other than 0.75 not implemented")
    }
  }
}


