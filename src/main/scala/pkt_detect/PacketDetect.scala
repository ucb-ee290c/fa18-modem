package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._


/**
 * Base class for PacketDetect parameters
 * Type generic
 */
trait PacketDetectParams[T <: Data] {
  val protoIQ: DspComplex[T]
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
  // binary point is iqWidth-3 to allow for some inflation
  val protoIQ = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP))
  val powerThreshVal = 0.75
  val correlationThreshVal = 0.75
  val correlationWindow = powerThreshWindow
  val correlationStride = 16
}

/**
  * Bundle type for debug data
  */
class PacketDetectDebugBundle[T <: Data](params: PacketDetectParams[T]) extends Bundle {
  val powerHigh: Bool = Bool()
  val powerLow: Bool = Bool()
  val corrComp: Bool = Bool()
  val iq: DspComplex[T] = params.protoIQ.cloneType
  val corrNum: T = params.protoIQ.real.cloneType
  val corrDenom: T = params.protoIQ.real.cloneType
  override def cloneType: this.type = PacketDetectDebugBundle(params).asInstanceOf[this.type]

}
object PacketDetectDebugBundle {
  def apply[T <: Data](params: PacketDetectParams[T]): PacketDetectDebugBundle[T] = new PacketDetectDebugBundle[T](params)
}

/**
  * Bundle type as IO for packet detect modules
  */
class PacketDetectIO[T <: Data](params: PacketDetectParams[T]) extends Bundle {
  val in = Flipped(Decoupled(IQBundle(IQBundleParams(params.protoIQ))))
  val out = Decoupled(PacketBundle(PacketBundleParams(1, params.protoIQ)))

  val debug = Output(PacketDetectDebugBundle[T](params))

  override def cloneType: this.type = PacketDetectIO(params).asInstanceOf[this.type]
}
object PacketDetectIO {
  def apply[T <: Data](params: PacketDetectParams[T]): PacketDetectIO[T] =
    new PacketDetectIO(params)
}

/**
  * Correlation for packet detect
  */
class Correlator[T <: Data : Real : BinaryRepresentation](params: PacketDetectParams[T]) extends Module {
  val windowSize = params.correlationStride + params.correlationWindow
  val io = IO(new Bundle {
    val data = Input(Vec(windowSize, params.protoIQ))
    val correlated = Output(Bool())

    val corrNum = Output(params.protoIQ.real.cloneType)
    val corrDenom = Output(params.protoIQ.real.cloneType)
  })
  // Squared magnitude of sum of correlations
  val corrNumVal = (0 until params.correlationWindow).map(
      i => io.data(i) * io.data(i + params.correlationStride).conj()
      ).map(x => x.div2(log2Ceil(params.correlationWindow))).
      reduce(_ + _).abssq()
  io.corrNum := corrNumVal
  // Squared magnitude (real)
  val corrDenomVal = (0 until params.correlationWindow).map(i => io.data(i).abssq()).
    map(x => x >> log2Ceil(params.correlationWindow)).reduce(_ + _)
  io.corrDenom := corrDenomVal
  if (params.correlationThreshVal == 0.75) {
    // Compare numerator to 0.75 denominator
    val corrComp = corrNumVal > ((corrDenomVal >> 1) + (corrDenomVal >> 2))
    io.correlated := corrComp
  } else {
    assert(false, "Correlation thresholds other than 0.75 not implemented")
  }
}
object Correlator {
  def apply[T <: Data : Real : BinaryRepresentation](data: Vec[DspComplex[T]], params: PacketDetectParams[T]): (Bool, T, T) = {
    val correlator = Module(new Correlator(params))
    correlator.io.data := data
    //correlator.io.correlated
    (correlator.io.correlated, correlator.io.corrNum, correlator.io.corrDenom)
  }
}

/**
  * Power thresholding for packet detect
  */
class PowerMeter[T <: Data : Real : BinaryRepresentation](params: PacketDetectParams[T]) extends Module {
  val io = IO(new Bundle {
    val data = Input(Vec(params.powerThreshWindow, params.protoIQ))
    val powerHigh = Output(Bool())
    val powerLow = Output(Bool())
  })
  val powers = (0 until params.powerThreshWindow).map(i => io.data(i).abssq()).map(p => p > Real[T].fromDouble(params.powerThreshVal))
  io.powerHigh := powers.reduce(_ & _)
  io.powerLow  := !powers.reduce(_ | _)
}
object PowerMeter {
  def apply[T <: Data : Real : BinaryRepresentation](data: Vec[DspComplex[T]], params: PacketDetectParams[T]): PowerMeter[T] = {
    val meter = Module(new PowerMeter(params))
    meter.io.data := data
    meter
  }
}

/**
  * PacketDetector
  * Takes IQ samples and uses some combination of power and correlation thresholding to detect the start and end
  * of packets
  */
class PacketDetect[T <: Data : Real : BinaryRepresentation](params: PacketDetectParams[T]) extends Module {
  val io = IO(PacketDetectIO(params))

  val windowSize = params.correlationStride + params.correlationWindow
  val complexZero = Wire(params.protoIQ)
  complexZero.real := Real[T].zero
  complexZero.imag := Real[T].zero

  // State machine states
  val sFlushing :: sNoPkt :: sNoPktValid :: sPkt :: sPktValid :: Nil = Enum(5)
  val state = RegInit(sFlushing)
  val nextState = Wire(sFlushing.cloneType)
  val flushCounter = RegInit(0.U(8.W))
  val isValid = state === sNoPktValid || state === sPktValid
  val isPkt = state === sPkt || state === sPktValid
  val isNextValid = nextState === sNoPktValid || nextState === sPktValid
  val isNextPkt = nextState === sPkt || nextState === sPktValid

  // Store inputs for processing
  val iqBuf = RegNext(io.in.bits.iq)
  val dataVec = Reg(Vec(windowSize, params.protoIQ))
  when(state === sFlushing) {
    dataVec(0) := complexZero
    for (i <- 1 until windowSize) {
      dataVec(i) := dataVec(i - 1)
    }
  }
  .elsewhen(state === sPktValid || state === sNoPktValid) {
    printf("bringing in new data\n")
    dataVec(0) := iqBuf
    for (i <- 1 until windowSize) {
      dataVec(i) := dataVec(i - 1)
    }
  }
  .otherwise {
    for (i <- 0 until windowSize) {
      dataVec(i) := dataVec(i)
    }
  }

  // Power Threshold
  val powerMeter = PowerMeter(VecInit(dataVec.drop(dataVec.length - params.powerThreshWindow)), params)
  val powerHigh = powerMeter.io.powerHigh
  val powerLow = powerMeter.io.powerLow

  // Correlation Threshold
  val corrComp = Wire(Bool())
  corrComp := true.B
  val corrNum = Wire(params.protoIQ.real.cloneType)
  corrNum := Real[T].zero
  val corrDenom = Wire(params.protoIQ.real.cloneType)
  corrDenom := Real[T].zero
  if (params.correlationThresh) {
    // Reverse dataVec to have denominator based on earlier-in-time data
//    corrComp := Correlator(Vec(dataVec.reverse), params)
    val (cmp, num, denom) = Correlator(VecInit(dataVec.reverse), params)
    corrComp := cmp
    corrNum := num
    corrDenom := denom
  }

  // State Update
  when (flushCounter < windowSize.U) {
    nextState := sFlushing // Do an initial flush of the registers
    flushCounter := flushCounter + 1.U
  }.otherwise {
    flushCounter := flushCounter
    when(io.in.valid) {
      when(powerHigh && corrComp) {
        nextState := sPktValid
      }.elsewhen(powerLow) {
        nextState := sNoPktValid
      }.elsewhen(isPkt) {
        nextState := sPktValid
      }.otherwise {
        nextState := sNoPktValid
      }
    }.otherwise {
      when(powerHigh && corrComp) {
        nextState := sPkt
      }.elsewhen(powerLow) {
        nextState := sNoPkt
      }.elsewhen(isPkt) {
        nextState := sPkt
      }.otherwise {
        nextState := sNoPkt
      }
    }
  }
  state := nextState

  // Output Logic
  io.in.ready := state =!= sFlushing
  // pktStart goes high on transition from nopkt to pkt
  val pktStartReg = Reg(Bool())
  val pktStopReg  = Reg(Bool())
  pktStartReg := !isPkt && isNextPkt
  pktStopReg := isPkt && !isNextPkt
  // Assign outputs
  io.out.bits.pktStart := pktStartReg
  io.out.bits.pktEnd  := pktStopReg
  io.out.bits.iq(0) := dataVec(dataVec.length - 1)
  io.out.valid := isNextPkt && isNextValid

  // debug output
  io.debug.corrComp := corrComp
  io.debug.corrNum := corrNum
  io.debug.corrDenom := corrDenom
  io.debug.powerHigh := powerHigh
  io.debug.powerLow := powerLow
  io.debug.iq := dataVec(windowSize-1)// dataVec(0) * dataVec(0).conj()
}
