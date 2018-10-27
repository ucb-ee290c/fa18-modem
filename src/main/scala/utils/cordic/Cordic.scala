package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

/**
 * Base class for CORDIC parameters
 *
 * These are type generic
 */
trait CordicParams[T <: Data] {
  val protoXY: T
  val protoZ: T
  val nStages: Int
  val correctGain: Boolean
  val stagesPerCycle: Int
}

/**
 * CORDIC parameters object for fixed-point CORDICs
 */
case class FixedCordicParams(
  // width of X and Y
  xyWidth: Int,
  // width of Z
  zWidth: Int,
  // scale output by correction factor?
  correctGain: Boolean = true,
  // number of CORDIC stages to perform per clock cycle
  stagesPerCycle: Int = 1,
) extends CordicParams[FixedPoint] {
  // prototype for x and y
  // binary point is (xyWidth-2) to represent 1.0 exactly
  val protoXY = FixedPoint(xyWidth.W, (xyWidth-3).BP)
  // prototype for z
  // binary point is (xyWidth-3) to represent Pi/2 exactly
  val protoZ = FixedPoint(zWidth.W, (zWidth-2).BP)
  val minNumber = math.pow(2.0, -(zWidth-2))
  // number of cordic stages
  private var n = 0
  while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
    n += 1
  }
  val nStages = n
}

/**
 * Bundle type that describes the input, state, and output of CORDIC
 */
class CordicBundle[T <: Data](params: CordicParams[T]) extends Bundle {
  val vectoring: Bool = Bool()
  val x: T = params.protoXY.cloneType
  val y: T = params.protoXY.cloneType
  val z: T = params.protoZ.cloneType

  override def cloneType: this.type = CordicBundle(params).asInstanceOf[this.type]
}
object CordicBundle {
  def apply[T <: Data](params: CordicParams[T]): CordicBundle[T] = new CordicBundle(params)
}

/*class VectoredCordicBundle[T <: Data](params: CordicParams[T]) extends CordicBundle[T](params) {
  val vectoring: Bool = Bool()

  override def cloneType: this.type = VectoredCordicBundle(params).asInstanceOf[this.type]
}
object VectoredCordicBundle {
  def apply[T <: Data](params: CordicParams[T]): VectoredCordicBundle[T] = new VectoredCordicBundle(params)
}*/
/**
 * Bundle type as IO for iterative CORDIC modules
 */
class IterativeCordicIO[T <: Data](params: CordicParams[T]) extends Bundle {
  val in = Flipped(Decoupled(CordicBundle(params)))
  val out = Decoupled(CordicBundle(params))

  // val vectoring = Input(Bool())

  override def cloneType: this.type = IterativeCordicIO(params).asInstanceOf[this.type]
}
object IterativeCordicIO {
  def apply[T <: Data](params: CordicParams[T]): IterativeCordicIO[T] =
    new IterativeCordicIO(params)
}

/**
 * Bundle type as IO for pipelined CORDIC modules
 * Same as iterative CORDIC IO but with valid interface instead of decoupled
 */
class PipelinedCordicIO[T <: Data](params: CordicParams[T]) extends Bundle {
  val in = Flipped(Valid(CordicBundle(params)))
  val out = Valid(CordicBundle(params))

  // val vectoring = Input(Bool())

  override def cloneType: this.type = PipelinedCordicIO(params).asInstanceOf[this.type]
}
object PipelinedCordicIO {
  def apply[T <: Data](params: CordicParams[T]): PipelinedCordicIO[T] =
    new PipelinedCordicIO(params)
}

object AddSub {
  def apply[T <: Data : Ring](sel: Bool, a: T, b: T): T = {
    Mux(sel, a + b, a - b)
  }
}

class CordicIter[T<:Data:Ring:BinaryRepresentation:ConvertableTo:Order](val params: CordicParams[T], stage: Int) extends Module{
    val io = IO(new Bundle{
        val inXYZ = Input(CordicBundle(params))
        //val vectoring = Input(Bool())
        val outXYZ = Output(CordicBundle(params))
    })
    val atan = ConvertableTo[T].fromDouble(Constants.arctan(stage+1).last, io.inXYZ.z) // arctan value
    val d = Mux(io.inXYZ.vectoring, io.inXYZ.y.signBit(), !io.inXYZ.z.signBit())
    if (params.correctGain && stage == params.nStages - 1){ // Apply gain correction to the last CORDIC iteration
      io.outXYZ.x := AddSub(!d, io.inXYZ.x, BinaryRepresentation[T].shr(io.inXYZ.y, (stage))) * ConvertableTo[T].fromDouble(1/Constants.gain(params.nStages), io.inXYZ.x)
      io.outXYZ.y := AddSub(d, io.inXYZ.y, BinaryRepresentation[T].shr(io.inXYZ.x, (stage))) * ConvertableTo[T].fromDouble(1/Constants.gain(params.nStages), io.inXYZ.y)
    } else{
      io.outXYZ.x := AddSub(!d, io.inXYZ.x, BinaryRepresentation[T].shr(io.inXYZ.y, (stage)))
      io.outXYZ.y := AddSub(d, io.inXYZ.y, BinaryRepresentation[T].shr(io.inXYZ.x, (stage)))
    }
    io.outXYZ.z := AddSub(!d, io.inXYZ.z, atan)
    io.outXYZ.vectoring := io.inXYZ.vectoring
}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */
// trait HasPeripheryCordic extends BaseSubsystem {
//   // instantiate cordic chain
//   val cordicChain = LazyModule(new CordicThing(FixedCordicParams(16, 16)))
//   // connect memory interfaces to pbus
//   pbus.toVariableWidthSlave(Some("cordicWrite")) { cordicChain.writeQueue.mem.get }
//   pbus.toVariableWidthSlave(Some("cordicRead")) { cordicChain.readQueue.mem.get }
// }

class IterativeCordic[T<:Data:Ring:BinaryRepresentation:ConvertableTo:Order](val params: CordicParams[T]) extends Module{
  val io = IO(IterativeCordicIO(params))
  val cycles = params.nStages/params.stagesPerCycle // Calculate number of pipeline stages
  val push = io.out.ready || (!io.out.valid && io.in.valid) // Pipeline is clear to advance if there is space in the pipeline or output is ready to accept data

  val stages = (0 until params.nStages).map{ i: Int => Module( new CordicIter(params, i) )} // Instatiate all the CORDIC iterations
  // stages.foreach{ iter => iter.io.vectoring := io.vectoring } // Connect vectoring io port for all the stages

  io.out.bits := stages.grouped(params.stagesPerCycle).foldLeft(io.in.bits){    // Group iterations into pipeline stage Seqs
    (prevStage, currStage) => RegEnable(currStage.foldLeft(prevStage){          // For each pipeline Seq, make a RegEnable to store value
      (prevIter, currIter) => {                                                 // Iterate through pipeline Seq to perform unrolling
        currIter.io.inXYZ := prevIter                                           // Connect each iteration to previous iteration output or previous pipeline register
        currIter.io.outXYZ                                                      // Return output of current stage
      }
    } , push)                                                                   // Enable signal for pipeline stages
  }

  io.out.valid := ShiftRegister(in=io.in.valid, n=cycles, resetData=false.B, en=push) // Shift Register to track ready/valids
  io.in.ready := push // Input is ready when pipeline is clear to advance
}

class PipelinedCordic[T<:Data:Ring:BinaryRepresentation:ConvertableTo:Order](val params: CordicParams[T]) extends Module{
  val io = IO(PipelinedCordicIO(params))
  val cycles: Int = params.nStages/params.stagesPerCycle // Calculate number of pipeline stages

  val stages = (0 until params.nStages).map{ i: Int => Module( new CordicIter(params, i) )} // Instatiate all the CORDIC iterations

  io.out.bits := stages.grouped(params.stagesPerCycle).foldLeft(io.in.bits){    // Group iterations into pipeline stage Seqs
    (prevStage, currStage) => Reg(currStage.foldLeft(prevStage){                // For each pipeline Seq, make a Reg to store value
      (prevIter, currIter) => {                                                 // Iterate through pipeline Seq to perform unrolling
        currIter.io.inXYZ := prevIter                                           // Connect each iteration to previous iteration output or previous pipeline register
        currIter.io.outXYZ                                                      // Return output of current stage
      }})
  }

  io.out.valid := ShiftRegister(in=io.in.valid, n=cycles, resetData=false.B, en=true.B) // Shift Register to track valids
}