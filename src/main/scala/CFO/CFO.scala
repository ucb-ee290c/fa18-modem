//// package cordic
//
//import chisel3._
//import chisel3.experimental.FixedPoint
//import chisel3.util._
//
//import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem
//
///**
// * Base class for CORDIC parameters
// *
// * These are type generic
// */
//trait CFOParams[T <: Data] {
//  val protoIn: DspComplex[T]
//  val protoout: DspComplex[T]
//  val mulPipe: Int
//  val addPipe: Int
//  val preamble: Boolean
//}
//
///**
// * CORDIC parameters object for fixed-point CORDICs
// */
//// case class FixedCordicParams(
////   // width of X and Y
////   xyWidth: Int,
////   // width of Z
////   zWidth: Int,
////   // scale output by correction factor?
////   correctGain: Boolean = true,
////   // number of CORDIC stages to perform per clock cycle
////   stagesPerCycle: Int = 1,
//// ) extends CordicParams[FixedPoint] {
////   // prototype for x and y
////   // binary point is (xyWidth-2) to represent 1.0 exactly
////   val protoXY = FixedPoint(xyWidth.W, (xyWidth-3).BP)
////   // prototype for z
////   // binary point is (xyWidth-3) to represent Pi/2 exactly
////   val protoZ = FixedPoint(zWidth.W, (zWidth-2).BP)
////   val minNumber = math.pow(2.0, -(zWidth-2))
////   // number of cordic stages
////   private var n = 0
////   while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
////     n += 1
////   }
////   val nStages = n
//// }
//
///**
// * Bundle type that describes the input, state, and output of CORDIC
// */
//// class CFOBundle[T <: Data](params: CFOParams[T]) extends Bundle {
////   val i: T = params.protoIQ.cloneType
////   val q: T = params.protoIQ.cloneType
////
////   override def cloneType: this.type = CordicBundle(params).asInstanceOf[this.type]
//// }
//// object CFOBundle {
////   def apply[T <: Data](params: CFOParams[T]): CFOBundle[T] = new CFOBundle(params)
//// }
//
///**
// * Bundle type as IO for iterative CORDIC modules
// */
//class CFOIO[T <: Data](params: CFOParams[T]) extends Bundle {
//  val in = Flipped(Decoupled(params.protoIn))
//  val out = Decoupled(params.protoOut)
//
//  val stPreamble = Bool()
//  val ltPreamble = Bool()
//
//  override def cloneType: this.type = CFOIO(params).asInstanceOf[this.type]
//}
//object CFOIO {
//  def apply[T <: Data](params: CFOParams[T]): CFOIO[T] =
//    new CFOIO(params)
//}
//
//object AddSub {
//  def apply[T <: Data : Ring](sel: Bool, a: T, b: T): T = {
//    Mux(sel, a + b, a - b)
//  }
//}
//
//
///**
//  * Mixin for top-level rocket to add a PWM
//  *
//  */
//// trait HasPeripheryCFO extends BaseSubsystem {
////   // instantiate cordic chain
////   val cordicChain = LazyModule(new CordicThing(FixedCFOParams(8, 10)))
////   // connect memory interfaces to pbus
////   pbus.toVariableWidthSlave(Some("cordicWrite")) { cordicChain.writeQueue.mem.get }
////   pbus.toVariableWidthSlave(Some("cordicRead")) { cordicChain.readQueue.mem.get }
//// }
//
//class CFOCorrection(val params: CFOParams[FixedPoint]) extends Module {
//  requireIsChiselType(params.protoIn)
//  val io = IO(CFOIO(params))
//
//  if(params.hasPreamble == true){
//
//  }
//
//}
