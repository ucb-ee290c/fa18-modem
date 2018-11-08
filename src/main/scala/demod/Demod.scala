package modem

import chisel3._
import dsptools.numbers._


trait CordicParams[T<:Data] extends PacketBundleParams[T]{
  val protoXY: T
  val protoZ: T
  val nStages: Int
  val correctGain: Boolean
  val stagesPerCycle: Int
}

trait CFOParams[T <: Data] extends CordicParams[T]{
  val stLength: Int
  val ltLength: Int
  val preamble: Boolean
}

class SerialPacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val iq: DspComplex[T] = params.protoIQ
}
object SerialPacketBundle {
  def apply[T <: Data](params: PacketBundleParams[T]): SerialPacketBundle[T] = new SerialPacketBundle(params)
}

class CFOIO[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(IQBundle(params)))
  val out = Decoupled(IQBundle(params))

  override def cloneType: this.type = CFOIO(params).asInstanceOf[this.type]
}
object CFOIO {
  def apply[T <: Data](params: PacketBundleParams[T]): CFOIO[T] =
    new CFOIO(params)
}


class PhaseRotator[T<:Data:Real:BinaryRepresentation](val params: CFOParams[T]) extends Module{
  val io = IO(new Bundle{
    val inIQ = Flipped(Decoupled(SerialPacketBundle(params)))
    val outIQ = Decoupled(SerialPacketBundle(params))
    val phiCorrect = Input(params.protoZ)
  })

  val cordic = Module( new IterativeCordic(params))

  io.outIQ = io.inIQ
  // cordic.io.in.bits.x := io.inIQ.bits.iq.real
  // cordic.io.in.bits.y := io.inIQ.bits.iq.imag
  // cordic.io.in.bits.z := io.phiCorrect
  // cordic.io.in.bits.vectoring := false.B
  // cordic.io.in.valid := true.B
  // io.inIQ.ready := cordic.io.in.ready
  // io.outIQ.bits.iq.real := cordic.io.out.bits.x
  // io.outIQ.bits.iq.imag := cordic.io.out.bits.y
  // io.outIQ.valid := cordic.io.out.valid
}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */
// trait HasPeripheryCFO extends BaseSubsystem {
//   // instantiate cordic chain
//   val cordicChain = LazyModule(new CordicThing(FixedCFOParams(8, 10)))
//   // connect memory interfaces to pbus
//   pbus.toVariableWidthSlave(Some("cordicWrite")) { cordicChain.writeQueue.mem.get }
//   pbus.toVariableWidthSlave(Some("cordicRead")) { cordicChain.readQueue.mem.get }
// }

class CFOCorrection[T<:Data:Real:BinaryRepresentation:ConvertableTo](val params: CFOParams[T]) extends Module {
  // requireIsChiselType(params.protoIn)
  val io = IO(CFOIO(params))

  val cordic = Module ( new IterativeCordic(params))
  val phaseCorrect = Module( new PhaseRotator(params))

  io.out := io.in
  // phaseCorrect.io.inIQ.bits.iq := io.in.bits.iq
  // io.out.bits.iq := phaseCorrect.io.outIQ.bits.iq
  // io.out.bits.pktStart := io.in.bits.pktStart
  // io.out.bits.pktEnd := io.in.bits.pktEnd

  // if(params.preamble == true){
  //   // val sm = Module( new PreambleStateMachine(params.stLength, params.ltLength) )
  //   val stDelay = 16
  //   val ltDelay = 64
  //
  //   val curState = Reg(UInt(2.W))
  //   val coarseOffset = RegInit(params.protoZ, ConvertableTo[T].fromDouble(0))
  //   val fineOffset = RegInit(params.protoZ, ConvertableTo[T].fromDouble(0))
  //
  //   val stMul = Wire(params.protoIQ)
  //   val stAcc = Reg(params.protoIQ)
  //   val ltMul = Wire(params.protoIQ)
  //   val ltAcc = Reg(params.protoIQ)
  //   val stCounter = Counter(params.stLength)
  //   val ltCounter = Counter(params.ltLength)
  //
  //   val estimatorReady = Wire(Bool())
  //   val rotatorReady = phaseCorrect.io.inIQ.ready
  //
  //   val delayIQByST = (0 until stDelay).foldLeft(io.in.bits.iq){(prev, curr) => RegNext(prev)}
  //   val delayIQByLT = (0 until ltDelay).foldLeft(io.in.bits.iq){(prev, curr) => RegNext(prev)}
  //   val delayValidByST = (0 until stDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev)}
  //   val delayValidByLT = (0 until ltDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev)}
  //
  //   val idle::st::lt::data::Nil = Enum(4)
  //
  //   io.in.ready := estimatorReady && rotatorReady
  //   io.out.valid := phaseCorrect.io.outIQ.valid
  //   phaseCorrect.io.outIQ.ready := io.out.ready
  //   phaseCorrect.io.phiCorrect := coarseOffset + fineOffset
  //
  //   switch(curState){
  //     is(idle){
  //       when(io.in.bits.pktStart && io.in.fire()){
  //         curState := st
  //       }.otherwise{
  //         curState := idle
  //         // stAcc := DspComplex[T](0,0)
  //       }
  //     }
  //     is(st){
  //       when(stCounter.inc()){
  //         cordic.io.in.bits.x := stAcc.real
  //         cordic.io.in.bits.y := stAcc.imag
  //         cordic.io.in.bits.z := ConvertableTo[T].fromDouble(0)
  //         cordic.io.in.bits.vectoring := true.B
  //         cordic.io.in.valid := true.B
  //         estimatorReady := false.B
  //         cordic.io.out.ready := true.B
  //       }.elsewhen(io.in.fire()){
  //         curState := st
  //         when(delayValidByST){
  //           stMul := (io.in.bits.iq * delayIQByST.conj())
  //           stAcc := stAcc + stMul
  //         }
  //       }.otherwise{
  //         cordic.io.in.valid := false.B
  //         when(cordic.io.out.valid){
  //           coarseOffset := cordic.io.out.bits.z * ConvertableTo[T].fromDouble(1/stDelay)
  //           estimatorReady := true.B
  //           curState := lt
  //         }
  //       }
  //     }
  //     is(lt){
  //       when(ltCounter.inc()){
  //         cordic.io.in.bits.x := ltAcc.real
  //         cordic.io.in.bits.y := ltAcc.imag
  //         cordic.io.in.bits.z := ConvertableTo[T].fromDouble(0)
  //         cordic.io.in.bits.vectoring := true.B
  //         cordic.io.in.valid := true.B
  //         estimatorReady := false.B
  //         cordic.io.out.ready := true.B
  //       }.elsewhen(io.in.fire()){
  //         curState := lt
  //         when(delayValidByLT){
  //           ltMul := (io.in.bits.iq * delayIQByLT.conj())
  //           ltAcc := stAcc + stMul
  //         }
  //       }
  //       .otherwise{
  //         cordic.io.in.valid := false.B
  //         when(cordic.io.out.valid){
  //           fineOffset := cordic.io.out.bits.z * ConvertableTo[T].fromDouble(1/ltDelay)
  //           estimatorReady := true.B
  //           curState := data
  //         }
  //       }
  //     }
  //     is(data){
  //       when(io.in.bits.pktEnd){
  //         curState := idle
  //       }.otherwise{
  //         curState := data
  //       }
  //     }
  //   }
  //
  // }

}
