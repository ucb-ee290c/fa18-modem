package cfo

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


trait PacketBundleParams[T <: Data] {
  val width: Int
  val protoIQ: DspComplex[T]
}

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

class PacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val iq: Vec[DspComplex[T]] = Vec(params.width, params.protoIQ)
}
object PacketBundle {
  def apply[T <: Data](params: PacketBundleParams[T]): PacketBundle[T] = new PacketBundle(params)
}

class CFOIO[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params)))
  val out = Decoupled(PacketBundle(params))

  override def cloneType: this.type = CFOIO(params).asInstanceOf[this.type]
}
object CFOIO {
  def apply[T <: Data](params: CFOParams[T]): CFOIO[T] =
    new CFOIO(params)
}

// class PreambleStateMachine[T<:Data](stLength: Int, ltLength: Int) extends Module{
//   val io = IO(new Bundle{
//       val pktStart = Input(Bool())
//       val pktEnd = Input(Bool())
//       val state = Output(UInt(2.W))
//   })
//
//   val curState = Reg(UInt(2.W))
//   val nexState = Wire(UInt(2.W))
//   val stCounter = Counter(stLength)
//   val ltCounter = Counter(ltLength)
//
//   val idle::st::lt::data = Enum(nodeType:UInt, n: 4)
//
//   switch(curState){
//     is(idle){
//       when(pktStart){
//         nexState := st
//       }.otherwise{
//         nexState := idle
//       }
//     }
//     is(st){
//       when(stCounter.inc()){
//         nexState := lt
//       }.otherwise{
//         nexState := st
//       }
//     }
//     is(lt){
//       when(ltCounter.inc()){
//         nexState := data
//       }.otherwise{
//         nexState := lt
//       }
//     }
//     is(data){
//       when(pktStop){
//         nexState := idle
//       }.otherwise{
//         nexState := data
//       }
//     }
//   }
//
//   io.state := curState
//   curState := nexState
// }

class PhaseRotator[T<:Data:Real:BinaryRepresentation](val params: CFOParams[T]) extends Module{
  val io = IO(new Bundle{
    val inIQ = Flipped(Decoupled(PacketBundle(params)))
    val outIQ = Decoupled(PacketBundle(params))
    val phiCorrect = Input(params.protoZ)
  })

  val cordic = Module( new IterativeCordic(params))

  cordic.io.in.bits.x := io.inIQ.bits.real
  cordic.io.in.bits.y := io.inIQ.bits.imag
  cordic.io.in.bits.z := io.phiCorrect



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

class CFOCorrection[T<:Data:Real:BinaryRepresentation](val params: CFOParams[T]) extends Module {
  // requireIsChiselType(params.protoIn)
  val io = IO(CFOIO(params))

  val cordic = Module ( new IterativeCordic(params))

  if(params.preamble == true){
    // val sm = Module( new PreambleStateMachine(params.stLength, params.ltLength) )
    val stDelay = 16
    val ltDelay = 64

    val curState = Reg(UInt(2.W))
    val nexState = Wire(UInt(2.W))
    val stMul = Wire(params.protoIQ)
    val stAcc = Reg(params.protoIQ)
    val stCounter = Counter(params.stLength)
    val ltCounter = Counter(params.ltLength)

    val delayIQByST = (0 until stDelay).foldLeft(io.in.bits.iq){(prev, curr) => RegNext(prev)}
    val delayIQByLT = (0 until ltDelay).foldLeft(io.in.bits.iq){(prev, curr) => RegNext(prev)}
    val delayValidByST = (0 until stDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev)}
    val delayValidByLT = (0 until ltDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev)}

    val idle::st::lt::data::Nil = Enum(4)

    switch(curState){
      is(idle){
        when(io.in.bits.pktStart && io.in.fire()){
          curState := st
        }.otherwise{
          curState := idle
        }
      }
      is(st){
        when(stCounter.inc()){

          curState := lt

        }.otherwise{
          curState := st
          stMul := (io.in.bits.iq * delayIQByST.conj)
          stAcc := stAcc + stMul
        }
      }
      is(lt){
        when(ltCounter.inc()){
          nexState := data
        }.otherwise{
          nexState := lt
        }
      }
      is(data){
        when(pktStop){
          nexState := idle
        }.otherwise{
          nexState := data
        }
      }
    }

  }

}
