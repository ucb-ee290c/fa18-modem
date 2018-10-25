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
trait CFOParams[T <: Data] {
  val protoIn: DspComplex[T]
  val protoout: DspComplex[T]
  val mulPipe: Int
  val addPipe: Int
  val preamble: Boolean
}

trait PacketBundleParams[T <: Data] {
  val width: Int
  val protoIQ: DspComplex[T]
}

class PacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
  val iq: Vec[DspComplex[T]] = Vec(params.width, params.protoIQ)
}

class CFOIO[T <: Data](params: CFOParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params.PacketBundleParams)))
  val out = Decoupled(PacketBundle(params.PacketBundleParams))

  override def cloneType: this.type = CFOIO(params).asInstanceOf[this.type]
}
object CFOIO {
  def apply[T <: Data](params: CFOParams[T]): CFOIO[T] =
    new CFOIO(params)
}

object AddSub {
  def apply[T <: Data : Ring](sel: Bool, a: T, b: T): T = {
    Mux(sel, a + b, a - b)
  }
}

class PreambleStateMachine(val stf: Boolean, val ltf Boolean, val frameLength: Int) extends Module{
  val io = IO(new Bundle{
      val pktStart = Input(Bool())
      val pktEnd = Input(Bool())
      val state = Output(UInt(3.W))
  })

  val curState = Reg(UInt(3.W))
  val nexState = Wire(UInt(3.W))

  val idle::st::lt::data = Enum(nodeType:UInt, n: 4)

  switch(curState){
    is(idle){
      when(pktStart){
        nexState := st
      }.otherwise{
        nexState := idle
      }
    }
    is(st){
      when(pktStop){
        nexState := lt
      }.otherwise{
        nexState := st
      }
    }
    is(lt){
      when(pktStop){
        nexState := data
      }.otherwise{
        nexState := lt
      }
    }
    is(data){

    }
  }

  io.state := curState
  curState := nexState

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

class CFOCorrection(val params: CFOParams[FixedPoint]) extends Module {
  requireIsChiselType(params.protoIn)
  val io = IO(CFOIO(params))



  if(params.preamble == true){



  }

}
