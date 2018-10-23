package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

/**
  * Mixin for top-level rocket to add a modem
  *
  */
trait HasPeripheryModem extends BaseSubsystem {
  // instantiate chain blocks
  val cordicChain = LazyModule(new CordicThing(FixedCordicParams(8, 10)))
  val CFOChain = LazyModule(new CFOThing(FixedCFOParams(8, 10)))
  val packetDetectChain = LazyModule(new PacketDetectThing(FixedPacketDetectParams(8, 10)))
  val equalizerChain = LazyModule(new EqualizerThing(FixedEqualizerParams(8, 10)))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("modemWrite")) { modemChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("modemRead")) { modemChain.readQueue.mem.get }
}

