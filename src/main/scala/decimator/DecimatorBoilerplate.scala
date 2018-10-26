package decimator

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

 trait DecimatorParams[T <: Data] extends PacketBundleParams[T] {
   val nDecimation: Int
 }

 class SerialPacketBundle[T <: Data](params: PacketBundleParams[T]) extends Bundle {
   val pktStart: Bool = Bool()
   val pktEnd: Bool = Bool()
   val iq: DspComplex[T] = params.protoIQ
 }
 object SerialPacketBundle {
   def apply[T <: Data](params: PacketBundleParams[T]): SerialPacketBundle[T] = new SerialPacketBundle(params)
 }
