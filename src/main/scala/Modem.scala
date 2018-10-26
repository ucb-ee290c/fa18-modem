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
// trait HasPeripheryModem extends BaseSubsystem {
//   // instantiate chain blocks
// //  val cordicChain = LazyModule(new CordicThing(FixedCordicParams(8, 10)))
// //  val CFOChain = LazyModule(new CFOThing(FixedCFOParams(8, 10)))
// //  val packetDetectChain = LazyModule(new PacketDetectThing(FixedPacketDetectParams(8, 10)))
// //  val equalizerChain = LazyModule(new EqualizerThing(FixedEqualizerParams(8, 10)))
//   // connect memory interfaces to pbus
// //  pbus.toVariableWidthSlave(Some("modemWrite")) { modemChain.writeQueue.mem.get }
// //  pbus.toVariableWidthSlave(Some("modemRead")) { modemChain.readQueue.mem.get }
// }

class TX[T<:Data:Real:BinaryRepresentation](val params: TXParams[T]) extends Module {
  val io = IO(???)
}

class RX[T<:Data:Real:BinaryRepresentation](val params: RXParams[T]) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(IQBundle(params)))
    val out = Decoupled(DecodeBitsBundle(params))
  })

  val phaseRotator = Module( new PhaseRotator(params) )
  val pktDetect = Module( new PacketDetect(params) )
  val cfoEstimator = Module( new CFOCorrection(params) )
  val fft = Module( new FFT(params) )
  val eq = Module( new Equalizer(params) )
  val cfoPilot = Module( new CFOPilot(params) )
  val demod = Module( new Demodulator(params) )
  val decode = Module( new Decoder(params) )

  // Phase Rotation Block
  phaseRotator.io.inIQ := io.in
  phaseRotator.io.phiCorrect := cfoEstimator.phiCorrect

  // Packet Detector Block
  pktDetect.io.in := phaseRotator.io.outIQ

  // CFO Estimation
  cfoEstimator.io.in := pktDetect.io.out

  // FFT
  fft.io.in := cfoEstimator.io.out

  // EQ
  eq.io.in := fft.io.out

  // CFO Pilot Estimation
  cfoPilot.io.in := fft.io.out

  // Demodulator
  demod.io.in := eq.io.out

  // Decoder
  decode.io.in := demod.io.out

  io.out := decode.io.out
}

class Modem[T<:Data:Real:BinaryRepresentation](val params: ModemParams[T]) extends Module{
  val io = IO(???)


}
