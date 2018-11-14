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

class RX[T<:Data:Real:BinaryRepresentation, U<:Data](
  val iqBundleParams: IQBundleParams[T],
  val pktDetectParams: PacketDetectParams[T],
  val equalizerParams: EqualizerParams[T],
  val cfoParams: CFOParams[T],
  val fftParams: FFTParams[T],
  val bitsBundleParams: BitsBundleParams[U],
  val demodParams: DemodulationParams[U],
  val viterbiParams: ViterbiParams[U],
) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(IQBundle(iqBundleParams)))
    val out = Decoupled(BitsBundle(bitsBundleParams))
  })

  val phaseRotator = Module( new PhaseRotator(cfoParams) )
  val pktDetect = Module( new PacketDetect(pktDetectParams) )
  val cfoEstimator = Module( new CFOCorrection(cfoParams) )
  val fft = Module( new FFT(fftParams) )
  val eq = Module( new Equalizer(equalizerParams) )
  // val cfoPilot = Module( new CFOPilot(cfoParams) )
  val demod = Module( new Demodulator(demodParams) )
  val decode = Module( new Decoder(viterbiParams) )

  // Phase Rotation Block
  phaseRotator.io.inIQ := io.in
  phaseRotator.io.phiCorrect := ConvertableTo[T].fromDouble(0) //cfoEstimator.phiCorrect

  // Packet Detector Block
  pktDetect.io.in := phaseRotator.io.outIQ

  // CFO Estimation
  cfoEstimator.io.in := pktDetect.io.out

  // FFT
  fft.io.in := cfoEstimator.io.out

  // EQ
  eq.io.in := fft.io.out

  // CFO Pilot Estimation
  // cfoPilot.io.in := fft.io.out

  // Demodulator
  demod.io.in := eq.io.out

  // Decoder
  decode.io.in := demod.io.out

  io.out := decode.io.out
}

class Modem[T<:Data:Real:BinaryRepresentation](val params: ModemParams[T]) extends Module{
  val io = IO(???)


}
