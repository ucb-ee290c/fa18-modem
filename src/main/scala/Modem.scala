package modem

import chisel3._
// import chisel3.experimental.FixedPoint
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
// //  pbus.toVarbleWidthSlave(Some("modemRead")) { modemChain.readQueue.mem.get }
// }

class TX[T<:Data:Real:BinaryRepresentation, U<:Data](val txParams: TXParams[T, U]) extends Module {
  val io = IO(new Bundle{
    val in = Decoupled(Vec(36, UInt(1.W)))
    val out = Flipped(Decoupled(IQBundle(txParams.iqBundleParams)))
  })
  val encoder = Module( new Encoding(txParams.encoderParams) )
  // val modulator = Module( new Modulator(txParams.modulatorParams))
  val ifft = Module( new IFFT(txParams.ifftParams) )
  val cyclicPrefix = Module( new CyclicPrefix(txParams.cyclicPrefixParams) )
  val fir = Module( new RCFilter(txParams.firParams) )

  encoder.io.in <> io.in
  // modulator.io.in <> encoder.io.out
  // ifft.io.in <> modulator.io.in
  // cyclicPrefix.io.in <> ifft.io.out
  // fir.io.in <> cyclicPrefix.io.out
}

class RX[T<:Data:Real:BinaryRepresentation, U<:Data:Real:BinaryRepresentation, V<:Data:Real](
  rxParams: RXParams[T, U, V]
) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(IQBundle(rxParams.iqBundleParams)))
    //val out = Decoupled(BitsBundle(bitsBundleParams))
    val out = Decoupled(Vec(36, UInt(1.W)))
  })

  val phaseRotator = Module( new PhaseRotator(rxParams.cfoParams) )
  val pktDetect = Module( new PacketDetect(rxParams.pktDetectParams) )
  val cfoEstimator = Module( new CFOCorrection(rxParams.cfoParams) )
  val cyclicPrefix = Module( new CyclicPrefix(rxParams.cyclicPrefixParams) )
  val fft = Module( new FFT(rxParams.fftParams) )
  val eq = Module( new Equalizer(rxParams.equalizerParams) )
  // val cfoPilot = Module( new CFOPilot(cfoParams) )
  val demod = Module( new Demodulator(rxParams.demodParams) )
  val decode = Module( new ViterbiDecoder(rxParams.viterbiParams) )

  val dummyReg = Reg(Vec(48, SInt(2.W)))

  // Phase Rotation Block
  phaseRotator.io.inIQ <> io.in
  phaseRotator.io.phiCorrect := ConvertableTo[T].fromDouble(0) //cfoEstimator.phiCorrect

  // Packet Detector Block
  pktDetect.io.in <> phaseRotator.io.outIQ

  // CFO Estimation
  cfoEstimator.io.in <> pktDetect.io.out

  // CP Removal
  cyclicPrefix.io.in <> cfoEstimator.io.out
  cyclicPrefix.io.add := false.B

  // FFT
  fft.io.in <> cyclicPrefix.io.out

  // EQ
  eq.io.in <> fft.io.out

  // CFO Pilot Estimation
  // cfoPilot.io.in := fft.io.out

  // Demodulator
  demod.io.mod_ctrl := 0.U
  demod.io.in <> eq.io.out

  // Decoder
  decode.io.in_soft <> demod.io.out
  decode.io.in_hard := dummyReg

  io.out <> decode.io.out
}

trait HasPeripheryModem extends BaseSubsystem {
  // Instantiate rx chain
  val rxChain = LazyModule(new RXThing(FinalRxParams(16, 64, 48)))
  // Instantiate tx chain
  val txChain = LazyModule(new TXThing(FinalTxParams(16, 64, 48)))
  pbus.toVariableWidthSlave(Some("rxWrite")) { rxChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("rxRead")) { rxChain.readQueue.mem.get }
  pbus.toVariableWidthSlave(Some("txWrite")) { txChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("txRead")) { txChain.readQueue.mem.get }
}
// trait ModemParams[T<:Data, U<:Data] extends PacketBundleParams[T] with BitsBundleParams[U] {
//   val foo: Int
// }
//
// class Modem[T<:Data:Real:BinaryRepresentation, U<:Data](val params: ModemParams[T,U]) extends Module{
//   val io = IO(???)
//
//
// }
