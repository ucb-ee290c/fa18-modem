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
    val in = Flipped(Decoupled(new Bundle{
      val data = BitsBundle(48, UInt(1.W))
      val mod_ctrl = UInt(2.W)
      val isHead = Bool()
      val puncMatrix  = Vec(4, UInt(1.W))
    }))
    val out = Decoupled(IQBundle(txParams.iqBundleParams))
  })
  val serilizer = Module( new BitsSerializer(txParams.serParams) )
  val encoder = Module( new Encoding(txParams.encoderParams) )
  val modulator = Module( new Modulator(txParams.modulatorParams))
  val ifft = Module( new IFFT(txParams.ifftParams) )
  val cyclicPrefix = Module( new CyclicPrefix(txParams.cyclicPrefixParams) )
  val preambleAdder = Module( new PreambleAdder(txParams.preambleParams) )
  val fir = Module( new RCFilter(txParams.firParams) )

  val puncMatrixBuf = RegNext(next = io.in.bits.puncMatrix, enable = io.in.fire())
  val isHeadBuf = RegNext(next = io.in.bits.isHead, enable = io.in.fire())
  val mod_ctrlBuf = RegNext(next = io.in.bits.mod_ctrl, enable = io.in.fire())

  //encoder.io.in <> io.in
  serilizer.io.in.bits.bits:= io.in.bits.data.bits
  serilizer.io.in.bits.pktStart := io.in.bits.data.pktStart
  serilizer.io.in.bits.pktEnd := io.in.bits.data.pktEnd
  serilizer.io.in.valid := io.in.valid
  serilizer.io.out.ready := encoder.io.in.ready
  encoder.io.in.bits  := serilizer.io.out.bits.bits(0)
  encoder.io.pktStartIn  := serilizer.io.out.bits.pktStart
  encoder.io.pktEndIn  := serilizer.io.out.bits.pktEnd
  encoder.io.in.valid := serilizer.io.out.valid
  encoder.io.mac.isHead := isHeadBuf
  encoder.io.mac.puncMatrix := puncMatrixBuf
  modulator.io.in <> encoder.io.out

  modulator.io.mod_ctrl := mod_ctrlBuf
  //ifft.io.in <> modulator.io.in
  val z0 = Ring[T].zero
  ifft.io.in.valid := modulator.io.out.valid
  ifft.io.out.ready := modulator.io.in.ready
  ifft.io.in.bits.pktStart := modulator.io.in.bits.pktStart
  ifft.io.in.bits.pktEnd := modulator.io.in.bits.pktEnd

   // ADD PILOT default:0
   ifft.io.in.bits.iq(7).real := z0
   ifft.io.in.bits.iq(7).imag := z0
   ifft.io.in.bits.iq(21).real := z0
   ifft.io.in.bits.iq(21).imag := z0
   ifft.io.in.bits.iq(43).real := z0
   ifft.io.in.bits.iq(43).imag := z0
   ifft.io.in.bits.iq(57).real := z0
   ifft.io.in.bits.iq(57).imag := z0
   // subcarrier allocate
   ifft.io.in.bits.iq(0).real := z0
   ifft.io.in.bits.iq(0).imag := z0
   for (i <- 1 until 7){
     ifft.io.in.bits.iq(i) := modulator.io.out.bits.iq(i+23)
   }
   for (i <- 8 until 21){
     ifft.io.in.bits.iq(i) := modulator.io.out.bits.iq(i+22)
   }
   for (i <- 22 until 27){
     ifft.io.in.bits.iq(i) := modulator.io.out.bits.iq(i+21)
   }
   for (i <- 27 until 38){
     ifft.io.in.bits.iq(i).real := z0
     ifft.io.in.bits.iq(i).imag := z0
   }
   for (i <- 38 until 43){
     ifft.io.in.bits.iq(i) := modulator.io.out.bits.iq(i-38)
   }
   for (i <- 44 until 57){
     ifft.io.in.bits.iq(i) := modulator.io.out.bits.iq(i-39)
   }
   for (i <- 58 until 64){
     ifft.io.in.bits.iq(i) := modulator.io.out.bits.iq(i-40)
   }
  cyclicPrefix.io.add := true.B
  cyclicPrefix.io.in <> ifft.io.out

  preambleAdder.io.in <> cyclicPrefix.io.out
  fir.io.in <> preambleAdder.io.out
  io.out <> fir.io.out
  io.in.ready := serializer.io.in.ready

}

class RX[T<:Data:Real:BinaryRepresentation, U<:Data:Real:BinaryRepresentation, V<:Data:Real](
  rxParams: RXParams[T, U, V]
) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(IQBundle(rxParams.iqBundleParams)))
    //val out = Decoupled(BitsBundle(bitsBundleParams))
    val out = Decoupled(Vec(5, UInt(1.W)))
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
  phaseRotator.io.in <> io.in
  phaseRotator.io.phiCorrect := ConvertableTo[T].fromDouble(0) //cfoEstimator.phiCorrect

  // Packet Detector Block
  pktDetect.io.in <> phaseRotator.io.out

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
  decode.io.in <> demod.io.out

  io.out <> decode.io.out
  //io.in.ready := true.B

}

trait HasPeripheryModem extends BaseSubsystem {
  // Instantiate rx chain
  val rxChain = LazyModule(new RXThing(FinalRxParams(16, 64, 48)))
  // Instantiate tx chain
  //val txChain = LazyModule(new TXThing(FinalTxParams(16, 64, 48)))
  pbus.toVariableWidthSlave(Some("rxWrite")) { rxChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("rxRead")) { rxChain.readQueue.mem.get }
  //pbus.toVariableWidthSlave(Some("txWrite")) { txChain.writeQueue.mem.get }
  //pbus.toVariableWidthSlave(Some("txRead")) { txChain.readQueue.mem.get }
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
