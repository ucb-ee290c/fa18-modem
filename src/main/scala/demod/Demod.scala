package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint
import dsptools.numbers._

trait DemodulationParams[T<: Data, U<: Data] extends PacketBundleParams[T] with BitsBundleParams[U] {
  val tdummy: DspComplex[T]
  val udummy: U
}

case class HardDemodParams(
  val iqWidth: Int,
  val width: Int,
  val bitsWidth: Int
) extends DemodulationParams[FixedPoint, SInt]{
  val protoIQ = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP)).cloneType
  val protoBits = SInt(2.W)
  val tdummy = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP)).cloneType
  val udummy = SInt(2.W)
}



class Demodulator[T <: Data:Real:BinaryRepresentation, U <: Data](params: DemodulationParams[T, U]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(DeserialPacketBundle(params)))
    val out = Decoupled(BitsBundle(params))
  })
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid

  val dummyReg = RegNext(io.in.bits)
  val dummyBits = BitsBundle(params)

  dummyBits.bits := Vec.fill(params.bitsWidth){WireInit(1.S)}
  dummyBits.pktStart := io.in.bits.pktStart
  dummyBits.pktEnd := io.in.bits.pktEnd

  io.out.bits := dummyBits
}
