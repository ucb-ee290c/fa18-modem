package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint
import dsptools.numbers._

trait PreambleParams[T <: Data] extends PacketBundleParams[T] {
  val stLength: Int
  val ltLength: Int
}

class PreambleAdderIO[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
  val out = Decoupled(PacketBundle(1, params.protoIQ))

  override def cloneType: this.type = PreambleAdderIO(params).asInstanceOf[this.type]
}
object PreambleAdderIO {
  def apply[T <: Data](params: PacketBundleParams[T]): PreambleAdderIO[T] =
    new PreambleAdderIO(params)
}

case class FixedPreambleParams(
  width: Int = 1,
  iqWidth: Int,
  stLength: Int = 160,
  ltLength: Int = 160,
) extends PreambleParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP)).cloneType
}

class PreambleAdder[T<:Data:Ring:ConvertableTo](val params: PreambleParams[T]) extends Module {
  val io = IO( PreambleAdderIO(params) )

  val inputBuffer = (0 to (params.stLength + params.ltLength)).foldLeft(io.in.bits.iq(0)){(prev, cur) => RegNext(prev)}
  // val stfVec = VecInit(IEEE80211.stf)
  // val ltfVec = VecInit(IEEE80211.ltf)
  val preambleVecReal = VecInit((IEEE80211.stf ++ IEEE80211.ltf).map{x => ConvertableTo[T].fromDouble(x.real)})
  val preambleVecImag = VecInit((IEEE80211.stf ++ IEEE80211.ltf).map{x => ConvertableTo[T].fromDouble(x.imag)})

  val idle :: preamble :: data :: Nil = Enum(3)
  val nxtState = Wire(UInt(2.W))
  val curState = RegNext(nxtState, idle)
  val preambleCounter = Counter(params.stLength + params.ltLength)

  io.out.valid := false.B
  io.out.bits.iq(0).real := ConvertableTo[T].fromDouble(0.0)
  io.out.bits.iq(0).imag := ConvertableTo[T].fromDouble(0.0)
  
  nxtState := idle
  io.in.ready := true.B
  io.out.bits.pktStart := io.in.bits.pktStart
  io.out.bits.pktEnd := io.in.bits.pktEnd
  switch(curState){
    is(idle){
      preambleCounter.value := 0.U
      io.out.valid := false.B
      io.out.bits.iq(0).real := ConvertableTo[T].fromDouble(0.0)
      io.out.bits.iq(0).imag := ConvertableTo[T].fromDouble(0.0)
      nxtState := Mux(io.in.bits.pktStart, preamble, idle)
    }
    is(preamble){
      io.out.valid := true.B
      io.out.bits.iq(0).real := preambleVecReal(preambleCounter.value)
      io.out.bits.iq(0).imag := preambleVecImag(preambleCounter.value)
      when(io.out.ready){
        nxtState := Mux(preambleCounter.inc(), data, preamble)
      }.otherwise{
        nxtState := preamble
      }
    }
    is(data){
      io.out.valid := true.B
      io.out.bits.iq(0) := inputBuffer
      nxtState := Mux(io.in.bits.pktEnd, idle, data)
    }
  }
}
