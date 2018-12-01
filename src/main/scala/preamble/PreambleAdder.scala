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
  val out = Decoupled(IQBundle(params.protoIQ))

  override def cloneType: this.type = CFOIO(params).asInstanceOf[this.type]
}
object PreambleAdderIO i{
  def apply[T <: Data](params: PacketBundleParams[T]): CFOIO[T] =
    new PreambleAdderIO(params)
}

case class FixedPreambleParams(
  width: Int = 1,
  iqWidth: Int,
  stLength: Int = 160,
  ltLength: Int = 160,
) extends CFOParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP)).cloneType
}

class PreambleAdder[T<:Data](val params: PreambleParams[T]) extends Module {
  val io = IO( PreambleAdderIO(params) )

  val inputBuffer = (0 until (params.stLength + params.ltLength)).foldLeft(io.in.bits.iq(0)){(prev, cur) => RegNext(prev)}
  // val stfVec = VecInit(IEEE80211.stf)
  // val ltfVec = VecInit(IEEE80211.ltf)
  val preambleVec = VecInit(IEEE80211.stf :+ IEEE80211.ltf)

  val idle :: preamble :: data :: Nil
  val nxtState = Wire(UInt(2.W))
  val curState = RegNext(nxtState, idle)
  val preambleCounter = Counter(params.stLength + params.ltLength)

  io.out.bits.valid := false.B
  io.out.bits.iq.real := ConvertableTo[T].fromDouble(0.0)
  io.out.bits.iq.imag := ConvertableTo[T].fromDouble(0.0)
  switch(curState){
    is(idle){
      preambleCounter.value := 0.U
      io.out.bits.valid := false.B
      nxtState := Mux(io.in.bits.pktStart, preamble, idle)
    }
    is(preamble){
      io.out.bits.valid := true.B
      io.out.bits.iq := preambleVec(preambleCounter.value)
      when(io.out.bits.ready){
        nxtState := Mux(preambleCounter.inc(), data, preamble)
      }.otherwise{
        nxtState := preamble
      }
    }
    is(data){
      io.out.bits.valid ;= true.B
      io.out.bits.iq := inputBuffer
      nxtState := Mux(io.in.bits.pktEnd, idle, data)
    }
  }
