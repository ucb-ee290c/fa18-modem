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

class PreambleAdder[T<:Data] extends Module {
  val io = IO(
