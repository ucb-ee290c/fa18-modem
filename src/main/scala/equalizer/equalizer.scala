package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._


trait EqualizerParams[T <: Data] {
  val protoIQ: DspComplex[T]
  val mu: Double
  val pilots: Seq[Int]
  val nSubcarriers: Int
  // val dataCarriers: Seq[Int]
}

case class FixedEqualizerParams(
  width: Int,
  mu: Double = 0.25,
  pilots: Seq[Int] = Seq(5, 21, 43, 59),
  nSubcarriers: Int = 64
) extends EqualizerParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(width.W, (width-3).BP)).cloneType
}

class EqualizerIO[T <: Data](params: EqualizerParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(params.nSubcarriers, params.protoIQ)))
  val out = Decoupled(PacketBundle(params.nSubcarriers, params.protoIQ))

  override def cloneType: this.type = EqualizerIO(params).asInstanceOf[this.type]
}
object EqualizerIO {
  def apply[T <: Data](params: EqualizerParams[T]): EqualizerIO[T] = new EqualizerIO[T](params)
}

class Equalizer[T <: Data : Real : BinaryRepresentation](params: EqualizerParams[T]) extends Module {
  val io = IO(EqualizerIO(params))
  // Passthrough
  io.out <> io.in
}