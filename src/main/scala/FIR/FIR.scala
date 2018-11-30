package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.experimental.withClock
import chisel3.util.Decoupled
import chisel3.util._
import breeze.linalg.{DenseVector, randomDouble}
//import chisel3.core.data
import dsptools.numbers._
import breeze.numerics.{atan, pow, sqrt, abs,floor}
import breeze.numerics.constants.{Pi}
import scala.math._
import breeze.math.Complex

import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

class ScalaFirFilter(taps: Seq[Complex]) {
  var pseudoRegisters = List.fill(taps.length)(Complex(0,0))

  def poke(value: Complex): Complex = {
    pseudoRegisters = value :: pseudoRegisters.take(taps.length - 1)
    var accumulator = Complex(0,0)
    for(i <- taps.indices) {
      accumulator += taps(i) * pseudoRegisters(i)
    }
    accumulator
  }
}

// Fir

class FIRIO[T <: Data, U <: Data](params: ModFFTParams[T,U]) extends Bundle {
  val in = Flipped(Decoupled(SerialPacketBundle(params)))
  val out = Decoupled(SerialPacketBundle(params))

  override def cloneType: this.type = FIRIO(params).asInstanceOf[this.type]
}
object FIRIO {
  def apply[T <: Data, U <: Data](params: ModFFTParams[T,U]): FIRIO[T,U] =
    new FIRIO(params)
}




class MFir[T <: Data :Real:BinaryRepresentation,U <:Data](val params: ModFFTParams[T,U]) extends Module {
  val io = IO(FIRIO(params))
  val taps = Seq(Complex(1,0),Complex(1,0),Complex(1,0),Complex(1,0))
  val taps1 = taps.map(x=> DspComplex(Real[T].fromDouble(x.real),Real[T].fromDouble(x.imag)))
  val prods = taps1.map { t => t * io.in.bits.iq }

  val regs = Seq.fill(taps1.length - 1) { Reg(params.protoIQ.cloneType) }

  when (io.in.valid) {
    for (i <- 0 until regs.length - 1) {
      regs(i).real := prods(i + 1).real + regs(i + 1).real
      regs(i).imag := prods(i + 1).imag + regs(i + 1).imag

    }
    regs.last := prods.last
  }

  io.out.bits.iq.real := prods.head.real + regs.head.real
  io.out.bits.iq.imag := prods.head.imag + regs.head.imag

  io.out.valid := io.in.valid
 
  //val taps = Seq(io.in.bits.iq) ++ Seq.fill(consts.length - 1)(RegInit(Ring[T].zero))
  //taps.zip(taps.tail).foreach { case (a, b) => when (io.in.valid()) { b := a } }

  //io.out.bits.iq := taps.zip(consts).map { case (a, b) => a * b }.reduce(_ + _)
  io.out.valid := io.in.valid
  io.in.ready := true.B
  io.out.bits.pktStart := io.in.bits.pktStart
  io.out.bits.pktEnd := io.in.bits.pktEnd


}


