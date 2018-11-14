package modem

import chisel3._
import dsptools.numbers._


trait CordicParams[T<:Data] extends PacketBundleParams[T]{
  val protoXY: T
  val protoZ: T
  val nStages: Int
  val correctGain: Boolean
  val stagesPerCycle: Int
}

