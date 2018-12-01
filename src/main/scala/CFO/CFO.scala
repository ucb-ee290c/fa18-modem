package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint
import dsptools.numbers._

trait CFOParams[T <: Data] extends CordicParams[T] with PacketBundleParams[T] {
  val stLength: Int
  val ltLength: Int
  val giLength: Int
  val preamble: Boolean
}

class CFOIO[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
  val out = Decoupled(PacketBundle(1, params.protoIQ))

  override def cloneType: this.type = CFOIO(params).asInstanceOf[this.type]
}
object CFOIO {
  def apply[T <: Data](params: PacketBundleParams[T]): CFOIO[T] =
    new CFOIO(params)
}

class CFOEIO[T <: Data](params: PacketBundleParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
  val out = Decoupled(PacketBundle(1, params.protoIQ))

  val pErr = Output(params.protoIQ.real.cloneType)
  val cErr = Output(params.protoIQ.real.cloneType)
  val fErr = Output(params.protoIQ.real.cloneType)
  //val cordErr = Output(params.protoIQ.real.cloneType)
  //val stAcc = Output(params.protoIQ.cloneType)
  //val ltAcc = Output(params.protoIQ.cloneType)
  //val delaySTIQ = Output(params.protoIQ.cloneType)
  //val delaySTVal = Output(Bool())
  //val stCounter = Output(UInt(10.W))
  //val giCounter = Output(UInt(10.W))
  //val ltCounter = Output(UInt(10.W))
  val curState = Output(UInt(3.W))

  override def cloneType: this.type = CFOEIO(params).asInstanceOf[this.type]
}
object CFOEIO {
  def apply[T <: Data](params: PacketBundleParams[T]): CFOEIO[T] =
    new CFOEIO(params)
}

case class FixedCFOParams(
  width: Int = 1,
  iqWidth: Int,
  stLength: Int = 160,
  ltLength: Int = 160,
  giLength: Int = 32,
  preamble: Boolean = true,
  stagesPerCycle: Int = 32
) extends CFOParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(iqWidth.W, (iqWidth-3).BP)).cloneType
  val protoXY = FixedPoint(iqWidth.W, (iqWidth-3).BP).cloneType
  val protoZ = FixedPoint(iqWidth.W, (iqWidth-3).BP).cloneType
  val correctGain = true
  val minNumber = math.pow(2.0, -(iqWidth-3))
  // number of cordic stages
  private var n = 0
  while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
    n += 1
  }
  val nStages = n
}

class PhaseRotator[T<:Data:Real:BinaryRepresentation](val params: CFOParams[T]) extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(IQBundle(params)))
    val out = Decoupled(IQBundle(params))
    val phiCorrect = Input(params.protoZ)
  })

  val cordic = Module( new IterativeCordic(params))
  val phiNext = Wire(params.protoZ)
  val phi = RegNext(next = phiNext, init = ConvertableTo[T].fromDouble(0.0))
  val pi = ConvertableTo[T].fromDouble(math.Pi)

  when((phi + io.phiCorrect) > pi){
    phiNext := phi - 2 * pi + io.phiCorrect
  }.elsewhen((phi + io.phiCorrect) < -pi){
    phiNext := phi + 2 * pi + io.phiCorrect
  }.otherwise{
    phiNext := phi + io.phiCorrect
  }

  cordic.io.in.bits.x := io.in.bits.iq.real
  cordic.io.in.bits.y := io.in.bits.iq.imag
  cordic.io.in.bits.z := phi
  cordic.io.in.bits.vectoring := false.B
  cordic.io.in.valid := io.in.valid

  io.in.ready := cordic.io.in.ready
  io.out.bits.iq.real := cordic.io.out.bits.x
  io.out.bits.iq.imag := cordic.io.out.bits.y
  io.out.valid := cordic.io.out.valid
  cordic.io.out.ready := io.out.ready
}

/**
  * Mixin for top-level rocket to add a PWM
  *
  */
// trait HasPeripheryCFO extends BaseSubsystem {
//   // instantiate cordic chain
//   val cordicChain = LazyModule(new CordicThing(FixedCFOParams(8, 10)))
//   // connect memory interfaces to pbus
//   pbus.toVariableWidthSlave(Some("cordicWrite")) { cordicChain.writeQueue.mem.get }
//   pbus.toVariableWidthSlave(Some("cordicRead")) { cordicChain.readQueue.mem.get }
// }

class OneCyclePulseGen[T<:Data] extends Module {
  val io = IO( new Bundle{
    val in = Input(Bool())
    val out = Output(Bool())
  })

  val idle::trig::stop::Nil = Enum(3)
  val delayedIn = RegNext(next = io.in, init = true.B)
  val nxtState = WireInit(idle)
  val curState = RegNext(next=nxtState, init=idle)

  io.out := false.B

  switch(curState){
    is(idle){
      io.out := false.B
      when(io.in && !delayedIn){
        nxtState := trig
      }.otherwise{
        nxtState := idle
      }
    }
    is(trig){
      io.out := true.B
      nxtState := stop
    }
    is(stop){
      io.out := false.B
      nxtState := idle
    }
  }
}

class STFDropper[T<:Data:ConvertableTo:Ring](val params: CFOParams[T]) extends Module {
  val io = IO(new Bundle{
    val in = Input(IQBundle(params))
    val keep = Input(Bool())
    val out = Output(IQBundle(params))
  })

  //val zero = DspComplex(ConvertableTo[T].fromDouble(0), ConvertableTo[T].fromDouble(0))
  //val zeroWire = WireInit(zero)

  when(io.keep){
    io.out.iq := io.in.iq
  }.otherwise{
    io.out.iq.real := ConvertableTo[T].fromDouble(0)
    io.out.iq.imag := ConvertableTo[T].fromDouble(0)
  }
}

class CoarseOffsetEstimator[T<:Data:ConvertableTo:BinaryRepresentation:Ring](val params: CFOParams[T], val stDelay: Int) extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(params.protoIQ))
    //val const = Outout(params.protoIQ.real)
    // val counter = Input(Bool())
    //val stMul = Output(params.protoIQ)
    //val stAcc = Output(params.protoIQ)
    //val delayIQ = Output(params.protoIQ)
    //val delayValid = Output(Bool())
    val cordicIn = Decoupled(CordicBundle(params))
    val cordicOut = Flipped(Decoupled(CordicBundle(params)))
    val out = Decoupled(params.protoZ)
  })

  //val pulseGen = Module( new OneCyclePulseGen )

  val stMul = Wire(params.protoIQ)
  val stAcc = Reg(params.protoIQ)
  //val div = ConvertableTo[T].fromDouble(1.0/stDelay)
  //val stAcc = RegInit(params.protoIQ, DspComplex[T](ConvertableTo[T].fromDouble(0), ConvertableTo[T].fromDouble(0)))
  // val stCounter = Counter(params.stLength)
  val delayIQByST = (0 until stDelay).foldLeft(io.in.bits){(prev, curr) => RegNext(prev)}
  val delayValidByST = (0 until stDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev, init = false.B)}

  io.cordicIn.bits.x := stAcc.real
  io.cordicIn.bits.y := stAcc.imag
  io.cordicIn.bits.z := ConvertableTo[T].fromDouble(0)
  io.cordicIn.bits.vectoring := true.B
  io.cordicOut.ready := io.out.ready
  stMul := (delayIQByST * io.in.bits.conj())
  //io.stAcc := stAcc
  //io.stMul := stMul
  //io.delayIQ := delayIQByST
  //io.delayValid := delayValidByST

  when(delayValidByST && io.in.valid){
    stAcc := stAcc + stMul
  }.otherwise{
    //stAcc := stAcc
    stAcc.real := ConvertableTo[T].fromDouble(0)
    stAcc.imag := ConvertableTo[T].fromDouble(0)
  }

  //pulseGen.io.in := !io.in.valid
  io.cordicIn.valid := !io.in.valid && RegNext(io.in.valid)

  io.out.bits := io.cordicOut.bits.z * ConvertableTo[T].fromDouble(1.0/stDelay)
  io.out.valid := io.cordicOut.valid
  io.in.ready := io.out.ready
}

class FineOffsetEstimator[T<:Data:ConvertableTo:BinaryRepresentation:Ring](val params: CFOParams[T], val ltDelay: Int) extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(params.protoIQ))
    // val ltCounter = Input(Bool())
    // val giCounter = Input(Bool())
    //val ltAcc = Output(params.protoIQ)
    //val ltMul = Output(params.protoIQ)
    //val delayIQ = Output(params.protoIQ)
    //val delayValid = Output(Bool())
    val cordicIn = Decoupled(CordicBundle(params))
    val cordicOut = Flipped(Decoupled(CordicBundle(params)))
    val out = Decoupled(params.protoZ)
  })
  val ltMul = Wire(params.protoIQ)
  val ltAcc = Reg(params.protoIQ)
  //val ltAcc = RegInit(params.protoIQ, DspComplex[T](ConvertableTo[T].fromDouble(0), ConvertableTo[T].fromDouble(0)))
  // val stCounter = Counter(params.stLength)
  val delayIQByLT = (0 until ltDelay).foldLeft(io.in.bits){(prev, curr) => RegNext(prev)}
  val delayValidByLT = (0 until ltDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev)}

  io.cordicIn.bits.x := ltAcc.real
  io.cordicIn.bits.y := ltAcc.imag
  io.cordicIn.bits.z := ConvertableTo[T].fromDouble(0)
  io.cordicIn.bits.vectoring := true.B
  io.cordicOut.ready := io.out.ready
  ltMul := (delayIQByLT * io.in.bits.conj())
  //io.ltAcc := ltAcc
  //io.ltMul := ltMul
  //io.delayIQ := delayIQByLT
  //io.delayValid := delayValidByLT

  when(delayValidByLT && io.in.valid){
    ltAcc := ltAcc + ltMul
  }.otherwise{
    //ltAcc := ltAcc
    ltAcc.real := ConvertableTo[T].fromDouble(0)
    ltAcc.imag := ConvertableTo[T].fromDouble(0)
  }

  io.cordicIn.valid := !io.in.valid && RegNext(io.in.valid) 

  io.out.bits := io.cordicOut.bits.z * ConvertableTo[T].fromDouble(1.0/ltDelay)
  io.out.valid := io.cordicOut.valid
  io.in.ready := io.out.ready
}

class CFOEstimation[T<:Data:Real:BinaryRepresentation:ConvertableTo](val params: CFOParams[T]) extends Module {
  // requireIsChiselType(params.protoIn)
  val io = IO(CFOEIO(params))

  val cordic = Module ( new IterativeCordic(params) )
  val pulseGen = Module ( new OneCyclePulseGen )
  val stfDropper = Module ( new STFDropper(params) )

  if(params.preamble == true){
    // val sm = Module( new PreambleStateMachine(params.stLength, params.ltLength) )
    val stDelay = 16
    val ltDelay = 64

    val idle::st::gi::lt::data::Nil = Enum(5)

    val coe = Module ( new CoarseOffsetEstimator(params, stDelay) )
    val foe = Module ( new FineOffsetEstimator(params, ltDelay) )

    val curState = RegInit(UInt(3.W), idle)
    val nxtState = Wire(UInt(3.W))
    val coarseOffset = RegEnable(next=coe.io.out.bits, init=ConvertableTo[T].fromDouble(0.0), enable=coe.io.out.valid)
    val fineOffset = RegEnable(next=foe.io.out.bits, init=ConvertableTo[T].fromDouble(0.0), enable=foe.io.out.valid)

    //val stMul = Wire(params.protoIQ)
    //val stAcc = Reg(params.protoIQ)
    //val ltMul = Wire(params.protoIQ)
    //val ltAcc = Reg(params.protoIQ)
    val stCounter = Counter(params.stLength)
    val ltCounter = Counter(params.ltLength - params.giLength)
    val giCounter = Counter(params.giLength)

    val estimatorReady = Wire(Bool())
    val stdbyInReg = Reg(CordicBundle(params))
    val stdbyOutReg = Reg(CordicBundle(params))
    val stdbyInVReg = Reg(Bool())
    val stdbyInRReg = Reg(Bool())
    val stdbyOutVReg = Reg(Bool())
    val stdbyOutRReg = Reg(Bool())

    //val delayIQByST = (0 until stDelay).foldLeft(io.in.bits.iq(0)){(prev, curr) => RegNext(prev)}
    //val delayIQByLT = (0 until ltDelay).foldLeft(io.in.bits.iq(0)){(prev, curr) => RegNext(prev)}
    //val delayValidByST = (0 until stDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev)}
    //val delayValidByLT = (0 until ltDelay).foldLeft(io.in.valid){(prev, curr) => RegNext(prev)}


    // def ConnectToCordic[T<:Data:BinaryRepresentation:Real:ConvertableTo](c, d): Boolean = {
    //   cordic.io.in.bits := c.io.cordicIn.bits
    //   c.io.cordicOut.bits := cordic.io.out.bits
    //   cordic.io.in.valid := c.io.cordicIn.valid
    //   c.io.cordicIn.ready := cordic.io.in.ready
    //   cordic.io.out.ready := c.io.cordicOut.ready
    //   c.io.cordicOut.valid := cordic.io.out.valid
    //   // Connect unused block to some placeholder registers so FIRRTL is happy
    //   stdbyInReg := d.io.cordicIn.bits
    //   d.io.cordicOut.bits := stdbyOutReg
    //   stdbyInVReg := d.io.cordicIn.valid
    //   d.io.cordicIn.ready := stdbyInRReg
    //   stdbyOutRReg := d.io.cordicOut.ready
    //   d.io.cordicOut.valid := stdbyOutVReg
    //
    //   true
    // }

    //fineOffset := ConvertableTo[T].fromDouble(0.0)
    io.in.ready := estimatorReady
    io.out.valid := cordic.io.out.valid
    io.out.bits.pktStart := nxtState === lt && RegNext(!(nxtState === lt))

    io.out.bits.pktEnd := io.in.bits.pktEnd
    //io.out.bits.iq(0) := Mux(curState === lt || curState === data, io.in.bits.iq(0), Wire(DspComplex(ConvertableTo[T].fromDouble(0),ConvertableTo[T].fromDouble(0))))
    stfDropper.io.in.iq := io.in.bits.iq(0)
    stfDropper.io.keep := (curState === lt || curState === data) // Put this in FSM
    pulseGen.io.in := (curState === lt || curState === data)
    io.out.bits.iq(0) := stfDropper.io.out.iq
    //io.pErr := (coe.io.out.bits & coe.io.out.valid) + (foe.io.out.bits & foe.io.out.valid)
    //io.pErr := Mux(coe.io.out.valid, coe.io.out.bits, ConvertableTo[T].fromDouble(0)) + (foe.io.out.valid, foe.io.out.bits, ConvertableTo[T].fromDouble(0))
    io.pErr := coarseOffset + fineOffset 
    //io.cErr := coe.io.out.bits
    io.cErr := coarseOffset 
    //io.fErr := foe.io.out.bits
    io.fErr := fineOffset
    //io.cordErr := cordic.io.out.bits.z
    //io.stAcc := coe.io.stAcc
    //io.ltAcc := foe.io.ltAcc
    //io.delaySTIQ := coe.io.delayIQ
    //io.delaySTVal := coe.io.delayValid
    //io.stCounter := stCounter.value
    //io.giCounter := giCounter.value
    //io.ltCounter := ltCounter.value

    coe.io.in.bits := io.in.bits.iq(0)
    foe.io.in.bits := io.in.bits.iq(0)

    estimatorReady := io.out.ready

     cordic.io.in.bits.x := ConvertableTo[T].fromDouble(0)
     cordic.io.in.bits.y := ConvertableTo[T].fromDouble(0)
     cordic.io.in.bits.z := ConvertableTo[T].fromDouble(0)
     cordic.io.in.bits.vectoring := false.B
     cordic.io.in.valid := false.B
     cordic.io.out.ready := false.B

     cordic.io.in.bits := coe.io.cordicIn.bits
     coe.io.cordicOut.bits := cordic.io.out.bits
     cordic.io.in.valid := coe.io.cordicIn.valid
     coe.io.out.ready := true.B
     coe.io.in.valid := false.B
     coe.io.cordicIn.ready := cordic.io.in.ready
     cordic.io.out.ready := coe.io.cordicOut.ready
     coe.io.cordicOut.valid := cordic.io.out.valid
     // Connect unused block to some placeholder registers so FIRRTL is happy
     stdbyInReg := foe.io.cordicIn.bits
     foe.io.cordicOut.bits := stdbyOutReg
     stdbyInVReg := foe.io.cordicIn.valid
     foe.io.cordicIn.ready := stdbyInRReg
     stdbyOutRReg := foe.io.cordicOut.ready
     foe.io.cordicOut.valid := stdbyOutVReg
     foe.io.out.ready := true.B
     foe.io.in.valid := false.B

     nxtState := idle
     io.curState := curState

    //stMul := (delayIQByST.conj() * io.in.bits.iq(0))
    //ltMul := (delayIQByLT.conj() * io.in.bits.iq(0))
    switch(curState){
      is(idle){
        giCounter.value := 0.U
        stCounter.value := 0.U
        ltCounter.value := 0.U
        // estimatorReady := io.out.ready
        coe.io.in.valid := false.B
        foe.io.in.valid := false.B
        cordic.io.in.valid := false.B
        cordic.io.out.ready := true.B

        cordic.io.in.bits := coe.io.cordicIn.bits
        coe.io.cordicOut.bits := cordic.io.out.bits
        cordic.io.in.valid := coe.io.cordicIn.valid
        coe.io.cordicIn.ready := cordic.io.in.ready
        cordic.io.out.ready := coe.io.cordicOut.ready
        coe.io.cordicOut.valid := cordic.io.out.valid
        // Connect unused block to some placeholder registers so FIRRTL is happy
        stdbyInReg := foe.io.cordicIn.bits
        foe.io.cordicOut.bits := stdbyOutReg
        stdbyInVReg := foe.io.cordicIn.valid
        foe.io.cordicIn.ready := stdbyInRReg
        stdbyOutRReg := foe.io.cordicOut.ready
        foe.io.cordicOut.valid := stdbyOutVReg


        nxtState := Mux(io.in.bits.pktStart, st, idle)
      }
      is(st){
        cordic.io.in.bits := coe.io.cordicIn.bits
        coe.io.cordicOut.bits := cordic.io.out.bits
        cordic.io.in.valid := coe.io.cordicIn.valid
        coe.io.cordicIn.ready := cordic.io.in.ready
        cordic.io.out.ready := coe.io.cordicOut.ready
        coe.io.cordicOut.valid := cordic.io.out.valid
        // Connect unused block to some placeholder registers so FIRRTL is happy
        stdbyInReg := foe.io.cordicIn.bits
        foe.io.cordicOut.bits := stdbyOutReg
        stdbyInVReg := foe.io.cordicIn.valid
        foe.io.cordicIn.ready := stdbyInRReg
        stdbyOutRReg := foe.io.cordicOut.ready
        foe.io.cordicOut.valid := stdbyOutVReg
        foe.io.in.valid := false.B
        when(io.in.fire()){
          coe.io.in.valid := true.B
          nxtState := Mux(stCounter.inc(), gi, st)
        }.otherwise{
          coe.io.in.valid := true.B
          nxtState := st
        }
      }
      is(gi){
        cordic.io.in.bits := coe.io.cordicIn.bits
        coe.io.cordicOut.bits := cordic.io.out.bits
        cordic.io.in.valid := coe.io.cordicIn.valid
        coe.io.cordicIn.ready := cordic.io.in.ready
        cordic.io.out.ready := coe.io.cordicOut.ready
        coe.io.cordicOut.valid := cordic.io.out.valid
        // Connect unused block to some placeholder registers so FIRRTL is happy
        stdbyInReg := foe.io.cordicIn.bits
        foe.io.cordicOut.bits := stdbyOutReg
        stdbyInVReg := foe.io.cordicIn.valid
        foe.io.cordicIn.ready := stdbyInRReg
        stdbyOutRReg := foe.io.cordicOut.ready
        foe.io.cordicOut.valid := stdbyOutVReg
        coe.io.in.valid := false.B
        when(io.in.fire()){
          foe.io.in.valid := false.B
          nxtState := Mux(giCounter.inc(), lt, gi)
        }.otherwise{
          foe.io.in.valid := false.B
          nxtState := gi
        }
      }
      is(lt){
        cordic.io.in.bits := foe.io.cordicIn.bits
        foe.io.cordicOut.bits := cordic.io.out.bits
        cordic.io.in.valid := foe.io.cordicIn.valid
        foe.io.cordicIn.ready := cordic.io.in.ready
        cordic.io.out.ready := foe.io.cordicOut.ready
        foe.io.cordicOut.valid := cordic.io.out.valid
        // Connect unused block to some placeholder registers so FIRRTL is happy
        stdbyInReg := coe.io.cordicIn.bits
        coe.io.cordicOut.bits := stdbyOutReg
        stdbyInVReg := coe.io.cordicIn.valid
        coe.io.cordicIn.ready := stdbyInRReg
        stdbyOutRReg := coe.io.cordicOut.ready
        coe.io.cordicOut.valid := stdbyOutVReg
        coe.io.in.valid := false.B
        when(io.in.fire()){
          foe.io.in.valid := true.B
          nxtState := Mux(ltCounter.inc(), data, lt)
        }.otherwise{
          foe.io.in.valid := true.B
          nxtState := lt
        }
      }
      is(data){
        cordic.io.in.bits := foe.io.cordicIn.bits
        foe.io.cordicOut.bits := cordic.io.out.bits
        cordic.io.in.valid := foe.io.cordicIn.valid
        foe.io.cordicIn.ready := cordic.io.in.ready
        cordic.io.out.ready := foe.io.cordicOut.ready
        foe.io.cordicOut.valid := cordic.io.out.valid
        // Connect unused block to some placeholder registers so FIRRTL is happy
        stdbyInReg := coe.io.cordicIn.bits
        coe.io.cordicOut.bits := stdbyOutReg
        stdbyInVReg := coe.io.cordicIn.valid
        coe.io.cordicIn.ready := stdbyInRReg
        stdbyOutRReg := coe.io.cordicOut.ready
        coe.io.cordicOut.valid := stdbyOutVReg
        coe.io.in.valid := false.B
        foe.io.in.valid := false.B
        nxtState := Mux(io.in.bits.pktEnd, idle, data)
      }
    }
    curState := nxtState
  }
}


class CFOCorrection[T<:Data:Real:BinaryRepresentation:ConvertableTo](val params: CFOParams[T]) extends Module {
  val io = IO( new Bundle{
    val in = Flipped(Decoupled(PacketBundle(1, params.protoIQ)))
    val prOut = Output(params.protoIQ)
    val out = Decoupled(PacketBundle(1, params.protoIQ))
    val pErr = Output(params.protoZ)
    val cErr = Output(params.protoZ)
    val fErr = Output(params.protoZ)
    val curState = Output(UInt(3.W))
  })

  val phaseRotator = Module( new PhaseRotator(params) )
  val cfoEstimator = Module( new CFOEstimation(params) )

  io.in.ready := phaseRotator.io.in.ready
  phaseRotator.io.in.valid := io.in.valid
  phaseRotator.io.in.bits.iq := io.in.bits.iq(0)
  phaseRotator.io.phiCorrect := cfoEstimator.io.pErr
  cfoEstimator.io.in.valid := phaseRotator.io.out.valid
  phaseRotator.io.out.ready := cfoEstimator.io.in.ready
  cfoEstimator.io.in.bits.pktStart := ShiftRegister(io.in.bits.pktStart, params.nStages)
  cfoEstimator.io.in.bits.pktEnd := ShiftRegister(io.in.bits.pktEnd, params.nStages)
  cfoEstimator.io.in.bits.iq(0) := phaseRotator.io.out.bits.iq
  io.out <> cfoEstimator.io.out
  io.pErr := cfoEstimator.io.pErr
  io.cErr := cfoEstimator.io.cErr
  io.fErr := cfoEstimator.io.fErr
  io.prOut := phaseRotator.io.out.bits.iq
  io.curState := cfoEstimator.io.curState

 }
class COEWrapper[T<:chisel3.Data:Real:ConvertableTo:BinaryRepresentation](val params: CFOParams[T], stLength: Int) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(params.protoIQ))
    //val stMul = Output(params.protoIQ)
    //val stAcc = Output(params.protoIQ)
    //val delayIQ = Output(params.protoIQ)
    //val delayValid = Output(Bool())
    val cordicIn = Output(CordicBundle(params))
    val cordicInVal = Output(Bool())
    val cordicOut = Output(CordicBundle(params))
    val cordicOutVal = Output(Bool())
    val out = Decoupled(params.protoZ)
    })

  val cordic = Module (new IterativeCordic[T](params))
  val coe = Module (new CoarseOffsetEstimator[T](params, stLength))

  
  cordic.io.in.bits := coe.io.cordicIn.bits
  cordic.io.out.ready := coe.io.cordicOut.ready
  cordic.io.in.valid := coe.io.cordicIn.valid
  coe.io.cordicOut.bits := cordic.io.out.bits
  coe.io.out.ready := io.out.ready
  coe.io.cordicIn.ready := cordic.io.in.ready
  coe.io.cordicOut.valid := cordic.io.out.valid

  coe.io.in <> io.in
  //io.stMul := coe.io.stMul
  //io.stAcc := coe.io.stAcc
  //io.delayIQ := coe.io.delayIQ
  //io.delayValid := coe.io.delayValid
  io.cordicIn := coe.io.cordicIn.bits
  io.cordicInVal := coe.io.cordicIn.valid
  io.cordicOut := cordic.io.out.bits
  io.cordicOutVal := cordic.io.out.valid
  io.out <> coe.io.out
}

//class FOEWrapper[T<:chisel3.Data:Real:ConvertableTo:BinaryRepresentation](val params: CFOParams[T], ltLength: Int) extends Module {
  //val io = IO(new Bundle{
    //val in = Flipped(Decoupled(params.protoIQ))
    //val ltMul = Output(params.protoIQ)
    //val ltAcc = Output(params.protoIQ)
    //val delayIQ = Output(params.protoIQ)
    //val delayValid = Output(Bool())
    //val cordicIn = Output(CordicBundle(params))
    //val cordicInVal = Output(Bool())
    //val cordicOut = Output(CordicBundle(params))
    //val cordicOutVal = Output(Bool())
    //val out = Decoupled(params.protoZ)
    //})

  //val cordic = Module (new IterativeCordic[T](params))
  //val coe = Module (new FineOffsetEstimator[T](params, ltLength))

  
  //cordic.io.in.bits := coe.io.cordicIn.bits
  //cordic.io.out.ready := coe.io.cordicOut.ready
  //cordic.io.in.valid := coe.io.cordicIn.valid
  //coe.io.cordicOut.bits := cordic.io.out.bits
  //coe.io.out.ready := io.out.ready
  //coe.io.cordicIn.ready := cordic.io.in.ready
  //coe.io.cordicOut.valid := cordic.io.out.valid

  //coe.io.in <> io.in
  //io.ltMul := coe.io.ltMul
  //io.ltAcc := coe.io.ltAcc
  //io.delayIQ := coe.io.delayIQ
  //io.delayValid := coe.io.delayValid
  //io.cordicIn := coe.io.cordicIn.bits
  //io.cordicInVal := coe.io.cordicIn.valid
  //io.cordicOut := cordic.io.out.bits
  //io.cordicOutVal := cordic.io.out.valid
  //io.out <> coe.io.out
//}
