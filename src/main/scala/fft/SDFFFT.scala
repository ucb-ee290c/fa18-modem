package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
import breeze.numerics.{cos, sin}
import scala.math.{Pi, pow}

/**************************************************
 * Single-Path Delay Feedback FFT (top level and sub-blocks)
 **************************************************/

/**
 * Top level SDF FFT block
 *
 * For serial-input serial-output, arbitrarily choose DIT for "optimal" decimation type setting
 */
class SDFFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  params.checkSDFRadix()
  params.checkNumPointsPowOfSDFRadix()
  val io = IO(SISOIO(params))
  val serdes_params = PacketSerDesParams(params.protoIQ.cloneType, params.numPoints)

  if (params.decimType == "dif") {
    val ser               = Module(new PacketSerializer(serdes_params))
    val sdf_fft_deser_out = Module(new SDFFFTDeserOut(params))
    io.in     <> sdf_fft_deser_out.io.in
    ser.io.in <> sdf_fft_deser_out.io.out
    io.out    <> ser.io.out
  } else {
    val des              = Module(new PacketDeserializer(serdes_params))
    val sdf_fft_deser_in = Module(new SDFFFTDeserIn(params))
    io.in      <> des.io.in
    des.io.out <> sdf_fft_deser_in.io.in
    io.out     <> sdf_fft_deser_in.io.out
  }
}

/**
 * Top level SDF FFT block for parallel/deserialized input
 *
 * For parallel-input serial-output, "optimal" setting is DIT (unscrambler is at the input)
 */
class SDFFFTDeserIn[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val dit: Boolean = true) extends Module {
  params.checkNumPointsPow2()
  val io = IO(DISOIO(params))

  val updated_params = if (params.decimType == "opt") FFTParams(params, "dit") else params
  val serdes_params  = PacketSerDesParams(params.protoIQ.cloneType, params.numPoints)
  val inp_ser     = Module(new PacketSerializer(serdes_params))
  val unscrambler = Module(new FFTUnscrambler(updated_params))
  val sdf_chain   = if (params.sdfRadix == 4) {
    Module(new SDFChainRadix4(updated_params))
  } else {
    Module(new SDFChainRadix2(updated_params))
  }

  val out_if = if (updated_params.decimType == "dit") {
    // Data flow: Unscrambler -> PacketSerializer -> SDF Chain
    unscrambler.io.in <> io.in
    inp_ser.io.in     <> unscrambler.io.out
    sdf_chain.io.out
  } else {
    // Data flow: PacketSerializer -> SDF Chain -> PacketDeserializer -> Unscrambler -> PacketSerializer
    val ser = Module(new PacketSerializer(serdes_params))
    val des = Module(new PacketDeserializer(serdes_params))
    inp_ser.io.in <> io.in
    des.io.in     <> sdf_chain.io.out
    des.io.out    <> unscrambler.io.in
    ser.io.in     <> unscrambler.io.out
    ser.io.out
  }
  inp_ser.io.out <> sdf_chain.io.in
  io.out         <> out_if
}

/**
 * Top level SDF FFT block for parallel/deserialized output
 *
 * For serial-input parallel-output, "optimal" setting is DIF (unscrambler is at the output)
 */
class SDFFFTDeserOut[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  val io = IO(SIDOIO(params))

  val updated_params = if (params.decimType == "opt") FFTParams(params, "dif") else params
  val serdes_params  = PacketSerDesParams(params.protoIQ.cloneType, params.numPoints)
  val out_des     = Module(new PacketDeserializer(serdes_params))
  val unscrambler = Module(new FFTUnscrambler(updated_params))
  val sdf_chain   = if (params.sdfRadix == 4) {
    Module(new SDFChainRadix4(updated_params))
  } else {
    Module(new SDFChainRadix2(updated_params))
  }

  val inp_if = if (updated_params.decimType == "dif") {
    // Data flow: SDF Chain -> PacketDeserializer -> Unscrambler
    unscrambler.io.in  <> out_des.io.out
    unscrambler.io.out <> io.out
    sdf_chain.io.in
  } else {
    // Data flow: PacketDeserializer -> Unscrambler -> PacketSerializer -> SDF Chain -> PacketDeserializer
    val ser = Module(new PacketSerializer(serdes_params))
    val des = Module(new PacketDeserializer(serdes_params))
    out_des.io.out <> io.out
    ser.io.out     <> sdf_chain.io.in
    ser.io.in      <> unscrambler.io.out
    des.io.out     <> unscrambler.io.in
    des.io.in
  }
  out_des.io.in <> sdf_chain.io.out
  inp_if        <> io.in
}

/**
 * SDF FFT Unscrambler
 *
 * Reorders parallel data by bit reversion
 */
class FFTUnscrambler[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(DIDOIO(params))

  // Bulk connect, but iq field will be re-connected in the following block of code
  io.out <> io.in

  val log2Radix = log2Ceil(params.sdfRadix)
  (0 until params.numPoints).foreach(i => {
    val indexWidth = log2Up(params.numPoints)
    val index = i.U(indexWidth.W)
    val index_vectorized = (0 until indexWidth by log2Radix).map(j => index(j + log2Radix - 1, j))
    val reversed_index = Cat(index_vectorized)
    io.out.bits.iq(reversed_index) := io.in.bits.iq(index)
  })
}

/**
 * Radix-2 SDF FFT Chain
 *
 * Instantiates and connects SDF FFT stages in series and provides necessary control signals for each stage
 */
class SDFChainRadix2[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  // At this point, "opt" decimType should already have been resolved to "dit" or "dif"
  require(Seq("dit", "dif").contains(params.decimType), s"""Decimation type must either be dit or dif""")
  val io = IO(SISOIO(params))

  // Calculation of constants
  val numPointsDiv2     = params.numPoints / 2                                                                // FFT size / 2
  val numStages         = log2Up(params.numPoints)                                                            // required number of SDF stages for given FFT size
  val delayLog2s        = if (params.decimType == "dit") (0 until numStages) else (0 until numStages).reverse // log2(delay) of each nth stage
  val delays            = delayLog2s.map(d => pow(2, d).toInt)                                                // actual delay of each nth stage
  val cumulative_delays = delays.scanLeft(0)(_ + _)  

  // FSM states for control logic
  val sIdle :: sComp :: sDone :: Nil = Enum(3)
  val state      = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  // Counter for control logic
  val cntr      = RegInit(0.U(log2Up(params.numPoints).W))
  val cntr_next = Wire(cntr.cloneType)

  // Instantiate and connect control signals of stages
  val sdf_stages = delayLog2s.zip(delays).zip(cumulative_delays).map {
    case ((delayLog2, delay), cumulative_delay) => {
      val stage = Module(new SDFStageRadix2(params, delay=delay))
      stage.io.cntr         := (cntr - cumulative_delay.U)(delayLog2, 0)
      stage.io.en           := io.in.fire()
      stage
    }
  }

  // Connect datapath of stages in series
  sdf_stages.map(_.io).foldLeft(RegNext(io.in.bits))((stg_in, stg_io) => {
    stg_io.in := stg_in
    stg_io.out
  })

  // Output interface connections
  // TODO: Do we need a Queue?
  io.out.bits  := sdf_stages.last.io.out
  io.out.valid := ShiftRegister(io.in.fire(), cumulative_delays.last + 1, resetData=false.B, en=true.B)
  io.in.ready  := io.out.ready

  // Controller FSM
  cntr_next  := cntr
  state_next := state

  switch (state) {
    is (sIdle) {
      when (io.in.fire()) { state_next := sComp }
    }
    is (sComp) {
      when (io.in.fire()) {
        cntr_next := cntr + 1.U
        when (cntr === (params.numPoints - 2).U) { state_next := sDone }
      }
    }
    is (sDone) {
      when      (io.in.fire())  { state_next := sComp }
      .elsewhen (io.out.fire()) { state_next := sIdle }
    }
  }

  when (state_next === sComp && state =/= sComp) {
    // Reset counter
    cntr_next := 0.U
  }

  cntr  := cntr_next
  state := state_next
}

/**
 * Radix-4 SDF FFT Chain
 *
 * Instantiates and connects SDF FFT stages in series and provides necessary control signals for each stage
 */
class SDFChainRadix4[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  // At this point, "opt" decimType should already have been resolved to "dit" or "dif"
  require(Seq("dit", "dif").contains(params.decimType), s"""Decimation type must either be dit or dif""")
  val io = IO(SISOIO(params))

  // Calculation of constants
  val numPointsDiv2     = params.numPoints / 2                                                                // FFT size / 2
  val numStages         = log2Up(params.numPoints) / 2                                                            // required number of SDF stages for given FFT size
  val delayLog4s        = if (params.decimType == "dit") (0 until numStages) else (0 until numStages).reverse // log2(delay) of each nth stage
  val delays            = delayLog4s.map(d => pow(4, d).toInt)                                                // actual delay of each nth stage
  val latencies = delays.map((4 - 1) * _)
  val cumulative_latencies = latencies.scanLeft(0)(_ + _)                                                           // Cumulative delay up to (and including) nth stage

  // FSM states for control logic
  val sIdle :: sComp :: sDone :: Nil = Enum(3)
  val state      = RegInit(sIdle)
  val state_next = Wire(state.cloneType)

  // Counter for control logic
  val cntr      = RegInit(0.U(log2Up(params.numPoints).W))
  val cntr_next = Wire(cntr.cloneType)

  // Instantiate and connect control signals of stages
  val sdf_stages = delays.zip(cumulative_latencies).map {
    case (delay, cumulative_latency) => {
      val stage = Module(new SDFStageRadix4(params, delay=delay))
      stage.io.cntr         := (cntr - cumulative_latency.U)
      stage.io.en           := io.in.fire()
      stage
    }
  }

  // Connect datapath of stages in series
  sdf_stages.map(_.io).foldLeft(RegNext(io.in.bits))((stg_in, stg_io) => {
    stg_io.in := stg_in
    stg_io.out
  })

  // Output interface connections
  // TODO: Do we need a Queue?
  io.out.bits  := sdf_stages.last.io.out
  io.out.valid := ShiftRegister(io.in.fire(), cumulative_latencies.last + 1, resetData=false.B, en=true.B)
  io.in.ready  := io.out.ready

  // Controller FSM
  cntr_next  := cntr
  state_next := state

  switch (state) {
    is (sIdle) {
      when (io.in.fire()) { state_next := sComp }
    }
    is (sComp) {
      when (io.in.fire()) {
        cntr_next := cntr + 1.U
        when (cntr === (params.numPoints - 2).U) { state_next := sDone }
      }
    }
    is (sDone) {
      when      (io.in.fire())  { state_next := sComp }
      .elsewhen (io.out.fire()) { state_next := sIdle }
    }
  }

  when (state_next === sComp && state =/= sComp) {
    // Reset counter
    cntr_next := 0.U
  }

  cntr  := cntr_next
  state := state_next
}

/**
 * Bundle type as IO for direct FFT stage
 */
class SDFStageIO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  // datapath
  val in  = Input(PacketBundle(1, params.protoIQ))
  val out = Output(PacketBundle(1, params.protoIQ))
  // control
  val cntr         = Input(UInt(log2Up(params.numPoints).W))
  val en           = Input(Bool())

  override def cloneType: this.type = SDFStageIO(params).asInstanceOf[this.type]
}
object SDFStageIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): SDFStageIO[T] = new SDFStageIO(params)
}

/**
 * Stage for radix-2 SDF FFT
 */
class SDFStageRadix2[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val delay: Int) extends Module {
  params.checkNumPointsPow2()
  require(isPow2(delay) && delay >= 1, "delay must be a power of 2 greater than or equal to 1")
  require(Seq("dit", "dif").contains(params.decimType), s"""Decimation type must either be dit or dif""")

  val io = IO(SDFStageIO(params))

  val inp = Wire(params.protoIQ.cloneType)
  val out = Wire(params.protoIQ.cloneType)

  // Generate ROM of twiddle factors
  val twiddles_rom = Wire(Vec(delay, params.protoTwiddle.cloneType))
  (0 until delay).map(n => {
    twiddles_rom(n).real := Real[T].fromDouble( cos(2 * Pi / (2 * delay) * n))
    twiddles_rom(n).imag := Real[T].fromDouble(-sin(2 * Pi / (2 * delay) * n))
  })

  // Apply twiddle factor at the input or output, depending on whether it's DIT or DIF
  if (params.decimType == "dit") {
    // Issue: using `inp := Mux(use_twiddle, io.in.iq(0) * twiddle, io.in.iq(0)` causes the following error:
    // can't create Mux with non-equivalent types dsptools.numbers.DspComplex@________ and dsptools.numbers.DspComplex@________
    when (io.cntr > delay.U) {
      inp := io.in.iq(0) * twiddles_rom(io.cntr - delay.U)
    } .otherwise {
      inp := io.in.iq(0)
    }
    io.out.iq(0) := out
  } else {
    inp := io.in.iq(0)
    when (io.cntr < delay.U && io.cntr =/= 0.U) {
      io.out.iq(0) := out * twiddles_rom(io.cntr)
    } .otherwise {
      io.out.iq(0) := out
    }
  }

  val butterfly_outputs = Seq.fill(2)(Wire(params.protoIQ.cloneType))

  val load_input = io.cntr < delay.U
  val shift_in   = Mux(load_input, inp, butterfly_outputs(1))
  val shift_out  = ShiftRegister(shift_in, delay, en=io.en)

  Butterfly[T](Seq(shift_out, inp)).zip(butterfly_outputs).foreach { case (out_val, out_wire) => out_wire := out_val }

  out := Mux(load_input, shift_out, butterfly_outputs(0))

  io.out.pktStart := ShiftRegister(io.in.pktStart, delay, en=io.en)
  io.out.pktEnd   := ShiftRegister(io.in.pktEnd  , delay, en=io.en)
}

/**
 * Stage for radix-4 SDF FFT
 */
class SDFStageRadix4[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val delay: Int) extends Module {
  params.checkNumPointsPow2()
  require(isPow2(delay) && delay >= 1, "delay must be a power of 2 greater than or equal to 1")
  require(Seq("dit", "dif").contains(params.decimType), s"""Decimation type must either be dit or dif""")

  val io = IO(SDFStageIO(params))

  val inp = Wire(params.protoIQ.cloneType)
  val out = Wire(params.protoIQ.cloneType)

  val subSize = 4 * delay
  val maxIndex = (4 - 1) * (delay - 1)
  val latency = (4 - 1) * delay

  // Generate ROM of twiddle factors
  val twiddles_rom = Wire(Vec(maxIndex + 1, params.protoTwiddle.cloneType))
  (0 to maxIndex).map(n => {
    twiddles_rom(n).real := Real[T].fromDouble( cos(2 * Pi / subSize * n))
    twiddles_rom(n).imag := Real[T].fromDouble(-sin(2 * Pi / subSize * n))
  })

  val cntr_msb = log2Ceil(subSize) - 1
  val in_counter = io.cntr(cntr_msb, 0)
  val out_counter = in_counter +% delay.U

  val butterfly_outputs = Seq.fill(4)(Wire(params.protoIQ.cloneType))

  val store_output = out_counter(cntr_msb, cntr_msb - 1) === 0.U

  // Apply twiddle factor at the input or output, depending on whether it's DIT or DIF
  if (delay == 1) {
    io.out.iq(0) := out
    inp := io.in.iq(0)
  } else if (params.decimType == "dit") {
    io.out.iq(0) := out
    when (in_counter(cntr_msb, cntr_msb - 1) =/= 0.U) {
      inp := io.in.iq(0) * twiddles_rom(in_counter(cntr_msb, cntr_msb - 1) * in_counter(cntr_msb - 2, 0))
    } .otherwise {
      inp := io.in.iq(0)
    }
  } else {
    inp := io.in.iq(0)
    when (!store_output) {
      io.out.iq(0) := out * twiddles_rom(out_counter(cntr_msb, cntr_msb - 1) * out_counter(cntr_msb - 2, 0))
    } .otherwise {
      io.out.iq(0) := out
    }
  }

  val shift_outs = VecInit((1 until 4).map(i => {
    val shift_in = Mux(store_output, butterfly_outputs(i), inp)
    ShiftRegister(shift_in, delay, en=io.en && (out_counter(cntr_msb, cntr_msb - 1) === i.U || store_output))
  }))

  Butterfly[T](shift_outs :+ inp, params.protoTwiddle).zip(butterfly_outputs).foreach {
    case (out_val, out_wire) => out_wire := out_val
  }

  out := Mux(store_output, butterfly_outputs(0), shift_outs(out_counter(cntr_msb, cntr_msb - 1) - 1.U))

  io.out.pktStart := ShiftRegister(io.in.pktStart, latency, en=io.en)
  io.out.pktEnd   := ShiftRegister(io.in.pktEnd  , latency, en=io.en)
}
