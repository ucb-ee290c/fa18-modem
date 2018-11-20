package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._

trait CyclicPrefixParams[T<:Data] {
  val protoIQ: DspComplex[T] // IQ sample prototype
  val prefixLength: Int      // length of prefix in samples
  val symbolLength: Int      // length of body of symbol in samples
}

/**
  * Bundle type as IO for cyclic prefix modules
  */
class CyclicPrefixIO[T <: Data](params: CyclicPrefixParams[T]) extends Bundle {
  val in = Flipped(Decoupled(PacketBundle(PacketBundleParams(1, params.protoIQ))))
  val out = Decoupled(SerialPacketBundle(PacketBundleParams(1, params.protoIQ)))
  val add = Input(Bool())

  override def cloneType: this.type = CyclicPrefixIO(params).asInstanceOf[this.type]
}
object CyclicPrefixIO {
  def apply[T <: Data](params: CyclicPrefixParams[T]): CyclicPrefixIO[T] =
    new CyclicPrefixIO(params)
}


/**
 * Cyclic Prefix removal and addition
 * Forces a one-cycle delay between packets to make state transition logic easier.
 */
 class CyclicPrefix[T<:Data](params: CyclicPrefixParams[T]) extends Module {
    val io = IO(CyclicPrefixIO(params))
    io.in.ready := false.B // For safety
    io.out.valid := false.B
    io.out.bits := io.in.bits(1)

    val sRemove :: sKeep :: sFill :: sAdd :: sDrain :: sIdle :: Nil = Enum(6)
    val state = RegInit(sIdle)
    val nextState = Wire(sRemove.cloneType)
    state := nextState
    nextState := state

    val rmCount = RegInit(0.U(32.W))
    val addCount = RegInit(0.U(32.W))
    val keepCount = RegInit(0.U(32.W))

    val pktStart = Reg(Bool())
    pktStart := pktStart
    val pktEnd = Reg(Bool())
    pktEnd := pktEnd

    val buffer = Reg(Vec(params.symbolLength, params.protoIQ))
    buffer := buffer

    switch(state) {
      is(sIdle) {
        // printf("IDLE\n")
        io.in.ready := true.B
        rmCount := 1.U
        addCount := 0.U
        keepCount := 1.U
        pktStart := true.B
        pktEnd := false.B
        buffer(0) := io.in.bits.iq(0)
        nextState := Mux(!io.in.fire(), sIdle,
                          Mux(io.add, sFill, sRemove))
      }
      is(sRemove) {
        // printf("REMOVE %d\n", rmCount)
        io.in.ready := true.B
        io.out.valid := false.B
        rmCount := rmCount + io.in.fire()
        keepCount := 0.U
        nextState := Mux(rmCount < (params.prefixLength-1).U, sRemove, sKeep)
      }
      is(sKeep) {
        // printf("KEEP\n")
        // Output logic
        io.in.ready := io.out.ready
        io.out.valid := io.in.valid
        io.out.bits.iq := io.in.bits.iq
        // Propagate a packet start if this is the first keep loop
        io.out.bits.pktStart := pktStart
        io.out.bits.pktEnd := io.in.bits.pktEnd
        pktStart := false.B
        // Transition logic
        rmCount := 0.U
        keepCount := keepCount + io.in.fire()
        nextState := Mux(keepCount < (params.symbolLength-1).U, sKeep,
                         Mux(io.in.bits.pktEnd, sIdle, sRemove))
      }
      is(sFill) {
        // printf("FILL %d\n", keepCount)
        // If we really want to be efficient, we could immediately start sending
        // the prefix once the last n samples start to arrive, but that's overkill
        // for such a low latency block
        io.in.ready := true.B
        io.out.valid := false.B
        pktEnd := io.out.bits.pktEnd
        addCount := 0.U
        rmCount := 0.U
        keepCount := keepCount + io.in.fire()
        buffer(keepCount) := io.in.bits.iq(0)
        nextState := Mux(keepCount < (params.symbolLength-1).U, sFill, sAdd)
      }
      is(sAdd) {
        // printf("ADD %d\n", addCount)
        // Push out prefix now
        io.in.ready := false.B
        io.out.valid := true.B
        keepCount := 0.U
        rmCount := 0.U
        addCount := addCount + io.out.fire()
        io.out.bits.iq(0) := buffer((params.symbolLength - params.prefixLength).U + addCount)
        io.out.bits.pktStart := pktStart
        pktStart := false.B
        io.out.bits.pktEnd := false.B
        nextState := Mux(addCount < (params.prefixLength-1).U, sAdd, sDrain)
      }
      is(sDrain) {
        // printf("DRAIN %d\n", rmCount)
        // Push out iq buffer now
        io.in.ready := false.B
        io.out.valid := true.B
        addCount := 0.U
        keepCount := 0.U
        rmCount := rmCount + io.out.fire()
        io.out.bits.iq(0) := buffer(rmCount)
        io.out.bits.pktStart := false.B
        io.out.bits.pktEnd := pktEnd && nextState =!= sDrain // Wait until last sample for pktEnd
        pktEnd := Mux(nextState =!= sDrain, false.B, pktEnd)
        nextState := Mux(rmCount < (params.symbolLength-1).U, sDrain,
                          Mux(pktEnd, sIdle, sFill))
      }
    }
 }
