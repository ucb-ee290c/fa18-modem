package modem

import chisel3._
import chisel3.util._
import dspblocks._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

/**
  * The memory interface writes entries into the queue.
  * They stream out the streaming interface
  * @param depth number of entries in the queue
  * @param streamParameters parameters for the stream node
  * @param p
  */
abstract class WriteQueue
(
  val depth: Int = 8,
  val streamParameters: AXI4StreamMasterParameters = AXI4StreamMasterParameters()
)(implicit p: Parameters) extends LazyModule with HasCSR {
  // stream node, output only
  val streamNode = AXI4StreamMasterNode(streamParameters)

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.out.length == 1)

    // get the output bundle associated with the AXI4Stream node
    val out = streamNode.out(0)._1
    // width (in bits) of the output interface
    val width = out.params.n * 8
    // instantiate a queue
    val queue = Module(new Queue(UInt(out.params.dataBits.W), depth))
    // connect queue output to streaming output
    out.valid := queue.io.deq.valid
    out.bits.data := queue.io.deq.bits
    // don't use last
    out.bits.last := false.B
    queue.io.deq.ready := out.ready

    regmap(
      // each write adds an entry to the queue
      0x0 -> Seq(RegField.w(width, queue.io.enq)),
      // read the number of entries in the queue
      (width+7)/8 -> Seq(RegField.r(width, queue.io.count)),
    )
  }
}

/**
  * TLDspBlock specialization of WriteQueue
  * @param depth number of entries in the queue
  * @param csrAddress address range for peripheral
  * @param beatBytes beatBytes of TL interface
  * @param p
  */
class TLWriteQueue
(
  depth: Int = 8,
  csrAddress: AddressSet = AddressSet(0x2000, 0xff),
  beatBytes: Int = 8,
)(implicit p: Parameters) extends WriteQueue(depth) with TLHasCSR {
  val devname = "tlQueueIn"
  val devcompat = Seq("ucb-art", "dsptools")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem = Some(TLRegisterNode(address = Seq(csrAddress), device = device, beatBytes = beatBytes))
}

/**
  * The streaming interface adds elements into the queue.
  * The memory interface can read elements out of the queue.
  * @param depth number of entries in the queue
  * @param streamParameters parameters for the stream node
  * @param p
  */
abstract class ReadQueue
(
  val depth: Int = 8,
  val streamParameters: AXI4StreamSlaveParameters = AXI4StreamSlaveParameters()
)(implicit p: Parameters) extends LazyModule with HasCSR {
  val streamNode = AXI4StreamSlaveNode(streamParameters)

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)

    // TODO

    // get the input bundle associated with the AXI4Stream node
    val in = streamNode.in(0)._1
    // width (in bits) of the input interface
    val width = in.params.n * 8
    // instantiate a queue
    val queue = Module(new Queue(UInt(in.params.dataBits.W), depth))
    // connect queue input to streaming input
    queue.io.enq.valid := in.valid
    queue.io.enq.bits := in.bits.data
    // don't use last?
    // out.bits.last := false.B
    in.ready := queue.io.enq.ready

    regmap(
      // each read removes an entry to the queue
      0x0 -> Seq(RegField.r(width, queue.io.deq)),
      // read the number of entries in the queue
      (width+7)/8 -> Seq(RegField.r(width, queue.io.count)),
    )
  }

}

/**
  * TLDspBlock specialization of ReadQueue
  * @param depth number of entries in the queue
  * @param csrAddress address range
  * @param beatBytes beatBytes of TL interface
  * @param p
  */
class TLReadQueue
(
  depth: Int = 8,
  csrAddress: AddressSet = AddressSet(0x2100, 0xff),
  beatBytes: Int = 8
)(implicit p: Parameters) extends ReadQueue(depth) with TLHasCSR {
  val devname = "tlQueueOut"
  val devcompat = Seq("ucb-art", "dsptools")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem = Some(TLRegisterNode(address = Seq(csrAddress), device = device, beatBytes = beatBytes))

}

/**
  * Make DspBlock wrapper for CORDIC
  * @param fftParams parameters for fft
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam D
  * @tparam U
  * @tparam EO
  * @tparam EI
  * @tparam B
  * @tparam T Type parameter for fft, i.e. FixedPoint or DspReal
  */
abstract class FFTBlock[D, U, EO, EI, B <: Data, T <: Data : Real : BinaryRepresentation : ChiselConvertableFrom]
(
  val fftParams: FFTParams[T]
)(implicit p: Parameters) extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()
  val mem = None

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    val fft_bundle = FFTIO(fftParams).in.bits

    val descriptorWidth: Int = fft_bundle.getWidth + 1 // + 1 because of vectoring
    require(descriptorWidth <= in.params.n * 8, "Streaming interface too small")

    // TODO

    // instantiate a CORDIC
    val fft = Module(new FFT(fftParams))

    fft.io.in.valid := in.valid

    // val xyWidth = fft_bundle.x.getWidth
    // val zWidth = fft_bundle.z.getWidth
    // fft.io.in.bits := in.bits.data(2 * xyWidth + zWidth - 1, 0).asTypeOf(fft_bundle)
    // fft.io.vectoring := in.bits.data(2 * xyWidth + zWidth)

    in.ready := fft.io.in.ready

    out.valid := fft.io.out.valid
    out.bits.data := fft.io.out.bits.asUInt()
    // don't use last
    out.bits.last := false.B
    fft.io.out.ready := out.ready
  }
}

/**
  * TLDspBlock specialization of FFTBlock
  * @param fftParams parameters for fft
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for fft data type
  */
class TLFFTBlock[T <: Data : Real : BinaryRepresentation : ChiselConvertableFrom]
(
  fftParams: FFTParams[T]
)(implicit p: Parameters) extends
  FFTBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T](fftParams)
  with TLDspBlock

/**
  * TLChain is the "right way" to do this, but the dspblocks library seems to be broken.
  * In the interim, this should work.
  * @param fftParams parameters for fft
  * @param depth depth of queues
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for fft, i.e. FixedPoint or DspReal
  */
class FFTThing[T <: Data : Real : BinaryRepresentation : ChiselConvertableFrom]
(
  val fftParams: FFTParams[T],
  val depth: Int = 8,
)(implicit p: Parameters) extends LazyModule {
  // instantiate lazy modules
  val writeQueue = LazyModule(new TLWriteQueue(depth))
  val fft = LazyModule(new TLFFTBlock(fftParams))
  val readQueue = LazyModule(new TLReadQueue(depth))

  // connect streamNodes of queues and fft
  readQueue.streamNode := fft.streamNode := writeQueue.streamNode

  lazy val module = new LazyModuleImp(this)
}
