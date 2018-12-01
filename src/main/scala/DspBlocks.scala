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

    // TODO - IN PROGRESS
    // get the input bundle associated with the AXI4Stream node
    val in = streamNode.in(0)._1
    // width (in bits) of the input interface
    val width = in.params.n * 8
    // instantiate a queue
    val queue = Module(new Queue(UInt(in.params.dataBits.W), depth))
    // connect queue input to streaming input
    in.ready := queue.io.enq.ready
    queue.io.enq.bits := in.bits.data
    queue.io.enq.valid := in.valid
    // don't use last
    in.bits.last := false.B

    regmap(
      // each write adds an entry to the queue
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
  * Make DspBlock wrapper for RX
  * @param rxParams parameters for cordic
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam D
  * @tparam W
  * @tparam EO
  * @tparam EI
  * @tparam B
  * @tparam T Type parameter for rx data type
  * @tparam U Type parameter for rx data type
  * @tparam V Type parameter for rx data type
  */
abstract class RXBlock[D, W, EO, EI, B <: Data, T<:Data:Real:BinaryRepresentation, U<:Data:Real:BinaryRepresentation, V<:Data:Real]
(
  val rxParams: RXParams[T, U, V]
)(implicit p: Parameters) extends DspBlock[D, W, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()
  val mem = None

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    val descriptorWidth: Int = IQBundle(rxParams.iqBundleParams).getWidth
    require(descriptorWidth <= in.params.n * 8, "Streaming interface too small")

    val rx = Module (new RX(rxParams))
    // Connect input queue
    rx.io.in.bits := in.bits.data.asTypeOf(IQBundle(rxParams.iqBundleParams))
    rx.io.in.valid := in.valid
    in.ready := rx.io.in.ready
    // Connect output queue
    out.bits.data := rx.io.out.bits.asUInt()
    out.valid := rx.io.out.valid
    rx.io.out.ready := out.ready
  }
}

/**
  * TLDspBlock specialization of CordicBlock
  * @param rxParams parameters for rx
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for rx data type
  * @tparam U Type parameter for rx data type
  * @tparam V Type parameter for rx data type
  */
class TLRXBlock[T<:Data:Real:BinaryRepresentation, U<:Data:Real:BinaryRepresentation, V<:Data:Real]
(
  rxParams: RXParams[T,U,V]
)(implicit p: Parameters) extends
  RXBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T, U, V](rxParams)
  with TLDspBlock

/**
  * TLChain is the "right way" to do this, but the dspblocks library seems to be broken.
  * In the interim, this should work.
  * @param rxParams parameters for rx
  * @param depth depth of queues
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for rx data type
  * @tparam U Type parameter for rx data type
  * @tparam V Type parameter for rx data type
  */
class RXThing[T<:Data:Real:BinaryRepresentation, U<:Data:Real:BinaryRepresentation, V<:Data:Real]
(
  val rxParams: RXParams[T, U, V],
  val depth: Int = 8,
)(implicit p: Parameters) extends LazyModule {
  // instantiate lazy modules
  val writeQueue = LazyModule(new TLWriteQueue(depth))
  val rx = LazyModule(new TLRXBlock(rxParams))
  val readQueue = LazyModule(new TLReadQueue(depth))

  // connect streamNodes of queues and cordic
  readQueue.streamNode := rx.streamNode := writeQueue.streamNode

  lazy val module = new LazyModuleImp(this)
}

/**
  * Make DspBlock wrapper for TX
  * @param txParams parameters for tx
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam D
  * @tparam W
  * @tparam EO
  * @tparam EI
  * @tparam B
  * @tparam T Type parameter for tx, i.e. FixedPoint or DspReal
  * @tparam T Type parameter for tx, i.e. UInt
  */
abstract class TXBlock[D, W, EO, EI, B <: Data, T<:Data:Real:BinaryRepresentation, U<:Data]
(
  val txParams: TXParams[T, U]
)(implicit p: Parameters) extends DspBlock[D, W, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()
  val mem = None

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    val descriptorWidth: Int = IQBundle(txParams.iqBundleParams).getWidth
    require(descriptorWidth <= in.params.n * 8, "Streaming interface too small")

    val tx = Module (new TX(txParams))
    // Connect input queue
    tx.io.in.bits := in.bits.data.asTypeOf(Vec(36, UInt(1.W)))
    tx.io.in.valid := in.valid
    in.ready := tx.io.in.ready
    // Connect output queue
    out.bits.data := tx.io.out.bits.asUInt()
    out.valid := tx.io.out.valid
    tx.io.out.ready := out.ready
  }
}

/**
  * TLDspBlock specialization of TXBlock
  * @param txParams parameters for tx
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for tx data type
  * @tparam U Type parameter for tx data type
  */
class TLTXBlock[T<:Data:Real:BinaryRepresentation, U<:Data]
(
  txParams: TXParams[T ,U]
)(implicit p: Parameters) extends
  TXBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T, U](txParams)
  with TLDspBlock

/**
  * TLChain is the "right way" to do this, but the dspblocks library seems to be broken.
  * In the interim, this should work.
  * @param txParams parameters for tx
  * @param depth depth of queues
  * @param ev$1
  * @param ev$2
  * @param ev$3
  * @param p
  * @tparam T Type parameter for tx, i.e. FixedPoint or DspReal
  * @tparam U Type parameter for tx, i.e. UInt
  */
class TXThing[T<:Data:Real:BinaryRepresentation, U<:Data]
(
  val txParams: TXParams[T, U],
  val depth: Int = 8,
)(implicit p: Parameters) extends LazyModule {
  // instantiate lazy modules
  val writeQueue = LazyModule(new TLWriteQueue(depth, AddressSet(0x2200, 0xff)))
  val tx = LazyModule(new TLTXBlock(txParams))
  val readQueue = LazyModule(new TLReadQueue(depth, AddressSet(0x2300, 0xff)))

  // connect streamNodes of queues and cordic
  readQueue.streamNode := tx.streamNode := writeQueue.streamNode

  lazy val module = new LazyModuleImp(this)
}
