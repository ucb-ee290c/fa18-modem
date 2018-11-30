package modem

import breeze.math.Complex
import dsptools.DspTester

/**
  * Case class holding information needed to run an individual test
  */
case class CFOIQ(
  // input iq vectors
  iqin: Seq[Complex],
  iqout: Option[Seq[Complex]] = None
)

/**
  * DspTester for CFO
  *
  * Run each trial in @trials
  */
class CFOEstimationTester[T <: chisel3.Data](c: CFOEstimation[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  var count = 0
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  // var outVec = Vector[Complex]()
  for(trial <- trials){
     poke(c.io.in.bits.pktStart,0)
     poke(c.io.in.bits.pktEnd, 1)
     step(2)
      poke(c.io.in.bits.pktStart, 1)
      poke(c.io.in.bits.pktEnd, 0)
      expect(c.io.curState, 0)
    for(iq <- trial.iqin){
      count = 0
      //poke(c.io.in.bits.pktStart, 1)
      //poke(c.io.in.bits.pktEnd, 0)
      poke(c.io.in.bits.iq(0), iq)
      print(iq)
      poke(c.io.in.valid, 1)
      peek(c.io.pErr)
      peek(c.io.cErr)
      peek(c.io.fErr)
      peek(c.io.cordErr)
      peek(c.io.curState)
      peek(c.io.stAcc)
      peek(c.io.ltAcc)
      peek(c.io.delaySTIQ)
      peek(c.io.delaySTVal)
      step(1)
    }
  }
}

class COETester[T <: chisel3.Data](c: COEWrapper[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  var count = 0
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  // var outVec = Vector[Complex]()
  for(trial <- trials){
     poke(c.io.in.valid, 0)
     step(2)
     poke(c.io.in.valid, 1)
    for(iq <- trial.iqin){
      count = 0
      //poke(c.io.in.bits.pktStart, 1)
      //poke(c.io.in.bits.pktEnd, 0)
      poke(c.io.in.bits, iq)
      peek(c.io.stMul)
      peek(c.io.stAcc)
      //print(s"$iq\n")
      peek(c.io.cordicIn.x)
      peek(c.io.cordicIn.y)
      peek(c.io.cordicIn.z)
      peek(c.io.cordicIn.vectoring)
      peek(c.io.cordicInVal)
      peek(c.io.cordicOut.x)
      peek(c.io.cordicOut.y)
      peek(c.io.cordicOut.z)
      //peek(c.io.cordicOut.bits.vectoring)
      peek(c.io.cordicOutVal)
      peek(c.io.delayIQ)
      peek(c.io.delayValid)
      peek(c.io.out.bits)
      peek(c.io.out.valid)

      step(1)
    }
    poke(c.io.in.valid, 0)
    while(!peek(c.io.out.valid)){
      step(1)
    }
    peek(c.io.cordicOut.z)
    peek(c.io.out.bits)
    //for (i <- 0 until 32){
      //peek(c.io.stMul)
      //peek(c.io.stAcc)
      ////print(s"$iq\n")
      //peek(c.io.cordicIn.x)
      //peek(c.io.cordicIn.y)
      //peek(c.io.cordicIn.z)
      //peek(c.io.cordicIn.vectoring)
      //peek(c.io.cordicInVal)
      //peek(c.io.cordicOut.x)
      //peek(c.io.cordicOut.y)
      //peek(c.io.cordicOut.z)
      ////peek(c.io.cordicOut.bits.vectoring)
      //peek(c.io.cordicOutVal)
      //peek(c.io.delayIQ)
      //peek(c.io.delayValid)
      //peek(c.io.out.bits)
      //peek(c.io.out.valid)
      //step(1)
    //}
  }
}


class FOETester[T <: chisel3.Data](c: FOEWrapper[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  var count = 0
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  // var outVec = Vector[Complex]()
  for(trial <- trials){
     poke(c.io.in.valid, 0)
     step(2)
     poke(c.io.in.valid, 1)
    for(iq <- trial.iqin){
      count = 0
      //poke(c.io.in.bits.pktStart, 1)
      //poke(c.io.in.bits.pktEnd, 0)
      poke(c.io.in.bits, iq)
      peek(c.io.ltMul)
      peek(c.io.ltAcc)
      //print(s"$iq\n")
      peek(c.io.cordicIn.x)
      peek(c.io.cordicIn.y)
      peek(c.io.cordicIn.z)
      peek(c.io.cordicIn.vectoring)
      peek(c.io.cordicInVal)
      peek(c.io.cordicOut.x)
      peek(c.io.cordicOut.y)
      peek(c.io.cordicOut.z)
      //peek(c.io.cordicOut.bits.vectoring)
      peek(c.io.cordicOutVal)
      peek(c.io.delayIQ)
      peek(c.io.delayValid)
      peek(c.io.out.bits)
      peek(c.io.out.valid)

      step(1)
    }
    poke(c.io.in.valid, 0)
    for (i <- 0 until 32){
      peek(c.io.ltMul)
      peek(c.io.ltAcc)
      //print(s"$iq\n")
      peek(c.io.cordicIn.x)
      peek(c.io.cordicIn.y)
      peek(c.io.cordicIn.z)
      peek(c.io.cordicIn.vectoring)
      peek(c.io.cordicInVal)
      peek(c.io.cordicOut.x)
      peek(c.io.cordicOut.y)
      peek(c.io.cordicOut.z)
      //peek(c.io.cordicOut.bits.vectoring)
      peek(c.io.cordicOutVal)
      peek(c.io.delayIQ)
      peek(c.io.delayValid)
      peek(c.io.out.bits)
      peek(c.io.out.valid)
      step(1)
    }
  }
}

/**
  * Convenience function for running tests
  */
object FixedCFOEstimationTester {
  def apply(params: FixedCFOParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new CFOEstimation(params)) {
      c => new CFOEstimationTester(c, trials)
    }
  }
}

object FixedCOETester {
  def apply(params: FixedCFOParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new COEWrapper(params, 16)) {
      c => new COETester(c, trials)
    }
  }
}

object FixedFOETester {
  def apply(params: FixedCFOParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new FOEWrapper(params, 64)) {
      c => new FOETester(c, trials)
    }
  }
}
// object RealCFOCorrectionTester {
//   def apply(params: CFOCorrectionParams[dsptools.numbers.DspReal], trials: Seq[IQ]): Boolean = {
//     chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CFOCorrection(params)) {
//       c => new CFOTester(c, trials)
//     }
//   }
// }
