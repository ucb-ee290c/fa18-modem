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
  var pktStart = false
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  // var outVec = Vector[Complex]()
  for(trial <- trials){
    count = 0
     poke(c.io.in.bits.pktStart,0)
     poke(c.io.in.bits.pktEnd, 1)
     //peek(c.io.curState)
     step(2)
      poke(c.io.in.bits.pktStart, 1)
      poke(c.io.in.bits.pktEnd, 0)
      //expect(c.io.curState, 0)
    for(iq <- trial.iqin){
      //count = 0
      //print(s"count = $count\n")
      //poke(c.io.in.bits.pktStart, 1)
      //poke(c.io.in.bits.pktEnd, 0)
      poke(c.io.in.bits.iq(0), iq)
      //print(iq)
      poke(c.io.in.valid, 1)
      peek(c.io.pErr)
      //peek(c.io.cErr)
      //peek(c.io.fErr)
      //peek(c.io.cordErr)
      //peek(c.io.curState)
      //if(count < 1 && !pktStart){
        //expect(c.io.curState, 0.0)
      //}else if(count < 161 && count >= 1){
        //expect(c.io.curState, 1.0)
      //}else if(count < 193 && count >= 161){
        //expect(c.io.curState, 2.0)
      //}else if(count < 321 && count >= 193){
        //expect(c.io.curState, 3.0)
      //}else{
        //expect(c.io.curState, 4.0)
      //}
      //peek(c.io.stAcc)
      //peek(c.io.ltAcc)
      //peek(c.io.delaySTIQ)
      //peek(c.io.delaySTVal)
      //peek(c.io.stCounter)
      //peek(c.io.giCounter)
      //peek(c.io.ltCounter)
      peek(c.io.out.bits.iq)
      step(1)
      count = count + 1
      pktStart = true
    }
    //step(2)
    //peek(c.io.curState)
  }
}

class CFOCorrectionTester[T <: chisel3.Data](c: CFOCorrection[T], trials: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  var count = 0
  var pktStart = false
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  // var outVec = Vector[Complex]()
  for(trial <- trials){
    count = 0
     poke(c.io.in.bits.pktStart,0)
     poke(c.io.in.bits.pktEnd, 1)
     //peek(c.io.curState)
     step(2)
      poke(c.io.in.bits.pktStart, 1)
      poke(c.io.in.bits.pktEnd, 0)
      //expect(c.io.curState, 0)
    for(iq <- trial.iqin){
      //count = 0
      //print(s"count = $count\n")
      //poke(c.io.in.bits.pktStart, 1)
      //poke(c.io.in.bits.pktEnd, 0)
      poke(c.io.in.bits.iq(0), iq)
      peek(c.io.prOut)
      //print(iq)
      poke(c.io.in.valid, 1)
      peek(c.io.pErr)
      peek(c.io.cErr)
      peek(c.io.fErr)
      //peek(c.io.cordErr)
      //peek(c.io.curState)
      //if(count < 1 && !pktStart){
        //expect(c.io.curState, 0.0)
      //}else if(count < 161 && count >= 1){
        //expect(c.io.curState, 1.0)
      //}else if(count < 193 && count >= 161){
        //expect(c.io.curState, 2.0)
      //}else if(count < 321 && count >= 193){
        //expect(c.io.curState, 3.0)
      //}else{
        //expect(c.io.curState, 4.0)
      //}
      //peek(c.io.stAcc)
      //peek(c.io.ltAcc)
      //peek(c.io.delaySTIQ)
      //peek(c.io.delaySTVal)
      //peek(c.io.stCounter)
      //peek(c.io.giCounter)
      //peek(c.io.ltCounter)
      peek(c.io.out.bits.iq)
      step(1)
      count = count + 1
      pktStart = true
    }
    //step(2)
    //peek(c.io.curState)
  }
}

//class COETester[T <: chisel3.Data](c: COEWrapper[T], trials: Seq[IQ], tolLSBs: Int = 3, cfo: Int = 5000) extends DspTester(c) {
  //val maxWaitCycles = 100
  //var count = 0
  //poke(c.io.in.valid, 0)
  //poke(c.io.out.ready, 1)

  //for(trial <- trials){
     //poke(c.io.in.valid, 0)
     //step(2)
     //poke(c.io.in.valid, 1)
    //for(iq <- trial.iqin){
      //count = 0
      //poke(c.io.in.bits, iq)
      //step(1)
    //}
    //poke(c.io.in.valid, 0)
    //while(!peek(c.io.out.valid)){
      //step(1)
    //}
    //peek(c.io.cordicOut.z)
    //fixTolLSBs.withValue(tolLSBs){
      //expect(c.io.out.bits, -2 * math.Pi * cfo/ 20.0e6)
    //}
  //}
//}


//class FOETester[T <: chisel3.Data](c: FOEWrapper[T], trials: Seq[IQ], tolLSBs: Int = 3, cfo: Int = 100) extends DspTester(c) {
  //val maxWaitCycles = 100
  //var count = 0
  //poke(c.io.in.valid, 0)
  //poke(c.io.out.ready, 1)

  //for(trial <- trials){
     //poke(c.io.in.valid, 0)
     //step(2)
     //poke(c.io.in.valid, 1)
    //for(iq <- trial.iqin){
      //count = 0
      //poke(c.io.in.bits, iq)
      //step(1)
    //}
    //poke(c.io.in.valid, 0)
    //while(!peek(c.io.out.valid)){
      //step(1)
    //}
    //peek(c.io.cordicOut.z)
    //fixTolLSBs.withValue(tolLSBs){
      //expect(c.io.out.bits, -2 * math.Pi * cfo/ 20.0e6)
    //}
  //}
//}

/**
  * Convenience function for running tests
  */
object FixedCFOCorrectionTester {
  def apply(params: FixedCFOParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new CFOCorrection(params)) {
      c => new CFOCorrectionTester(c, trials)
    }
  }
}

//object FixedCFOEstimationTester {
  //def apply(params: FixedCFOParams, trials: Seq[IQ]): Boolean = {
    //chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new CFOEstimation(params)) {
      //c => new CFOEstimationTester(c, trials)
    //}
  //}
//}

//object FixedCOETester {
  //def apply(params: FixedCFOParams, trials: Seq[IQ], cfo: Int): Boolean = {
    //chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new COEWrapper(params, 16)) {
      //c => new COETester(c, trials, cfo)
    //}
  //}
//}

//object FixedFOETester {
  //def apply(params: FixedCFOParams, trials: Seq[IQ], cfo: Int): Boolean = {
    //chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new FOEWrapper(params, 64)) {
      //c => new FOETester(c, trials, cfo)
    //}
  //}
//}
// object RealCFOCorrectionTester {
//   def apply(params: CFOCorrectionParams[dsptools.numbers.DspReal], trials: Seq[IQ]): Boolean = {
//     chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CFOCorrection(params)) {
//       c => new CFOTester(c, trials)
//     }
//   }
// }
