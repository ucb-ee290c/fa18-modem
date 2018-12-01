package modem

import scala.math._
import breeze.math.Complex
import breeze.numerics.{cos, sin, sqrt}
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
class CFOEstimationTester[T <: chisel3.Data](c: CFOEstimation[T], trials: Seq[IQ], reals: Seq[IQ], tolLSBs: Int = 2) extends DspTester(c) {
  val maxWaitCycles = 100
  var count = 0
  var phi0: Double = 0
  var phi: Double = 0
  var phiNext: Double = 0
  var pErr: Double = 0.0
  var rotIQ = Complex(0,0)
  var lastIQ = Complex(0,0)
  var accumPErr = Complex(0,0)
  var corrPErr = Complex(0,0)
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)
  
  val tries = trials zip reals
   var outVec = Vector[Complex]()
   var phiVec = Vector[Double]()
  for((trial, actual) <- tries){
    count = 0
     poke(c.io.in.bits.pktStart,0)
     poke(c.io.in.bits.pktEnd, 1)
     //peek(c.io.curState)
     step(2)
      poke(c.io.in.bits.pktStart, 1)
      poke(c.io.in.bits.pktEnd, 0)
      //expect(c.io.curState, 0)
      val trialiq = trial.iqin zip actual.iqin
    for((iq, actiq) <- trialiq){
        print(s"TV: $iq\n")
        //accumPErr = iq.conjugate * actiq
        phi0 = peek(c.io.pErr)
        phiNext = (phi + phi0) //% math.Pi
      //if(peek(c.io.curState) == 2.0){
        rotIQ = iq * Complex(cos(phi), sin(phi))
        print(s"TVr: $rotIQ\n")
        corrPErr = rotIQ.conjugate * actiq
        poke(c.io.in.bits.iq(0), rotIQ)
        //print(s"Corr PE: $corrPErr\n")
        //poke(c.io.in.bits.iq(0).imag, rotIQ)
      //}else{
        //poke(c.io.in.bits.iq(0), iq)
      //}
        //print(s"Acc PE: $accumPErr\n")
        accumPErr = rotIQ.conjugate * lastIQ
        pErr = atan2(accumPErr.real, accumPErr.imag)
       print(s"SV: $actiq\n")
       print(s"Phi: $phi\n")
       print(s"PERR: $pErr\n")
      //count = 0
      //print(s"count = $count\n")
      //poke(c.io.in.bits.pktStart, 1)
      //poke(c.io.in.bits.pktEnd, 0)
      //print(iq)
      poke(c.io.in.valid, 1)
      peek(c.io.pErr)
      peek(c.io.cErr)
      peek(c.io.fErr)
      //peek(c.io.cordErr)
      peek(c.io.curState)
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
      phi = phiNext
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
      //lastIQ = rotIQ
      outVec = outVec :+ rotIQ
      phiVec = phiVec :+ phi
    }
    //val checVecs = outVec zip trial
    var ltAccum = 0.0
    var ltMul = Complex(0,0)
    var ltPhase = 0.0
    var ltError = 0.0
    var curIQ = Complex(0,0)
    var curPhi = 0.0
    for (i <- 192 until 321){
      if(i > 255){
        ltMul = outVec(i).conjugate * outVec(i - 64)
        ltPhase = atan2(ltMul.real, ltMul.imag)
        ltAccum = ltAccum + ltPhase
      }
      ltError = ltAccum / 64
      curIQ = outVec(i)
      curPhi = phiVec(i)
      print(s"IQ: $curIQ  ")
      print(s"Phi: $curPhi  ")
      print(s"ltMul: $ltPhase ")
      print(s"ltAccum: $ltAccum ")
      print(s"ltError: $ltError\n")
    }
    //step(2)
    //peek(c.io.curState)
  }
}

class CFOCorrectionTester[T <: chisel3.Data](c: CFOCorrection[T], trials: Seq[IQ], reals: Seq[IQ], cfo: Int , tolLSBs: Int = 2) extends DspTester(c) {
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  for((trial, real) <- tries){
    val testinst = trial.iqin zip real.iqin
      poke(c.io.in.bits.pktStart,0)
      poke(c.io.in.bits.pktEnd, 1)
      step(2)
      poke(c.io.in.bits.pktStart, 1)
      poke(c.io.in.bits.pktEnd, 0)
    for((trialiq, realiq) <- testinst){
      poke(c.io.in.bits.iq(0), trialiq)
      poke(c.io.in.valid, 1)
      peek(c.io.pErr)
      peek(c.io.cErr)
      peek(c.io.fErr)
      step(1)
    }
    fixTolLSBs.withValue(tolLSBs){
      expect(c.io.pErr, (-2 * math.Pi * cfo)/ 20.0e6)
    }
  }
}

class COETester[T <: chisel3.Data](c: COEWrapper[T], trials: Seq[IQ], tolLSBs: Int = 3, cfo: Int) extends DspTester(c) {
  val maxWaitCycles = 100
  var count = 0
  poke(c.io.in.valid, 0)
  poke(c.io.out.ready, 1)

  for(trial <- trials){
     poke(c.io.in.valid, 0)
     step(2)
     poke(c.io.in.valid, 1)
    for(iq <- trial.iqin){
      count = 0
      poke(c.io.in.bits, iq)
      step(1)
    }
    poke(c.io.in.valid, 0)
    while(!peek(c.io.out.valid)){
      step(1)
    }
    peek(c.io.cordicOut.z)
    fixTolLSBs.withValue(tolLSBs){
      expect(c.io.out.bits, -2 * math.Pi * cfo/ 20.0e6)
    }
  }
}


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
  def apply(params: FixedCFOParams, trials: Seq[IQ], reals: Seq[IQ], cfo: Int, tolLSBs: Int=3): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new CFOCorrection(params)) {
      c => new CFOCorrectionTester(c, trials, reals, cfo, tolLSBs)
    }
  }
}

object FixedCFOEstimationTester {
  def apply(params: FixedCFOParams, trials: Seq[IQ], reals: Seq[IQ], tolLSBs: Int): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new CFOEstimation(params)) {
      c => new CFOEstimationTester(c, trials, reals, tolLSBs)
    }
  }
}

object FixedCOETester {
  def apply(params: FixedCFOParams, trials: Seq[IQ], cfo: Int): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new COEWrapper(params, 16)) {
      c => new COETester(c, trials, 3 ,cfo)
    }
  }
}

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
