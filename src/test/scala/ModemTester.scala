package modem

import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
import breeze.math.Complex
import scala.math
import dsptools.DspTester

class RXTester[T <: Data, U <: Data, V<: Data](c: RX[T,U,V], trials: Seq[DspComplex[UInt]]) extends DspTester(c) {
  val maxCyclesWait = 1000 //Whatever numnber

  poke(c.io.in.valid, 1)
  poke(c.io.out.ready, 1)
  print("Placeholder")
  for(trial <- trials) {

  }
  //expect(false, "Placeholder")
}

object FixedRXTester {
  def apply(
    rxParams: RXParams[FixedPoint, SInt, UInt],
    trials: Seq[DspComplex[UInt]]): Boolean = {
    // chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new RX(iqBundleParams = iqParams, pktDetectParams = pktDetectParams, equalizerParams = eqParams, cfoParams = cfoParams, cyclicPrefixParams = cpParams, fftParams = fftParams, bitsBundleParams = bitsBundleParams, demodParams = demodParams, viterbiParams = viterbiParams)) {
    dsptools.Driver.execute(() => new RX(rxParams), TestSetup.dspTesterOptions) {
      c => new RXTester(c, trials)
    }
  }
}
