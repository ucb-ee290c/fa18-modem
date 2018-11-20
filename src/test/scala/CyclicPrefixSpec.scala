package modem

import chisel3._
import chisel3.experimental.FixedPoint
import dsptools.numbers._
import dsptools.DspTester
import spire.implicits._
import breeze.math.Complex
import org.scalatest.{FlatSpec, Matchers}

case class CyclicPrefixTestVectors() {
  val remove1_64_16 = Seq.fill(16)(Complex.zero) ++ Seq.fill(64)(Complex.one)
  val add1_64_16 = Seq.fill(48)(Complex.one) ++ Seq.fill(15)(Complex.zero) ++ Seq.fill(1)(2*Complex.one)
  val remove3_32_8 = Seq.fill(8)(Complex.zero) ++ Seq.fill(32)(Complex.one) ++
                     Seq.fill(8)(Complex.zero) ++ Seq.fill(32)(Complex.one) ++
                     Seq.fill(8)(Complex.zero) ++ Seq.fill(32)(Complex.one)
  val add3_32_8 = Seq.fill(24)(Complex.one) ++ Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                  Seq.fill(24)(Complex.one) ++ Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                  Seq.fill(24)(Complex.one) ++ Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one)
  val rmOut_64_16 = Seq.fill(64)(Complex.one)
  val addOut_64_16 = Seq.fill(15)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                      Seq.fill(48)(Complex.one) ++ Seq.fill(15)(Complex.zero) ++ Seq.fill(1)(2*Complex.one)
  val rmOut_32_8 = Seq.fill(96)(Complex.one)
  val addOut_32_8 = Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                    Seq.fill(24)(Complex.one) ++ Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                    Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                    Seq.fill(24)(Complex.one) ++ Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                    Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one) ++
                    Seq.fill(24)(Complex.one) ++ Seq.fill(7)(Complex.zero) ++ Seq.fill(1)(2*Complex.one)
}

case class CPTrial(
  // input iq vectors
  iqin: Seq[Complex],
  iqout: Seq[Complex],
  add: Boolean
)

class CyclicPrefixSpec extends FlatSpec with Matchers {
  val vecs = CyclicPrefixTestVectors()
  behavior of "CyclicPrefix"

  val params64 = new CyclicPrefixParams[FixedPoint] {
    val protoIQ = DspComplex(FixedPoint(16.W, 13.BP))
    val prefixLength = 16
    val symbolLength = 64
  }
  val params32 = new CyclicPrefixParams[FixedPoint] {
    val protoIQ = DspComplex(FixedPoint(16.W, 13.BP))
    val prefixLength = 8
    val symbolLength = 32
  }

  it should "remove prefixes" in {
    val trials = Seq(CPTrial(vecs.remove1_64_16, vecs.rmOut_64_16, false))
    CyclicPrefixTester(params64, trials) should be (true)
  }

  it should "remove multiple prefixes" in {
    val trials = Seq(CPTrial(vecs.remove3_32_8, vecs.rmOut_32_8, false))
    CyclicPrefixTester(params32, trials) should be (true)
  }

  it should "add prefixes" in {
    val trials = Seq(CPTrial(vecs.add1_64_16, vecs.addOut_64_16, true))
    CyclicPrefixTester(params64, trials) should be (true)
  }

  it should "add multiple prefixes" in {
    val trials = Seq(CPTrial(vecs.add3_32_8, vecs.addOut_32_8, true))
    CyclicPrefixTester(params32, trials) should be (true)
  }

  it should "switch from adding to removing prefixes" in {
    val trials = Seq(CPTrial(vecs.add1_64_16, vecs.addOut_64_16, true),
                     CPTrial(vecs.remove1_64_16, vecs.rmOut_64_16, false))
    CyclicPrefixTester(params64, trials)
  }
}


/**
  * DspTester for CyclicPrefix
  *
  * Run each trial in @trials
  */
class CyclicPrefixTester[T <: chisel3.Data](c: CyclicPrefix[T], trials: Seq[CPTrial], tolLSBs: Int = 2) extends DspTester(c) {
  def expectIQ(c: CyclicPrefix[T], v: Complex, pktStart: Boolean, pktEnd: Boolean, n: Int): Boolean = {
    var valid = false
    if (peek(c.io.out.valid)) {
      expect(c.io.out.bits.iq(0), v, s"iq at $n")
      expect(c.io.out.bits.pktStart, pktStart)
      expect(c.io.out.bits.pktEnd, pktEnd)
      valid = true
    }
    valid
  }

  val maxCyclesWait = 100

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 0)
  step(1)

  for (trial <- trials) {
    var nout = 0
    poke(c.io.in.valid, 1)
    poke(c.io.add, trial.add)
    poke(c.io.in.bits.pktStart, true)
    for (i <- trial.iqin.indices) {
      poke(c.io.in.bits.iq(0), trial.iqin(i))
      poke(c.io.in.bits.pktStart, i == 0)
      poke(c.io.in.bits.pktEnd, i == trial.iqin.length - 1)
      // wait until input is accepted
      var cyclesWaiting = 0
      while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
        cyclesWaiting += 1
        if (cyclesWaiting >= maxCyclesWait) {
          expect(false, "waited for input too long")
        }
        if (expectIQ(c, trial.iqout(nout), nout == 0, nout == (trial.iqout.length - 1), nout)) {
          nout += 1
        }
        step(1)
      }
      if (expectIQ(c, trial.iqout(nout), nout == 0, nout == (trial.iqout.length - 1), nout)) {
        nout += 1
      }
      step(1)
    }
    // Wait for rest of iq to come out
    poke(c.io.in.valid, 0)
    var cyclesWaiting = 0
    while (nout < trial.iqout.length && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for input too long")
      }
      if (expectIQ(c, trial.iqout(nout), nout == 0, nout == trial.iqout.length - 1, nout)) {
        nout += 1
      }
      step(1)
    }
  }
}


/**
  * Convenience function for running tests
  */
object CyclicPrefixTester{
  def apply[T<:Data](params: CyclicPrefixParams[T], trials: Seq[CPTrial]): Boolean = {
    // chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new CyclicPrefix(params)) {
    dsptools.Driver.execute(() => new ConfigurationMemory(params), TestSetup.dspTesterOptions) {
      c => new CyclicPrefixTester(c, trials)
    }
  }
}
