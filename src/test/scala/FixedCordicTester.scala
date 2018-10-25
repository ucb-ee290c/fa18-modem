package cordic

import dsptools.DspTester
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental._
import dsptools.numbers._

/**
 * Case class holding information needed to run an individual test
 */
case class XYZ(
  // input x, y and z
  xin: Double,
  yin: Double,
  zin: Double,
  // mode
  vectoring: Boolean,
  // optional outputs
  // if None, then don't check the result
  // if Some(...), check that the result matches
  xout: Option[Double] = None,
  yout: Option[Double] = None,
  zout: Option[Double] = None
)

case class IQ(
  in: Double,
  qn: Double,
  out: Double
)

case class IQG(
  in: Double,
  iout: Double,
  qout: Double

)
/**
 * DspTester for FixedIterativeCordic
 *
 * Run each trial in @trials
 */

//class FixedCordicTester[T <: chisel3.Data](c: FixedIterativeCordic[T], trials: Seq[XYZ]) extends DspTester(c) {
class CordicTester[T <: chisel3.Data](c: IterativeCordic[T], trials: Seq[XYZ], tolLSBs: Int = 2) extends DspTester(c) {

  val maxCyclesWait = 50

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  for (trial <- trials) {
    poke(c.io.in.bits.x, trial.xin)
    poke(c.io.in.bits.y, trial.yin)
    poke(c.io.in.bits.z, trial.zin)
    poke(c.io.vectoring, trial.vectoring)
    

    // wait until input is accepted
    var cyclesWaiting = 0
    while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      peek(c.io.in.bits.x)
      peek(c.io.in.bits.y)
      peek(c.io.in.bits.z)
      peek(c.io.out.bits.x)
      peek(c.io.out.bits.y)
      peek(c.io.out.bits.z)
      peek(c.io.cnt)
      //peek(c.io.out1)
      //expect(cyclesWaiting < maxCyclesWait, "waited for input too long")
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for input too long")
      }

      step(1)
    }
    // wait until output is valid
    cyclesWaiting = 0
    while (!peek(c.io.out.valid) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      peek(c.io.in.bits.x)
      peek(c.io.in.bits.y)
      peek(c.io.in.bits.z)
      peek(c.io.out.bits.x)
      peek(c.io.out.bits.y)
      peek(c.io.out.bits.z)
      peek(c.io.cnt)
      //peek(c.io.out1)
      //expect(cyclesWaiting < maxCyclesWait, "waited for output too long")
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for output too long")
      }
      step(1)
    }
    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
      trial.xout.foreach { x => expect(c.io.out.bits.x, x) }
      trial.yout.foreach { y => expect(c.io.out.bits.y, y) }
      trial.zout.foreach { z => expect(c.io.out.bits.z, z) }
    }
  }
}

class ShiftRegisterTester(c: ShiftRegister) extends DspTester(c) {
    //println(s"Testing ShiftRegister of type ${c.io.in} and depth ${c.io.out.length}")
    
    for(i <- 0 until 5) {
    poke(c.io.sin.valid, 1)
    
    poke(c.io.pout(i).ready, 1)
  }
    for (i <- 0 until 15) {
        poke(c.io.sin.bits, i)
        peek(c.io.pout(0).bits)
        peek(c.io.pout(0).valid)
        //println(s"$i: ${peek(c.io.out)}")
        step(1)
    }
}


class DemapperTester[T <: chisel3.Data](c: Demapper[T],trials: Seq[IQ]) extends DspTester(c) {
    //val idata = Seq(-1, -0.5, 0.25, 0.5, 1)
    //val qdata = Seq(1, 0.5, 0.25, -0.25,-1)
    for (trial <- trials) {
      poke(c.io.ini, trial.in)
      poke(c.io.inq, trial.qn)
      expect(c.io.out, trial.out)
    }
    
}

object DemapperTester {
  def apply(params: FixedDemapParams, trials: Seq[IQ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demapper(params)) {
      c => new DemapperTester(c, trials)
    }
  }
}

class InterleavTester[T <: chisel3.Data](c: Interleav[T]) extends DspTester(c) {
    
    for (i <- 0 until 48) {
      poke(c.io.in(i), 0) 
      
    }
    expect(c.io.out(0), 0)
}

object InterleavTester {
  def apply(params:UintInterleavParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Interleav(params)) {
      c => new InterleavTester(c)
    }
  }
}

class DeinterleavTester[T <: chisel3.Data](c: Deinterleav[T]) extends DspTester(c) {
    
    for (i <- 0 until 48) {
      poke(c.io.in(i), 0) 
      
    }
    expect(c.io.out(0), 0)
}

object DeinterleavTester {
  def apply(params:UintInterleavParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Deinterleav(params)) {
      c => new DeinterleavTester(c)
    }
  }
}

class MapperTester[T <: chisel3.Data](c: Mapper[T],trials: Seq[IQG],tolLSBs: Int = 2) extends DspTester(c) {
    //val idata = Seq(-1, -0.5, 0.25, 0.5, 1)
    //val qdata = Seq(1, 0.5, 0.25, -0.25,-1)
    for (trial <- trials) {
      poke(c.io.in, trial.in)
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       expect(c.io.out_i, trial.iout) 
       expect(c.io.out_q, trial.qout)
    }
      
    }
    
}

object MapperTester {
  def apply(params: FixedMapParams, trials: Seq[IQG]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Mapper(params)) {
      c => new MapperTester(c, trials)
    }
  }
}

class MFirTester[T <: chisel3.Data](c: MFir[T]) extends DspTester(c) {
    val goldenModel = new ScalaFirFilter(Seq(1, 1, 1, 1))
    poke(c.io.consts(0), 1)
    poke(c.io.consts(1), 1)
    poke(c.io.consts(2), 1)
    poke(c.io.consts(3), 1)
    poke(c.io.valid, 1)
    for(i <- 0 until 100) {
      val input = scala.util.Random.nextInt(8)

      val goldenModelResult = goldenModel.poke(input)

      poke(c.io.in, input)

      expect(c.io.out, goldenModelResult, s"i $i, input $input, gm $goldenModelResult, ${peek(c.io.out)}")

      step(1)
    }
    
    
}

object MFirTester {
  def apply(params: UIntFirParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new MFir(params)) {
      c => new MFirTester(c)
    }
  }
}



object ShiftRegisterTester {
  def apply(params: Uintp2sParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new ShiftRegister(params)) {
      c => new ShiftRegisterTester(c)
    }
  }
}

/**
 * Convenience function for running tests
 */
object FixedCordicTester {
  def apply(params: FixedCordicParams, trials: Seq[XYZ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new IterativeCordic(params)) {
      c => new CordicTester(c, trials)
    }
  }
}

object RealCordicTester {
  def apply(params: CordicParams[dsptools.numbers.DspReal], trials: Seq[XYZ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "verilator", "-fiwv"), () => new IterativeCordic(params)) {
      c => new CordicTester(c, trials)
    }
  }
}
