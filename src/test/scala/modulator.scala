package modem

import dsptools.DspTester
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental._
import dsptools.numbers._
import breeze.math.{Complex}

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

case class IQGM(
  in1: Double,
  in0: Double,
  iout: Double,
  qout: Double

)

/**
 * DspTester for FixedIterativeCordic
 *
 * Run each trial in @trials
 */

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
    step(2)
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
      poke(c.io.in(i), 1) 
      
    }
    for (i <- 0 until 48) {
      poke(c.io.in(i+48), 0) 
      
    }
    step(2)
    expect(c.io.out(0), 1)
}

object DeinterleavTester {
  def apply(params:UintInterleavParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Deinterleav(params)) {
      c => new DeinterleavTester(c)
    }
  }
}

class ModulatorTester[T <: chisel3.Data](c: Modulator[T],trials: Seq[IQG],tolLSBs: Int = 2) extends DspTester(c) {
    //val idata = Seq(-1, -0.5, 0.25, 0.5, 1)
    //val qdata = Seq(1, 0.5, 0.25, -0.25,-1)
    val maxCyclesWait = 5

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    for (trial <- trials) {
      
      poke(c.io.in.bits, trial.in)
       
      // wait until input is accepted
    var cyclesWaiting = 0
    while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      
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
      
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for output too long")
      }
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       expect(c.io.out.bits.opiq, Complex(trial.iout,trial.qout)) 
       //expect(c.io.out.bits.opiq.imag, trial.qout)
    }
     
    }
    
}

object ModulatorTester {
  def apply(params1: FixedMapParams, params: FixedModParams,trials: Seq[IQG]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Modulator(params1,params)) {
      c => new ModulatorTester(c, trials)
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


class ModulatormTester[T <: chisel3.Data](c: Modulator[T],trials: Seq[IQGM],tolLSBs: Int = 2) extends DspTester(c) {
    //val idata = Seq(-1, -0.5, 0.25, 0.5, 1)
    //val qdata = Seq(1, 0.5, 0.25, -0.25,-1)
    val maxCyclesWait = 10

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    for (trial <- trials) {
      // wait until input is accepted

      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
    peek(c.io.in.ready)
      peek(c.io.out.valid)
      peek(c.io.par)
      peek(c.io.sta)
      peek(c.io.cnt)
      
      poke(c.io.in.bits, trial.in1)
      peek(c.io.in.ready)
      peek(c.io.out.valid)
      peek(c.io.par)
      peek(c.io.sta)
      peek(c.io.cnt)
      step(1)
      poke(c.io.in.bits, trial.in0)
      peek(c.io.in.ready)
      peek(c.io.out.valid)
      peek(c.io.par)
   
      peek(c.io.sta)
      peek(c.io.cnt)  

      
    
      
      
    // wait until output is valid
    var cyclesWaiting = 0
    while (!peek(c.io.out.valid) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      peek(c.io.par)
   
      peek(c.io.sta)
      peek(c.io.cnt)
      
      if (cyclesWaiting >= maxCyclesWait) {
        expect(false, "waited for output too long")
      }
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       expect(c.io.out.bits.opiq, Complex(trial.iout,trial.qout)) 
       //expect(c.io.out.bits.opiq, trial.qout)
    }
     peek(c.io.par)
     
     peek(c.io.sta)
     peek(c.io.cnt) 
     
    }
    
}

object ModulatormTester {
  def apply(params1: FixedMapParams,params: FixedModParams, trials: Seq[IQGM]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Modulator(params1,params)) {
      c => new ModulatormTester(c, trials)
    }
  }
}

// modulator + FFT tester

class QPSKModFFTTester[T <: chisel3.Data](c: QPSKModFFT[T],inp: Seq[Complex], out: Seq[Complex],pktStart: Boolean = true, pktEnd: Boolean = true, tolLSBs: Int = 5) extends DspTester(c) with HasTesterUtil[QPSKModFFT[T]] {
  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  //poke_seq(c.io.in.bits.fec, inp)
  poke(c.io.in.bits.pktStart, pktStart)
  poke(c.io.in.bits.pktEnd  , pktEnd)
  while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     for (i <- 0 until 96) {
       poke(c.io.in.bits.fec, 1)
       peek(c.io.in.ready)
       peek(c.io.out.valid)
      
       if(i < 95){
        step(1)} 
      
    }      
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       expect_seq(c.io.out.bits.iq, out)
       
       
    }
  


}

object FixedQPSKModFFTTester {
  def apply(params: FixedModFFTParams, inp: Seq[Complex], out: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKModFFT(params)) { c => new QPSKModFFTTester(c, inp, out) }
  }
}

class InterleaverTester[T <: chisel3.Data](c: Interleaver[T]) extends DspTester(c) {
    
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    val idata = Seq(1,0,1,0)
      // wait until input is accepted

      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      }
    peek(c.io.in.ready)
    peek(c.io.out.valid)
    peek(c.io.cnt)  
      
    for (i <- 0 until 48) {
       poke(c.io.in.bits, 0)
       peek(c.io.in.ready)
       peek(c.io.out.valid)
       peek(c.io.cnt)
       if(i < 47){
        step(1)} 
      
    }
    //poke(c.io.in.bits, 1)
    //step(1)
    //poke(c.io.in.bits, 1)
    //step(1)
    //poke(c.io.in.bits, 1)
    //step(1)
    //poke(c.io.in.bits, 0)
    peek(c.io.cnt)
    peek(c.io.in.ready)
    peek(c.io.out.valid)
      
      
      
      
    
      
      
    // wait until output is valid
    
    while (!peek(c.io.out.valid)) {
      

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      
      
      
      step(1)
    }
      
      for (i <- 0 until 48){
       expect(c.io.out.bits(i), 0)}
     
 
    
}
object InterleaverTester {
  def apply(params:FixedModFFTParams ): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Interleaver(params)) {
      c => new InterleaverTester(c)
    }
  }
}

class DeinterleaverTester[T <: chisel3.Data](c: Deinterleaver[T]) extends DspTester(c) {
    
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)

      
    for (i <- 0 until 48) {
       poke(c.io.in.bits(i), 0)
       
    }
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      }
    peek(c.io.in.ready)
    peek(c.io.out.valid)
    //peek(c.io.cnt)  
      
    
    //poke(c.io.in.bits, 1)
    //step(1)
    //poke(c.io.in.bits, 1)
    //step(1)
    //poke(c.io.in.bits, 1)
    //step(1)
    //poke(c.io.in.bits, 0)
    //peek(c.io.cnt)
    peek(c.io.in.ready)
    peek(c.io.out.valid)
      
      
      
      
    
      
      
    // wait until output is valid
    
    while (!peek(c.io.out.valid)) {
      

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      
      
      
      step(1)
    }
      
      for (i <- 0 until 48){
       expect(c.io.out.bits(i), 0)
     }
     
 
    
}
object DeinterleaverTester {
  def apply(params:FixedBPSKModParams ): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Deinterleaver(params)) {
      c => new DeinterleaverTester(c)
    }
  }
}

class BPSKCPModTester[T <: chisel3.Data](c: BPSKCPModulator[T],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
      // wait until input is accepted
   for (i <- 0 until 2) {
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     for (i <- 0 until 48) {
       poke(c.io.in.bits, 0)
       peek(c.io.in.ready)
       peek(c.io.out.valid)
      
       if(i < 47){
        step(1)} 
      
    }  
       
       
    
      

      
    
      
      
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       for (i <- 0 until 48){
       //expect(c.io.out.bits(3), Complex(1,0)) 
       //expect(c.io.out.bits(2), Complex(-1,0)) 
       //expect(c.io.out.bits(1), Complex(1,0)) 
       expect(c.io.out.bits(i), Complex(-1,0)) }
       
    }
   }      
}
object BPSKCPModTester {
  def apply(params: FixedModFFTParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BPSKCPModulator(params)) {
      c => new BPSKCPModTester(c)
    }
  }
}

class BPSKDemodTester[T <: chisel3.Data](c: BPSKDemodulator[T],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    for (j <- 0 until 2)  {
    for (i <- 0 until 48) {
       poke(c.io.in.bits(i), Complex(-1,0))
      
    }  
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     
       
            
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       for (i <- 0 until 48){
       //expect(c.io.out.bits(3), Complex(1,0)) 
       //expect(c.io.out.bits(2), Complex(-1,0)) 
       //expect(c.io.out.bits(1), Complex(1,0)) 
       expect(c.io.out.bits(i), 0) }
       
    }
   }      
}
object BPSKDemodTester {
  def apply(params: FixedBPSKModParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BPSKDemodulator(params)) {
      c => new BPSKDemodTester(c)
    }
  }
}
class BPSKSerDemodTester[T <: chisel3.Data](c: BPSKSerDemodulator[T]) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
    for (i <- 0 until 48) {
       poke(c.io.in.bits(i), Complex(1,0))}
     
    
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     
       
            
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      
       for (i <- 0 until 48) {
         expect(c.io.out.bits, 1) 
         step(1)}
       
    
        
}
object BPSKSerDemodTester {
  def apply(params: FixedBPSKModParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BPSKSerDemodulator(params)) {
      c => new BPSKSerDemodTester(c)
    }
  }
}
class QPSKDemodTester[T <: chisel3.Data](c: QPSKDemodulator[T],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
    for (i <- 0 until 48) {
       poke(c.io.in.bits(i), Complex(0.8,0.8))
      
    }  
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     
       
       
      
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       for (i <- 0 until 96){
       //expect(c.io.out.bits(3), Complex(1,0)) 
       //expect(c.io.out.bits(2), Complex(-1,0)) 
       //expect(c.io.out.bits(1), Complex(1,0)) 
       expect(c.io.out.bits(i), 1) }
       
    }
         
}
object QPSKDemodTester {
  def apply(params: FixedBPSKModParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKDemodulator(params)) {
      c => new QPSKDemodTester(c)
    }
  }
}

class QAM16DemodTester[T <: chisel3.Data](c: QAM16Demodulator[T],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
    for (i <- 0 until 48) {
       poke(c.io.in.bits(i), Complex(-0.8,-0.8))
      
    }  
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     
       
       
      
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       for (i <- 0 until 48){
       //expect(c.io.out.bits(3), Complex(1,0)) 
       //expect(c.io.out.bits(2), Complex(-1,0)) 
       //expect(c.io.out.bits(1), Complex(1,0)) 
       expect(c.io.out.bits(4*i), 0) 
       expect(c.io.out.bits(4*i +1), 0)
       expect(c.io.out.bits(4*i +2), 0) 
       expect(c.io.out.bits(4*i +3), 0)
       }
       
    }
         
}
object QAM16DemodTester {
  def apply(params: FixedBPSKModParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QAM16Demodulator(params)) {
      c => new QAM16DemodTester(c)
    }
  }
}


class QPSKCPModTester[T <: chisel3.Data](c: QPSKCPModulator[T],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
      // wait until input is accepted
   for (i <- 0 until 2) {
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     for (i <- 0 until 48) {
       poke(c.io.in.bits, 0)
       peek(c.io.in.ready)
       peek(c.io.out.valid)
      
       if(i < 47){
        step(1)} 
      
    }  
       
       
    
      

      
    
      
      
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       for (i <- 0 until 48){
       //expect(c.io.out.bits(3), Complex(1,0)) 
       //expect(c.io.out.bits(2), Complex(-1,0)) 
       //expect(c.io.out.bits(1), Complex(1,0)) 
       expect(c.io.out.bits(i), Complex(-0.707,-0.707)) }
       
    }
   }      
}
object QPSKCPModTester {
  def apply(params: FixedModFFTParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKCPModulator(params)) {
      c => new QPSKCPModTester(c)
    }
  }
}

class SerTester[T <: chisel3.Data](c: Serilizer[T],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
    
    poke(c.io.in.bits(0), 1)
    poke(c.io.in.bits(1), 0)
    poke(c.io.in.bits(2), 1)
    poke(c.io.in.bits(3), 1)
  
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
     
       
       
      
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
           
      step(1)
    }
    expect(c.io.out.bits, 1) 
    step(1)
    expect(c.io.out.bits, 0) 
    step(1)
    expect(c.io.out.bits, 1) 
    step(1)
    expect(c.io.out.bits, 1) 
    step(1)  
    
       
    
         
}
object SerTester {
  def apply(params: FixedBPSKModParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Serilizer(params)) {
      c => new SerTester(c)
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
