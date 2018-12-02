package modem

import dsptools.DspTester
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental._
import dsptools.numbers._
import breeze.math.{Complex}
import breeze.linalg.{DenseVector, randomDouble}

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

//case class IQ(
  //in: Double,
  //qn: Double,
  //out: Double
//)

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




// modulator + FFT tester

class QPSKModFFTTester[T <: chisel3.Data,U <: chisel3.Data](c: QPSKModFFT[T,U],inp: Seq[Complex], out: Seq[Complex],pktStart: Boolean = true, pktEnd: Boolean = true, tolLSBs: Int = 5) extends DspTester(c) with HasTesterUtil[QPSKModFFT[T,U]] {
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
    
    //while (!peek(c.io.out.valid) ) {
     

     // peek(c.io.in.ready)
      //peek(c.io.out.valid)
            
      //step(1)
   // }
      //fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       //expect_seq(c.io.out.bits.iq, out)       
    //}
    out.zipWithIndex.foreach { case (value, index) =>
    wait_for_assert(c.io.out.valid, 500)
    expect(c.io.out.bits.pktStart, (pktStart && (index == 0)))
    expect(c.io.out.bits.pktEnd  , (pktEnd && (index == inp.length - 1)))
    fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits.iq, value) }
    step(1)
}


}

object FixedQPSKModFFTTester {
  def apply(params: FixedModFFTParams, inp: Seq[Complex], out: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKModFFT(params)) { c => new QPSKModFFTTester(c, inp, out) }
  }
}

class InterleaverTester[T <: chisel3.Data,U <: chisel3.Data](c: Interleaver[T,U]) extends DspTester(c) {
    
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


class Interleaverds2Tester[T <: chisel3.Data,U <: chisel3.Data](c: Interleaverds2[T,U]) extends DspTester(c) {
    
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
      
    for (i <- 0 until 4) {
       poke(c.io.in.bits(0), 0)
       poke(c.io.in.bits(1), 1)
       peek(c.io.in.ready)
       peek(c.io.out.valid)
       peek(c.io.cnt)
       peek(c.io.sat)
       if(i < 3){
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
       peek(c.io.cnt)
       peek(c.io.sat)
      
      
      
      step(1)
    }
    peek(c.io.in.ready)
      peek(c.io.out.valid)
       peek(c.io.cnt)
       peek(c.io.sat)
      
     expect(c.io.out.bits(0), 0)
     expect(c.io.out.bits(1), 1)
     expect(c.io.out.bits(2), 0)
     expect(c.io.out.bits(3), 1)
     expect(c.io.out.bits(4), 0)
     expect(c.io.out.bits(5), 1)
     expect(c.io.out.bits(6), 0)
     expect(c.io.out.bits(7), 1)
     
 
    
}
object Interleaverds2Tester {
  def apply(params:FixedModFFTParams ): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Interleaverds2(params)) {
      c => new Interleaverds2Tester(c)
    }
  }
}
class Interleaverds2bTester[T <: chisel3.Data,U <: chisel3.Data](c: Interleaverds2b[T,U]) extends DspTester(c) {
    
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
      
    
    for (i <- 0 until 2) {
       poke(c.io.in.bits.bits(0), 0)
       poke(c.io.in.bits.bits(1), 1)
       peek(c.io.in.ready)
       peek(c.io.out.valid)
       step(1)
      
    }

    for (i <- 2 until 4) {
       poke(c.io.in.bits.bits(0), 1)
       poke(c.io.in.bits.bits(1), 1)
       peek(c.io.in.ready)
       peek(c.io.out.valid)
       

       if(i < 3){
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
       peek(c.io.cnt)
       peek(c.io.sat)
      
      
      
      step(1)
    }
    peek(c.io.in.ready)
      peek(c.io.out.valid)
       peek(c.io.cnt)
       peek(c.io.sat)
      
     expect(c.io.out.bits.bits(0), 1)
     expect(c.io.out.bits.bits(1), 1)
     expect(c.io.out.bits.bits(2), 1)
     expect(c.io.out.bits.bits(3), 1)
     expect(c.io.out.bits.bits(4), 0)
     expect(c.io.out.bits.bits(5), 1)
     expect(c.io.out.bits.bits(6), 0)
     expect(c.io.out.bits.bits(7), 1)
     
 
    
}
object Interleaverds2bTester {
  def apply(params:FixedModFFTParams ): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Interleaverds2b(params)) {
      c => new Interleaverds2bTester(c)
    }
  }
}
class Interleaverds1Tester[T <: chisel3.Data,U <: chisel3.Data](c: Interleaverds1[T,U]) extends DspTester(c) {
    
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    val idata = Seq(1,0,1,0)
      // wait until input is accepted

      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      }
    peek(c.io.in.ready)
    peek(c.io.out.valid)
    
      
    poke(c.io.in.bits(3), 0)
    poke(c.io.in.bits(2), 1)
    poke(c.io.in.bits(1), 1)
    poke(c.io.in.bits(0), 0)

    peek(c.io.in.ready)
    peek(c.io.out.valid)      
      
      
      
    
      
      
    // wait until output is valid
    
    while (!peek(c.io.out.valid)) {
      

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      
      
      
      step(1)
    }
      
      
      expect(c.io.out.bits(3), 0)
      expect(c.io.out.bits(2), 1)
      expect(c.io.out.bits(1), 1)
      expect(c.io.out.bits(0), 0)


     
 
    
}
object Interleaverds1Tester {
  def apply(params:FixedModFFTParams ): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Interleaverds1(params)) {
      c => new Interleaverds1Tester(c)
    }
  }
}

class Interleaverds1bTester[T <: chisel3.Data,U <: chisel3.Data](c: Interleaverds1b[T,U]) extends DspTester(c) {
    
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)
    val idata = Seq(1,0,1,0)
      // wait until input is accepted

      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      }
    peek(c.io.in.ready)
    peek(c.io.out.valid)
    
      
    poke(c.io.in.bits.bits(3), 0)
    poke(c.io.in.bits.bits(2), 1)
    poke(c.io.in.bits.bits(1), 1)
    poke(c.io.in.bits.bits(0), 0)

    peek(c.io.in.ready)
    peek(c.io.out.valid)      
      
      
      
    
      
      
    // wait until output is valid
    
    while (!peek(c.io.out.valid)) {
      

      peek(c.io.in.ready)
      peek(c.io.out.valid)
      
      
      
      step(1)
    }
      
      
      expect(c.io.out.bits.bits(3), 0)
      expect(c.io.out.bits.bits(2), 1)
      expect(c.io.out.bits.bits(1), 1)
      expect(c.io.out.bits.bits(0), 0)
      expect(c.io.out.bits.pktStart, 1)


     
 
    
}
object Interleaverds1bTester {
  def apply(params:FixedModFFTParams ): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Interleaverds1b(params)) {
      c => new Interleaverds1bTester(c)
    }
  }
}



class QPSKCPModTester[T <: chisel3.Data,U <: chisel3.Data](c: QPSKCPModulator[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

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

class QPSKCPMod1Tester[T <: chisel3.Data,U <: chisel3.Data](c: QPSKCPModulator1[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
      // wait until input is accepted
   for (i <- 0 until 2) {
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      
      }
            
     
       poke(c.io.in.bits.bits(0), 0)
       poke(c.io.in.bits.bits(1), 1)
       

       peek(c.io.in.ready)
       peek(c.io.out.valid)
       step(1)
       poke(c.io.in.bits.bits(0), 1)
       poke(c.io.in.bits.bits(1), 1)
       

       peek(c.io.in.ready)
       peek(c.io.out.valid)
               
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       //for (i <- 0 until 48){
       //expect(c.io.out.bits.iq(3), Complex(-0.707,0.707)) 
       //expect(c.io.out.bits.iq(2), Complex(-0.707,0.707)) 
       expect(c.io.out.bits.iq(1), Complex(-0.707,0.707))
       expect(c.io.out.bits.iq(0), Complex(0.707,0.707))

       //expect(c.io.out.bits(i), Complex(-0.707,-0.707)) }
       
    }
   }      
}
object QPSKCPMod1Tester {
  def apply(params: FixedModFFTParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKCPModulator1(params)) {
      c => new QPSKCPMod1Tester(c)
    }
  }
}

class QPSKCPModulatorTester[T <: chisel3.Data,U <: chisel3.Data](c: Modulator[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,1)
    
      // wait until input is accepted
   for (i <- 0 until 2) {
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      
      }
            
       for (i <- 0 until 48){
         poke(c.io.in.bits.bits(i), 0)
       }
       

       peek(c.io.in.ready)
       peek(c.io.out.valid)
       step(1)
       for (i <- 0 until 48){
         poke(c.io.in.bits.bits(i), 0)
       }

       

       peek(c.io.in.ready)
       peek(c.io.out.valid)
               
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs) {
      // check every output where we have an expected value
       for (i <- 0 until 48){
       //expect(c.io.out.bits.iq(3), Complex(-0.707,0.707)) 
       //expect(c.io.out.bits.iq(2), Complex(-0.707,0.707)) 
       //expect(c.io.out.bits.iq(1), Complex(-0.707,0.707))
       //expect(c.io.out.bits.iq(0), Complex(0.707,0.707))

       expect(c.io.out.bits.iq(i), Complex(-0.707,-0.707)) }
       
    }
   }      
}
object QPSKCPModulatorTester {
  def apply(params: FixedModFFTParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Modulator(params)) {
      c => new QPSKCPModulatorTester(c)
    }
  }
}

class QAM16ModulatorTester[T <: chisel3.Data,U <: chisel3.Data](c: Modulator[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,2)
    
      // wait until input is accepted
   for (i <- 0 until 2) {
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      
      }
            
     
    
    for (i <- 0 until 4) {
       for (i <- 0 until 48){
           poke(c.io.in.bits.bits(i), 1)}
       
       peek(c.io.in.ready)
       peek(c.io.out.valid)
       

       if(i < 3){
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
       //expect(c.io.out.bits.iq(3), Complex(-0.707,0.707)) 
       //expect(c.io.out.bits.iq(2), Complex(-0.707,0.707)) 
       //expect(c.io.out.bits.iq(1), Complex(-0.316,-0.316))
       expect(c.io.out.bits.iq(i), Complex(0.316,0.316))

       //expect(c.io.out.bits(i), Complex(0.707,0.707)) 
       }
       
    }
   }      
}
object QAM16ModulatorTester {
  def apply(params: FixedModFFTParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Modulator(params)) {
      c => new QAM16ModulatorTester(c)
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

class BPSKCPModulatorTester[T <: chisel3.Data,U <: chisel3.Data](c: Modulator[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,0)
    
      // wait until input is accepted
   for (i <- 0 until 2) {
     for (i <- 0 until 48) {
        //poke(c.io.in.bits.bits(3), 0)
        //poke(c.io.in.bits.bits(2), 1)
        //poke(c.io.in.bits.bits(1), 1)
        //poke(c.io.in.bits.bits(0), 0)
       poke(c.io.in.bits.bits(i), 0) 
      
    }  
    peek(c.io.in.ready)
     peek(c.io.out.valid)
    
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
       //expect(c.io.out.bits.iq(3), Complex(-1,0)) 
       //expect(c.io.out.bits.iq(2), Complex(1,0)) 
       //expect(c.io.out.bits.iq(1), Complex(1,0))
       //expect(c.io.out.bits.iq(0), Complex(-1,0))

        expect(c.io.out.bits.iq(i), Complex(-1,0)) }
       
    }
   }      
}
object BPSKCPModulatorTester {
  def apply(params: FixedModFFTParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Modulator(params)) {
      c => new BPSKCPModulatorTester(c)
    }
  }
}



