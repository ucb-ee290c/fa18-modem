package modem

import dsptools.DspTester
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental._
import dsptools.numbers._
import breeze.math.{Complex}

       
class BPSKDemodTester[T <: chisel3.Data,U <: chisel3.Data ](c: BPSKDemodulator1[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)
    for (j <- 0 until 2)  {
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(-1,0))
      
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
       expect(c.io.out.bits.bits(i), -1) }
       
    }
   }      
}
object BPSKDemodTester {
  def apply(params: HardDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BPSKDemodulator1(params)) {
      c => new BPSKDemodTester(c)
    }
  }
}

class BPSKDemodTester1s[T <: chisel3.Data, U <: chisel3.Data](c: BPSKDemodulator1s[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)
    for (j <- 0 until 2)  {
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(0.5,0))
      
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
       expect(c.io.out.bits.iq(i), Complex(0.5,0) ) }
       
    }
   }      
}
object BPSKDemodTester1s {
  def apply(params: SoftDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BPSKDemodulator1s(params)) {
      c => new BPSKDemodTester1s(c)
    }
  }
}


class BPSKDemodTester1[T <: chisel3.Data, U <: chisel3.Data](c: BPSKDemodulator1[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)
    for (j <- 0 until 2)  {
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(-1,0))
      
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
       expect(c.io.out.bits.bits(i), -1) }
       
    }
   }      
}
object BPSKDemodTester1 {
  def apply(params: HardDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BPSKDemodulator1(params)) {
      c => new BPSKDemodTester1(c)
    }
  }
}


class QPSKDemodSerTester[T <: chisel3.Data, U <: chisel3.Data](c: QPSKDemodulatorSer1[T,U]) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 24) {
      // poke(c.io.in.bits.iq(i), Complex(0.8,0.8))}
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(-0.2,-0.2))}

     
    
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
         expect(c.io.out.bits.bits(i), -1) 
        }
        step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), -1) 
         }
      
       
    
        
}
object QPSKDemodSerTester {
  def apply(params: SoftDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKDemodulatorSer1(params)) {
      c => new QPSKDemodSerTester(c)
    }
  }
}

class QPSKDemodSer1Tester[T <: chisel3.Data, U <: chisel3.Data](c: QPSKDemodulatorSer1[T,U]) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 24) {
      // poke(c.io.in.bits.iq(i), Complex(0.8,0.8))}
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(-0.2,-0.2))}

     
    
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
         expect(c.io.out.bits.bits(i), -1) 
        }
        step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), -1) 
         }
      
       
    
        
}
object QPSKDemodSer1Tester {
  def apply(params: HardDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKDemodulatorSer1(params)) {
      c => new QPSKDemodSer1Tester(c)
    }
  }
}

class QPSKDemodSer1sTester[T <: chisel3.Data, U <: chisel3.Data](c: QPSKDemodulatorSer1s[T, U],tolLSBs: Int = 2) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 24) {
      // poke(c.io.in.bits.iq(i), Complex(0.8,0.8))}
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(-0.2,-0.2))}

     
    
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

       for (i <- 0 until 48) {
         expect(c.io.out.bits.iq(i), Complex(-0.1414,0))
        }
        step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.iq(i), Complex(-0.1414,0)) 
         }
      
     }  
    
        
}
object QPSKDemodSer1sTester {
  def apply(params: SoftDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKDemodulatorSer1s(params)) {
      c => new QPSKDemodSer1sTester(c)
    }
  }
}


class QAM16DemodSerTester[T <: chisel3.Data, U <: chisel3.Data](c: QAM16DemodulatorSer1[T,U]) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     
    
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
         expect(c.io.out.bits.bits(i), 1) 
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1) 
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1)} 
         step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1) 
         }

      
       
    
        
}
object QAM16DemodSerTester {
  def apply(params: HardDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QAM16DemodulatorSer1(params)) {
      c => new QAM16DemodSerTester(c)
    }
  }
}

class QAM16DemodSer1Tester[T <: chisel3.Data, U <: chisel3.Data](c: QAM16DemodulatorSer1[T,U]) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     
    
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
         expect(c.io.out.bits.bits(i), 1) 
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1) 
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1)} 
         step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1) 
         }
        expect(c.io.out.bits.pktStart,1)

      
       
    
        
}
object QAM16DemodSer1Tester {
  def apply(params: HardDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QAM16DemodulatorSer1(params)) {
      c => new QAM16DemodSer1Tester(c)
    }
  }
}

class DemodulatorbpskTester[T <: chisel3.Data, U <: chisel3.Data](c: Demodulator[T,U],inp: Seq[Complex]) extends DspTester(c) with HasTesterUtil[Demodulator[T,U]] {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,0)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 64) {
       //poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     poke_seq(c.io.in.bits.iq, inp)
     
    
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
                 
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      
       for (i <- 6 until 10) {
         expect(c.io.out.bits.bits(i), -1) 
         }
        for (i <- 10 until 22) {
         expect(c.io.out.bits.bits(i), 1) 
         }
       //step(1)
       //for (i <- 0 until 48) {
        // expect(c.io.out.bits.bits(i), 1) 
        // }
       //step(1)
      // for (i <- 0 until 48) {
        // expect(c.io.out.bits.bits(i), 1)} 
         step(1)
       //for (i <- 0 until 48) {
         //expect(c.io.out.bits.bits(i), 1) 
         //}
        expect(c.io.out.bits.pktStart,1)

      
       
    
        
}
object DemodulatorbpskTester {
  def apply(params: HardDemodParams,inp: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demodulator(params)) {
      c => new DemodulatorbpskTester(c,inp)
    }
  }
}

class DemodulatorqamTester[T <: chisel3.Data, U <: chisel3.Data](c: Demodulator[T,U],inpqam: Seq[Complex]) extends DspTester(c) with HasTesterUtil[Demodulator[T,U]] {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,2)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 64) {
       //poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     poke_seq(c.io.in.bits.iq, inpqam)
     
    
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
                 
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
    //fixTolLSBs.withValue(tolLSBs){
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1)
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1) 
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1)} 
         step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 1) 
         }
        
     // }
   
    expect(c.io.out.bits.pktStart,1)

      
       
    
        
}
object DemodulatorqamTester {
  def apply(params: HardDemodParams,inpqam: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demodulator(params)) {
      c => new DemodulatorqamTester(c,inpqam)
    }
  }
}

class DemodulatorqamsTester[T <: chisel3.Data, U <: chisel3.Data](c: Demodulator[T,U],inpqam: Seq[Complex],tolLSBs: Int = 5) extends DspTester(c) with HasTesterUtil[Demodulator[T,U]] {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,2)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 64) {
       //poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     poke_seq(c.io.in.bits.iq, inpqam)
     
    
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
                 
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
    fixTolLSBs.withValue(tolLSBs){
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 0.099856)
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 0.099856) 
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 0.099856)} 
         step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), 0.099856) 
         }
        
      }
   
    expect(c.io.out.bits.pktStart,1)

      
       
    
        
}
object DemodulatorqamsTester {
  def apply(params: SoftDemodParams,inpqam: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demodulator(params)) {
      c => new DemodulatorqamsTester(c,inpqam)
    }
  }
}


class DemodulatorbpsksTester[T <: chisel3.Data, U <: chisel3.Data](c: Demodulator[T,U],inp: Seq[Complex],tolLSBs: Int = 2) extends DspTester(c) with HasTesterUtil[Demodulator[T,U]] {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,0)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 64) {
       //poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     poke_seq(c.io.in.bits.iq, inp)
     
    
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
       for (i <- 6 until 10) {
         expect(c.io.out.bits.bits(i), -0.5) 
         }
        for (i <- 10 until 22) {
         expect(c.io.out.bits.bits(i), 0.5) 
         }
      }
       //step(1)
       //for (i <- 0 until 48) {
        // expect(c.io.out.bits.bits(i), 1) 
        // }
       //step(1)
      // for (i <- 0 until 48) {
        // expect(c.io.out.bits.bits(i), 1)} 
         step(1)
       //for (i <- 0 until 48) {
         //expect(c.io.out.bits.bits(i), 1) 
         //}
        expect(c.io.out.bits.pktStart,1)

      
       
    
        
}
object DemodulatorbpsksTester {
  def apply(params: SoftDemodParams,inp: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demodulator(params)) {
      c => new DemodulatorbpsksTester(c,inp)
    }
  }
}

class DemodulatorqpskTester[T <: chisel3.Data, U <: chisel3.Data](c: Demodulator[T,U],inpqpsk: Seq[Complex]) extends DspTester(c) with HasTesterUtil[Demodulator[T,U]] {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 64) {
       //poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     poke_seq(c.io.in.bits.iq, inpqpsk)
     
    
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
                 
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
    //fixTolLSBs.withValue(tolLSBs){
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), -1)
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), -1) 
         }
               
     // }
   
    expect(c.io.out.bits.pktStart,1)

      
       
    
        
}
object DemodulatorqpskTester {
  def apply(params: HardDemodParams,inpqpsk: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demodulator(params)) {
      c => new DemodulatorqpskTester(c,inpqpsk)
    }
  }
}

class DemodulatorqpsksTester[T <: chisel3.Data, U <: chisel3.Data](c: Demodulator[T,U],inpqpsk: Seq[Complex],tolLSBs: Int = 4) extends DspTester(c) with HasTesterUtil[Demodulator[T,U]] {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.mod_ctrl,1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    //for (i <- 0 until 64) {
       //poke(c.io.in.bits.iq(i), Complex(0.5,0.5))}
     poke_seq(c.io.in.bits.iq, inpqpsk)
     
    
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
                 
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
    fixTolLSBs.withValue(tolLSBs){
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), -0.1414)
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.bits(i), -0.1414) 
         }
               
      }
   
    expect(c.io.out.bits.pktStart,1)

      
       
    
        
}
object DemodulatorqpsksTester {
  def apply(params: SoftDemodParams,inpqpsk: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demodulator(params)) {
      c => new DemodulatorqpsksTester(c,inpqpsk)
    }
  }
}
class QAM16DemodSer1sTester[T <: chisel3.Data, U <: chisel3.Data](c: QAM16DemodulatorSer1s[T,U],tolLSBs: Int = 8) extends DspTester(c) {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits.pktStart, 1)
    poke(c.io.in.bits.pktEnd, 1)

    
    for (i <- 0 until 64) {
       poke(c.io.in.bits.iq(i), Complex(0.316,0.316))}
     
    
      // wait until input is accepted
   
      
    
    while (!peek(c.io.in.ready)) {
      
       step(1)
      

      }
                 
    
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
            
      step(1)
    }
      fixTolLSBs.withValue(tolLSBs){
       for (i <- 0 until 48) {
         expect(c.io.out.bits.iq(i), Complex(0.099856,0))
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.iq(i), Complex(0.099856,0)) 
         }
       step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.iq(i), Complex(0.099856,0))} 
         step(1)
       for (i <- 0 until 48) {
         expect(c.io.out.bits.iq(i), Complex(0.099856,0)) 
         }
        expect(c.io.out.bits.pktStart,1)
      }
      
       
    
        
}
object QAM16DemodSer1sTester {
  def apply(params: SoftDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QAM16DemodulatorSer1s(params)) {
      c => new QAM16DemodSer1sTester(c)
    }
  }
}


