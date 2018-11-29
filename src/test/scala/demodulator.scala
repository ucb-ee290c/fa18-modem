package modem

import dsptools.DspTester
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental._
import dsptools.numbers._
import breeze.math.{Complex}

       
class BPSKDemodTester[T <: chisel3.Data,U <: chisel3.Data ](c: BPSKDemodulator[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    

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
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new BPSKDemodulator(params)) {
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

class SermTester[T <: chisel3.Data, U <: chisel3.Data](c: Serilizerm[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    
   
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    for (j <- 0 until 2)  {

    poke(c.io.in.bits(0), 1)
    poke(c.io.in.bits(1), 0)
    poke(c.io.in.bits(2), 0)
    poke(c.io.in.bits(3), 1) 
     poke(c.io.in.bits(4), 1)
    poke(c.io.in.bits(5), 0)
    poke(c.io.in.bits(6), 1)
    poke(c.io.in.bits(7), 1) 
  
      // wait until input is accepted
   
          
    while (!peek(c.io.in.ready)) {
      
       step(1)
      }
        
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
           
      step(1)
    }
    expect(c.io.out.bits(0), 1) 
    expect(c.io.out.bits(1), 1)
    step(1)
    expect(c.io.out.bits(0), 1)
    expect(c.io.out.bits(1),0)
    step(1)
    expect(c.io.out.bits(0), 0) 
    expect(c.io.out.bits(1), 1)
    step(1)
    expect(c.io.out.bits(0), 1)
    expect(c.io.out.bits(1),0)
    

  }         
         
}
object SermTester {
  def apply(params: HardDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Serilizerm(params)) {
      c => new SermTester(c)
    }
  }
}

class Serm1Tester[T <: chisel3.Data, U <: chisel3.Data](c: Serilizerm1[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    
   
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    for (j <- 0 until 2)  {

    poke(c.io.in.bits.bits(0), 1)
    poke(c.io.in.bits.bits(1), 0)
    poke(c.io.in.bits.bits(2), 0)
    poke(c.io.in.bits.bits(3), 1) 
     poke(c.io.in.bits.bits(4), 1)
    poke(c.io.in.bits.bits(5), 0)
    poke(c.io.in.bits.bits(6), 1)
    poke(c.io.in.bits.bits(7), 1)
    
    poke(c.io.in.bits.pktStart,1)
  
      // wait until input is accepted
   
          
    while (!peek(c.io.in.ready)) {
      
       step(1)
      }
        
    while (!peek(c.io.out.valid) ) {
     

      peek(c.io.in.ready)
      peek(c.io.out.valid)
           
      step(1)
    }
    expect(c.io.out.bits.bits(0), 1) 
    expect(c.io.out.bits.bits(1), 1)
    step(1)
    expect(c.io.out.bits.bits(0), 1)
    expect(c.io.out.bits.bits(1),0)
    step(1)
    expect(c.io.out.bits.bits(0), 0) 
    expect(c.io.out.bits.bits(1), 1)
    step(1)
    expect(c.io.out.bits.bits(0), 1)
    expect(c.io.out.bits.bits(1),0)
    
    expect(c.io.out.bits.pktStart,1)
    

  }         
         
}
object Serm1Tester {
  def apply(params: SoftDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Serilizerm1(params)) {
      c => new Serm1Tester(c)
    }
  }
}
class Serms1Tester[T <: chisel3.Data, U <: chisel3.Data](c: Serilizerms1[T,U],tolLSBs: Int = 2) extends DspTester(c) {
    
   
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    for (j <- 0 until 2)  {

    poke(c.io.in.bits.iq(0), Complex(1,1))
    poke(c.io.in.bits.iq(1),Complex(0,0) )
    poke(c.io.in.bits.iq(2), Complex(0,0))
    poke(c.io.in.bits.iq(3), Complex(1,1)) 
     poke(c.io.in.bits.iq(4), Complex(1,1))
    poke(c.io.in.bits.iq(5), Complex(0,0))
    poke(c.io.in.bits.iq(6), Complex(1,1))
    poke(c.io.in.bits.iq(7), Complex(1,1))
    poke(c.io.in.bits.pktStart,1)
  
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

    expect(c.io.out.bits.iq(0), Complex(1,1)) 
    expect(c.io.out.bits.iq(1), Complex(1,1))
    step(1)
    expect(c.io.out.bits.iq(0), Complex(1,1))
    expect(c.io.out.bits.iq(1),Complex(0,0))
    step(1)
    expect(c.io.out.bits.iq(0), Complex(0,0)) 
    expect(c.io.out.bits.iq(1), Complex(1,1))
    step(1)
    expect(c.io.out.bits.iq(0), Complex(1,1))
    expect(c.io.out.bits.iq(1),Complex(0,0))
    expect(c.io.out.bits.pktStart,1)
    }

  }         
         
}
object Serms1Tester {
  def apply(params: SoftDemodParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Serilizerms1(params)) {
      c => new Serms1Tester(c)
    }
  }
}

class QPSKDemodSerTester[T <: chisel3.Data, U <: chisel3.Data](c: QPSKDemodulatorSer[T,U]) extends DspTester(c) {
    

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
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QPSKDemodulatorSer(params)) {
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


class QAM16DemodSerTester[T <: chisel3.Data, U <: chisel3.Data](c: QAM16DemodulatorSer[T,U]) extends DspTester(c) {
    

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
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new QAM16DemodulatorSer(params)) {
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

class DemodulatorTester[T <: chisel3.Data, U <: chisel3.Data](c: Demodulator[T,U],inp: Seq[Complex]) extends DspTester(c) with HasTesterUtil[Demodulator[T,U]] {
    

    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
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
object DemodulatorTester {
  def apply(params: HardDemodParams,inp: Seq[Complex]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new Demodulator(params)) {
      c => new DemodulatorTester(c,inp)
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


