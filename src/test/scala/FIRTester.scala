package modem

import dsptools.DspTester
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3.experimental._
import dsptools.numbers._
import breeze.math.{Complex}
import breeze.linalg.{DenseVector, randomDouble}




class MFirTester[T <: chisel3.Data](c: MFir[T],tolLSBs: Int = 4) extends DspTester(c) {
    val goldenModel = new ScalaFirFilter(Seq(Complex(1,0),Complex(1,0),Complex(1,0),Complex(1,0)))
    //poke(c.io.consts(0), Complex(1,0))
    //poke(c.io.consts(1), Complex(1,0))
   // poke(c.io.consts(2), Complex(1,0))
   // poke(c.io.consts(3), Complex(1,0))
    poke(c.io.in.valid, 1)
    for(i <- 0 until 64) {
      val input = Complex(randomDouble() * 2 - 1,randomDouble() * 2 - 1)

      val goldenModelResult = goldenModel.poke(input)

      poke(c.io.in.bits.iq, input)
      while (!peek(c.io.out.valid) ) {
        
      peek(c.io.out.valid)
            
      step(1)
    }
      if( i >3) {
        fixTolLSBs.withValue(tolLSBs) {
        expect(c.io.out.bits.iq, goldenModelResult, s"i $i, input $input, gm $goldenModelResult, ${peek(c.io.out.bits.iq)}")
        }
      }
      step(1)
    }
    
    
}
object MFirTester {
  def apply(params: FixedModFFTParams): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), () => new MFir(params)) {
      c => new MFirTester(c)
    }
  }
}

