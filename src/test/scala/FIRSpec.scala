package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import chisel3.util.log2Ceil
import breeze.linalg.{DenseVector, randomDouble}






class FIRSpec extends FlatSpec with Matchers {
  behavior of "FIR"
 behavior of "Fir"
  val paramsfir =   FixedModFFTParams(
    dataWidth = 20,
    bitsWidth = 48,
    Ncbps = 8,
    Nbpsc = 4,
    twiddleWidth = 10,
    numPoints = 2,
    binPoints = 14,
    fftType = "direct"
  )
 
  MFirTester(paramsfir)





}
