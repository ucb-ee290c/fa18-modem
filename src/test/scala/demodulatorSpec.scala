package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{DenseVector, randomDouble}
import chisel3.util.log2Ceil





class demodulatorSpec extends FlatSpec with Matchers {
  behavior of "BPSKDemodulator"
  val paramsbpskdemod = FixedDemodParams(
     datawidth = 16,
     width = 64,
     bitsWidth = 48,
     Ncbps = 48,
     Nbpsc = 1
  )
  //BPSKDemodTester(paramsbpskdemod)
  //BPSKDemodTester1(paramsbpskdemod)
  BPSKDemodTester1s(paramsbpskdemod)

  behavior of "Serilizer"
  val paramsser = FixedDemodParams(
     datawidth = 16,
     width = 64,
     bitsWidth = 48,
     Ncbps = 8,
     Nbpsc = 4
  )
  //SermTester(paramsser)
  behavior of "Serilizer1"
  val paramsser1 = FixedDemodParams(
     datawidth = 16,
     width = 64,
     bitsWidth = 48,
     Ncbps = 8,
     Nbpsc = 4
  )
  //Serm1Tester(paramsser1)
  behavior of "Serilizerms1"
  val paramssers1 = FixedDemodParams(
     datawidth = 16,
     width = 64,
     bitsWidth = 48,
     Ncbps = 8,
     Nbpsc = 4
  )
  //Serms1Tester(paramssers1)

   behavior of "QPSKDEMOD"
  val paramsqpskdemod = FixedDemodParams(
     datawidth = 16,
     width = 64,
     bitsWidth = 48,
     Ncbps = 96,
     Nbpsc = 2
  )
  //QPSKDemodSerTester(paramsqpskdemod)
  //QPSKDemodSer1sTester(paramsqpskdemod)
   behavior of "QAM16DEMOD"
  val paramsqam16demod = FixedDemodParams(
     datawidth = 16,
     width = 64,
     bitsWidth = 48,
     Ncbps = 192,
     Nbpsc = 4
  )
  //QAM16DemodSerTester(paramsqam16demod)
  //QAM16DemodSer1Tester(paramsqam16demod)
  //QAM16DemodSer1sTester(paramsqam16demod)







}



