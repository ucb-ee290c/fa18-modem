package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{DenseVector, randomDouble}
import chisel3.util.log2Ceil
import breeze.numerics.{atan, pow, sqrt, abs,floor}
import scala.collection.mutable.ListBuffer






class DemodulatorSpec extends FlatSpec with Matchers {
  behavior of "BPSKDemodulator"
  val paramsbpskdemodh = HardDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod = 1,
     bitsWidth = 48,
     Ncbps = 48,
     Nbpsc = 1
  )
  val paramsbpskdemods = SoftDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod = 0,
     bitsWidth = 48,
     Ncbps = 48,
     Nbpsc = 1
  )
 val inp0 = DenseVector.fill(1)(Complex(0, 0))
  val inp00 = DenseVector.fill(11)(Complex(0, 0))
  val inpa = DenseVector.fill(6)(Complex(-0.5, 0.5))
  val inpb = DenseVector.fill(13)(Complex(0.5, 0.5))
  val inpc = DenseVector.fill(5)(Complex(0.5, 0.5))
  val inpt1 = Seq.fill(18)(1)
  val inpt2 =  Seq.fill(12)(0)

  val inp = DenseVector.vertcat(inp0,inpa,inp0,inpb,inp0,inpc,inp00,inpc,inp0,inpb,inp0,inpa)
  val inp1 = inpt1 ++inpt2++inpt1
  val inpe1 = new ListBuffer[Int]()//Seq[Int]()
  val inpe2 = Seq.fill(6)(1)++Seq.fill(4)(0)
  val s=1
  for (j <- 0 until 48) {
      val iter = s*floor(j/s)+(j+floor(16* j/48)) %s
      //print("iter=",iter)

    }
   for (i <- 0 until 48) {

      val iter1 =( 16*i -(48 -1)* floor(16* i/48) )
      inpe1 += iter1

   }
    //print("inpe1=",inpe1)


  //BPSKDemodTester(paramsbpskdemodh)
  //BPSKDemodTester1(paramsbpskdemodh)
  //BPSKDemodTester1s(paramsbpskdemods)
  DemodulatorTester(paramsbpskdemodh,inp.toScalaVector)

  behavior of "Serilizer"
  val paramsser = HardDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod =1,
     bitsWidth = 48,
     Ncbps = 8,
     Nbpsc = 4
  )
  //SermTester(paramsser)
  behavior of "Serilizer1"
  val paramsser1 = HardDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod = 1,
     bitsWidth = 48,
     Ncbps = 8,
     Nbpsc = 4
  )
  //Serm1Tester(paramsser1)
  behavior of "Serilizerms1"
  val paramssers1 = SoftDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod =0,
     bitsWidth = 48,
     Ncbps = 8,
     Nbpsc = 4
  )
  //Serms1Tester(paramssers1)

   behavior of "QPSKDEMOD"
  val paramsqpskdemodh = HardDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod = 1,
     bitsWidth = 48,
     Ncbps = 96,
     Nbpsc = 2
  )
  val paramsqpskdemods = SoftDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod = 0,
     bitsWidth = 48,
     Ncbps = 96,
     Nbpsc = 2
  )

  //QPSKDemodSerTester(paramsqpskdemodh)
  //QPSKDemodSer1sTester(paramsqpskdemods)
   behavior of "QAM16DEMOD"
  val paramsqam16demodh = HardDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod = 1,
     bitsWidth = 48,
     Ncbps = 192,
     Nbpsc = 4
  )
  val paramsqam16demods = SoftDemodParams(
     dataWidth = 16,
     dataBinaryPoint = 13,
     width = 64,
     hsmod = 0,
     bitsWidth = 48,
     Ncbps = 192,
     Nbpsc = 4
  )

  //QAM16DemodSerTester(paramsqam16demodh)
  //QAM16DemodSer1Tester(paramsqam16demodh)
  //QAM16DemodSer1sTester(paramsqam16demods)
  //DemodulatorTester(paramsqam16demodh,inp.toScalaVector)







}



