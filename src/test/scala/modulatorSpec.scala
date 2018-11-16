package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{DenseVector, randomDouble}




class modulatorSpec extends FlatSpec with Matchers {
  behavior of "ShiftRegister"
  val params1 = Uintp2sParams(
     mwidth = 4,
     n = 5
  )
  ShiftRegisterTester(params1)
  

  //behavior of "Fir"
  //val paramsfir = UIntFirParams(
     //mwidth = 8,
     //length = 4
  //)
  //MFirTester(paramsfir)
  
  behavior of "Interleaver"
  val paramsinter = UintInterleavParams(
     mwidth = 8,
     Ncbps = 48,
     Nbpsc = 4
  )
  //InterleavTester(paramsinter)

  behavior of "Interleaver_modify"
  val paramsinterleaver = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 48,
     Nbpsc = 1
  )
  //InterleaverTester(paramsinterleaver)

 behavior of "Deinterleaver_modify"
  val paramsdeinterleaver = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 48,
     Nbpsc = 1
  )
  //DeinterleaverTester(paramsdeinterleaver)

 behavior of "BPSKModulator"
  val paramsbpskmod = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 48,
     Nbpsc = 1
  )
  //BPSKCPModTester(paramsbpskmod)
 behavior of "Serilizer"
  val paramser = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 4,
     Nbpsc = 1
  )
  SerTester(paramser)
 behavior of "QAM16Demodulator"
  val paramsqam16demod = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 192,
     Nbpsc = 4
  )
  //QAM16DemodTester(paramsqam16demod)
  behavior of "BPSKDemodulator"
  val paramsbpskdemod = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 48,
     Nbpsc = 1
  )
  //BPSKDemodTester(paramsbpskdemod)
 behavior of "BPSKSerDemodulator"
  val paramserbpskdemod = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 48,
     Nbpsc = 1
  )
  //BPSKSerDemodTester(paramserbpskdemod)
 behavior of "QPSKDemodulator"
  val paramsqpskdemod = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 96,
     Nbpsc = 2
  )
  //QPSKDemodTester(paramsqpskdemod)
 behavior of "QPSKModulator"
  val paramsqpskmod = FixedBPSKModParams(
     dataWidth = 12,
     Ncbps = 96,
     Nbpsc = 2
  )
  //QPSKCPModTester(paramsqpskmod)

  behavior of "Deinterleaver"
  val paramsdeinter = UintInterleavParams(
     mwidth = 8,
     Ncbps = 96,
     Nbpsc = 2
  )
  //DeinterleavTester(paramsdeinter)

  behavior of "Demapper"
  val paramsdemap = FixedDemapParams(
      iqwidth = 12,
      n = 4
  )

  val baseTrial = IQ(in=1.0, qn=1.0,out=5.0)
  val anglesi = Seq(0.5, 1)
  val anglesq = Seq(0.25, -1)
  val trial1 = anglesi.map { in1 => baseTrial.copy(in = in1)}
  val trials = List(IQ(1.0, 0.25, 13.0), IQ(1.0, -1.0, 1.0))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  //DemapperTester(paramsdemap,trials)

 behavior of "Mapper"
  val paramsmap = FixedMapParams(
      iqwidth = 12,
      n = 4
  )
  
  val trial1s = List(IQG(3.0, 0.316, -0.949), IQG(6.0, -0.316, 0.949))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  MapperTester(paramsmap,trial1s)
 behavior of " QPSK modulator + FFT"
 val base_qmparams = FixedModFFTParams(
    dataWidth = 16,
    Ncbps = 96,
    Nbpsc = 2,
    twiddleWidth = 10,
    numPoints = 2,
    maxVal = 2,
  )
  //val inp = DenseVector.fill(1)(Complex(0, 0))++DenseVector.fill(6)(Complex(0.707, 0.707)) ++ DenseVector.fill(1)(Complex(0, 0))++
  //DenseVector.fill(13)(Complex(0.707, 0.707)) ++ DenseVector.fill(1)(Complex(0, 0))++ DenseVector.fill(5)(Complex(0.707, 0.707))++
  //DenseVector.fill(11)(Complex(0, 0))++DenseVector.fill(5)(Complex(0.707, 0.707))++DenseVector.fill(1)(Complex(0, 0))++
  //DenseVector.fill(13)(Complex(0.707, 0.707))++ DenseVector.fill(1)(Complex(0, 0))++DenseVector.fill(6)(Complex(0.707, 0.707))
  val inp0 = DenseVector.fill(1)(Complex(0, 0))
  val inpa = DenseVector.fill(6)(Complex(0.707, 0.707))
  val inpb = DenseVector.fill(13)(Complex(0.707, 0.707))
  val inpc = DenseVector.fill(5)(Complex(0.707, 0.707))
  val inp = DenseVector.vertcat(inp0,inpa,inp0,inpb,inp0,inpc,inp0,inpc,inp0,inpb,inp0,inpa)
 
  val out_ifft = iFourierTr(inp)
  val params = base_qmparams.copy(numPoints = 64, maxVal = 64, pipeline = true)
  //FixedQPSKModFFTTester(params, inp.toScalaVector, out_ifft.toScalaVector) should be (true)
 
 behavior of "Modulatorm"
  val paramsmodm = FixedMapParams(
      iqwidth = 12,
      n = 2
  )
  val base_params = FixedModParams(
    dataWidth = 12,
    n = 2
)
  val trialmodm = List(IQGM(0 ,0, -0.707, -0.707),IQGM(0 ,1, 0.707, -0.707))
//,IQGM(0 ,1, 0.707, -0.707),IQGM(1 ,1, 0.707, 0.707))
//IQGM(0 ,1, 0.707, -0.707))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  ModulatormTester(paramsmodm,base_params, trialmodm)

 //behavior of "Modulator"
  val paramsmod = FixedMapParams(
      iqwidth = 12,
      n = 1
  )
  
  val trialmod = List(IQG(1, 1, 0), IQG(0, -1, 0), IQG(0, -1, 0))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  //ModulatorTester(paramsmod,trialmod)
  

 

}
