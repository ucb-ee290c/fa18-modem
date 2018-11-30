package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{DenseVector, randomDouble}
import chisel3.util.log2Ceil





class modulatorSpec extends FlatSpec with Matchers {
  behavior of "ShiftRegister"
   


  behavior of "Interleaver_modify"
  val paramsinterleaver = FixedBPSKModParams(
     dataWidth = 12,
     dataBinaryPoint = 10,
     Ncbps = 48,
     Nbpsc = 1
  )
  //InterleaverTester(paramsinterleaver)
   behavior of "Interleaverds1_modify"
  val paramsinterleaverds1 =  FixedModFFTParams(
    dataWidth = 16,
    bitsWidth = 48,
    Ncbps = 4,
    Nbpsc = 1,
    twiddleWidth = 10,
    numPoints = 2,
    binPoints = 2,
    fftType = "direct"
  )
 //Interleaverds1Tester(paramsinterleaverds1)
  // Interleaverds1bTester(paramsinterleaverds1)
  behavior of "Interleaverds1_modify"
  val paramsinterleaverds2 =  FixedModFFTParams(
    dataWidth = 16,
    bitsWidth = 48,
    Ncbps = 8,
    Nbpsc = 4,
    twiddleWidth = 10,
    numPoints = 2,
    binPoints = 2,
    fftType = "direct"
  )
 //Interleaverds2Tester(paramsinterleaverds2)
 //Interleaverds2bTester(paramsinterleaverds2)


 behavior of "Deinterleaver_modify"
  val paramsdeinterleaver = FixedBPSKModParams(
     dataWidth = 12,
     dataBinaryPoint = 10,
     Ncbps = 48,
     Nbpsc = 1
  )
  //DeinterleaverTester(paramsdeinterleaver)

 behavior of "BPSKModulator"
  val paramsbpskmod =  FixedModFFTParams(
    dataWidth = 16,
    Ncbps = 4,
    bitsWidth = 48,
    Nbpsc = 1,
    twiddleWidth = 10,
    numPoints = 2,
    binPoints = 2,
    fftType = "direct"
  )

  //BPSKCPModTester(paramsbpskmod)
 // BPSKCPMod1Tester(paramsbpskmod)
  behavior of "qam16Modulator"
  val paramsqammod1 =  FixedModFFTParams(
    dataWidth = 16,
    bitsWidth = 48,
    Ncbps = 8,
    Nbpsc = 4,
    twiddleWidth = 10,
    numPoints = 2,
    binPoints = 14,
    fftType = "direct"
  )
  //QAM16CPMod1Tester(paramsqammod1)
  QAM16ModulatorTester(paramsqammod1)
  

 behavior of "Serilizer"
  val paramser = FixedBPSKModParams(
     dataWidth = 12,
     dataBinaryPoint = 10,
     Ncbps = 4,
     Nbpsc = 1
  )
  SerTester(paramser)
 behavior of "QAM16Demodulator"
  val paramsqam16demod = FixedBPSKModParams(
     dataWidth = 12,
     dataBinaryPoint = 10,
     Ncbps = 192,
     Nbpsc = 4
  )
  //QAM16DemodTester(paramsqam16demod)
  behavior of "BPSKDemodulator"
  val paramsbpskdemod = FixedBPSKModParams(
     dataWidth = 12,
     dataBinaryPoint = 10,
     Ncbps = 48,
     Nbpsc = 1
  )
  //BPSKDemodTester(paramsbpskdemod)
 behavior of "BPSKSerDemodulator"
  val paramserbpskdemod = FixedBPSKModParams(
     dataWidth = 12,
     dataBinaryPoint = 10,
     Ncbps = 48,
     Nbpsc = 1
  )
  //BPSKSerDemodTester(paramserbpskdemod)
 behavior of "QPSKmodulatorm"
  val paramsqpskmod1 =   FixedModFFTParams(
    dataWidth = 16,
    bitsWidth = 48,
    Ncbps = 4,
    Nbpsc = 2,
    twiddleWidth = 10,
    numPoints = 2,
    binPoints = 14,
    fftType = "direct"
  )
  QPSKCPModulatorTester(paramsqpskmod1)
  BPSKCPModulatorTester(paramsqpskmod1)

  //QPSKCPMod1Tester(paramsqpskmod1)
 behavior of "QPSKModulator"
  val paramsqpskmod = FixedBPSKModParams(
     dataWidth = 16,
     dataBinaryPoint = 14,
     Ncbps = 96,
     Nbpsc = 2
  )
  //QPSKCPModulatorTester(paramsqpskmod)


  
  behavior of "Demapper"
  val paramsdemap = FixedDemapParams(
      iqwidth = 12,
      dataBinaryPoint = 10,
      n = 4
  )

  //val baseTrial = IQ(in=1.0, qn=1.0,out=5.0)
  //val anglesi = Seq(0.5, 1)
  //val anglesq = Seq(0.25, -1)
  //val trial1 = anglesi.map { in1 => baseTrial.copy(in = in1)}
  //val trials = List(IQ(1.0, 0.25, 13.0), IQ(1.0, -1.0, 1.0))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  //DemapperTester(paramsdemap,trials)

 behavior of "Mapper"
  val paramsmap = FixedMapParams(
      iqwidth = 12,
      dataBinaryPoint = 10,
      n = 4
  )
  
  val trial1s = List(IQG(3.0, 0.316, -0.949), IQG(6.0, -0.316, 0.949))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  //MapperTester(paramsmap,trial1s)
 behavior of " QPSK modulator + FFT"
 val base_qmparams = FixedModFFTParams(
    dataWidth = 16,
    bitsWidth = 48,
    Ncbps = 96,
    Nbpsc = 2,
    twiddleWidth = 10,
    numPoints = 2,
    binPoints = 2,
    fftType = "direct"
  )
  //val inp = DenseVector.fill(1)(Complex(0, 0))++DenseVector.fill(6)(Complex(0.707, 0.707)) ++ DenseVector.fill(1)(Complex(0, 0))++
  //DenseVector.fill(13)(Complex(0.707, 0.707)) ++ DenseVector.fill(1)(Complex(0, 0))++ DenseVector.fill(5)(Complex(0.707, 0.707))++
  //DenseVector.fill(11)(Complex(0, 0))++DenseVector.fill(5)(Complex(0.707, 0.707))++DenseVector.fill(1)(Complex(0, 0))++
  //DenseVector.fill(13)(Complex(0.707, 0.707))++ DenseVector.fill(1)(Complex(0, 0))++DenseVector.fill(6)(Complex(0.707, 0.707))
  val inp0 = DenseVector.fill(1)(Complex(0, 0))
  val inp00 = DenseVector.fill(11)(Complex(0, 0))
  val inpa = DenseVector.fill(6)(Complex(0.707, 0.707))
  val inpb = DenseVector.fill(13)(Complex(0.707, 0.707))
  val inpc = DenseVector.fill(5)(Complex(0.707, 0.707))
  val inp = DenseVector.vertcat(inp0,inpa,inp0,inpb,inp0,inpc,inp00,inpc,inp0,inpb,inp0,inpa)
 
  val out_ifft = iFourierTr(inp)
  val binPoint = base_qmparams.dataWidth-2-log2Ceil(64)
  val params = base_qmparams.copy(numPoints = 64, binPoints = binPoint, pipeline = true)
  val sdf_params = params.copy(fftType="sdf")
  //FixedQPSKModFFTTester(params, inp.toScalaVector, out_ifft.toScalaVector) should be (true)
 
 behavior of "Modulatorm"
  val paramsmodm = FixedMapParams(
      iqwidth = 12,
      dataBinaryPoint = 10,
      n = 2
  )
  val base_params = FixedModParams(
    dataWidth = 12,
    dataBinaryPoint = 10,
    n = 2
)
  val trialmodm = List(IQGM(0 ,0, -0.707, -0.707),IQGM(0 ,1, 0.707, -0.707))
//,IQGM(0 ,1, 0.707, -0.707),IQGM(1 ,1, 0.707, 0.707))
//IQGM(0 ,1, 0.707, -0.707))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  //ModulatormTester(paramsmodm,base_params, trialmodm)

 //behavior of "Modulator"
  val paramsmod = FixedMapParams(
      iqwidth = 12,
      dataBinaryPoint = 10,
      n = 1
  )
  
  val trialmod = List(IQG(1, 1, 0), IQG(0, -1, 0), IQG(0, -1, 0))
  //= anglesq.map {qn1 => baseTrial.copy(qn = qn1)}
  //ModulatorTester(paramsmod,trialmod)
  

 

}
