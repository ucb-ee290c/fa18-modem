package cordic

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}


class FixedCordicSpec extends FlatSpec with Matchers {
  behavior of "ShiftRegister"
  val params1 = Uintp2sParams(
     mwidth = 4,
     n = 5
  )
  ShiftRegisterTester(params1)
  

  behavior of "Fir"
  val paramsfir = UIntFirParams(
     mwidth = 8,
     length = 4
  )
  MFirTester(paramsfir)
  
  behavior of "Interleaver"
  val paramsinter = UintInterleavParams(
     mwidth = 8,
     Ncbps = 48,
     Nbpsc = 4
  )
  InterleavTester(paramsinter)

  behavior of "Deinterleaver"
  val paramsdeinter = UintInterleavParams(
     mwidth = 8,
     Ncbps = 48,
     Nbpsc = 4
  )
  InterleavTester(paramsdeinter)

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
  DemapperTester(paramsdemap,trials)

 behavior of "Mapper"
  val paramsmap = FixedMapParams(
      iqwidth = 12,
      n = 4
  )
  

 
 

}
