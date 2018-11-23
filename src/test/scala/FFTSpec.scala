package modem

import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}
import breeze.math.{Complex}
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.{DenseVector, randomDouble}
import chisel3.util.log2Ceil

class FFTSpec extends FlatSpec with Matchers {

  def test_setup(base_params: FixedFFTParams, numPoints: Int, fftType: String): (FixedFFTParams, Vector[Complex], Vector[Complex], Vector[Complex]) = {
    val inp      = DenseVector.fill(numPoints) { Complex(randomDouble() * 2 - 1, randomDouble() * 2 - 1) }
    val out_fft  = fourierTr(inp).toScalaVector
    val out_ifft = iFourierTr(inp).toScalaVector
    val binPoint = base_params.dataWidth-2-log2Ceil(numPoints)
    val new_params = base_params.copy(numPoints=numPoints, binPoint=binPoint, fftType=fftType)
    (new_params, inp.toScalaVector, out_fft, out_ifft)
  }

  behavior of "FixedFFT"

  val base_params = FixedFFTParams(
    dataWidth = 10,
    twiddleWidth = 10,
    numPoints = 2,
    binPoint = 2,
    fftType = "direct"
  )

  for (i <- Seq(2, 4, 8, 16)) {
    it should f"compute $i-point Direct FFT/IFFT" in {
      val (params, inp, out_fft, out_ifft) = test_setup(base_params, i, "direct")
      FixedFFTTester(params,  inp, out_fft ) should be (true)
      FixedIFFTTester(params, inp, out_ifft) should be (true)
    }
    it should f"compute $i-point SDF FFT/IFFT" in {
      val (params, inp, out_fft, out_ifft) = test_setup(base_params, i, "sdf")
      FixedFFTTester(params,  inp, out_fft ) should be (true)
      FixedIFFTTester(params, inp, out_ifft) should be (true)
    }
  }

  for (i <- Seq(5, 7)) {
    it should f"compute $i-point FFT" in {
      val (params, inp, out_fft, _) = test_setup(base_params, i, "direct")
      FixedFFTTester(params, inp, out_fft) should be (true)
    }
  }

  behavior of "FFTUtil"
  it should "check factorize" in {
    val inputs = List(12, 52, 25)
    val outputs = List(List(2, 3), List(2, 13), List(5))
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.factorize(inp) == out)
      }
    }
  }
  it should "check gcd extended" in {
    val as   = List(30, 35)
    val bs   = List(20, 15)
    val gcds = List(10, 5)
    val xs   = List(1 , 1)
    val ys   = List(-1, -2)
    val inputs = as.zip(bs)
    val outputs = (gcds, xs, ys).zipped.toList
    inputs.zip(outputs).foreach {
      case ((a, b), (gcd, x, y)) => {
        assert(FFTUtil.gcd_extended(a, b) == (gcd, x, y))
      }
    }
  }
  it should "check mult inv" in {
    val inputs = List(List(2, 13), List(4, 13))
    val outputs = List(7, 10)
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.mult_inv(inp(0), inp(1)) == out)
      }
    }
  }
  it should "check primitive root" in {
    val inputs  = List(13, 17)
    val outputs = List(2 , 3)
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.primitive_root(inp) == out)
      }
    }
  }
  it should "check is prime" in {
    val inputs  = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val outputs = List(false, true, true, false, true, false, true, false, false, false)
    inputs.zip(outputs).foreach {
      case (inp, out) => {
        assert(FFTUtil.is_prime(inp) == out)
      }
    }
  }
}
