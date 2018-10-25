package cordic

import breeze.numerics.{atan, pow, sqrt, floor}

/**
 * Object for computing useful constants
 */
object Constants {
  /**
   * Get sequences of length n that go 1.0, 0.5, 0.25, ...
   */
  def linear(n: Int) = for (i <- 0 until n) yield pow(2.0, -i)
  /**
   * Get gain for n-stage CORDIC
   */
  def gain(n: Int) = linear(n).map(x => sqrt(1 + x * x)).reduce(_ * _)
  /**
   * Get sequences of length n that go atan(1), atan(0.5), atan(0.25), ...
   */
  def arctan(n: Int) = linear(n).map(atan(_))

  def interleaving1(Ncbps: Int, k: Int): Int =  floor(Ncbps/16) * (k % 16) + floor(k/16)
  //def interleaving2(Ncbps: Int, i: Int, Nbpsc: Int) Int = linear(i).map( max(Nbpsc/2, 1)*(i/max(Nbpsc/2, 1)) ) + (i+ Ncbps - (16*i/Ncbps)%max(Nbpsc/2,1)) )

  
}
