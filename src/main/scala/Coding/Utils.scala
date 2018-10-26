package Coding

import breeze.numerics.{atan, pow, sqrt}

/**
 * Object for computing useful constants
 */
object Utils {
  /**
   * Get sequences of length n that go 1.0, 0.5, 0.25, ...
   */
//  def dec2bitarray(in_number: Int, bit_width: Int) = {
//    val binary_string = in_number.toBinaryString
//    val len = binary_string.length
//    val bitarray = Array.fill(bit_width){0}
//    for (i <- 0 until len){
//      bitarray(bit_width-i-1) = binary_string(len-i-1).asDigit
//    }
//    bitarray
//  }

  def dec2bitarray(genPolynoimial: List[Int], bit_width: Int) = {
    val numRow = genPolynoimial.length
    val bitarray = Array.ofDim[Int](numRow, bit_width)
    for (i <- 0 until numRow){
      val binary_string = genPolynoimial(i).toBinaryString
      val len = binary_string.length
      for (j <- 0 until bit_width){
        bitarray(i)(bit_width-j-1) = binary_string(len-j-1).asDigit
      }
    }
    bitarray
  }

  def findMinBitWidth(arg: List[Int]): Int ={
    val result: Int = math.floor(math.log10(arg.max)/math.log10(2)).asInstanceOf[Int]
    result
  }

  def linear(n: Int) = for (i <- 0 until n) yield pow(2.0, -i)
  /**
   * Get gain for n-stage CORDIC
   */
  def gain(n: Int) = linear(n).map(x => sqrt(1 + x * x)).reduce(_ * _)
  /**
   * Get sequences of length n that go atan(1), atan(0.5), atan(0.25), ...
   */
  def arctan(n: Int) = linear(n).map(atan(_))

  def halfPi = math.Pi/2.0
}
