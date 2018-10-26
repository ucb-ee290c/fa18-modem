package modem

import breeze.numerics.{atan, pow, sqrt}

/**
 * Object for computing useful functions
 */
object FFTUtil {
  def factorize(x: Int): List[Int] = {
    var a : Int = 2
    var factors = List[Int]()
    var y = x
    while (a * a <= y) {
      if (y % a == 0) {
        factors = factors :+ a
        do y = y / a
        while (y % a == 0)
      }
      else a = a + 1
    }
    if (y != 1) factors = factors :+ y
    factors
  }

  def gcd_extended(a: Int, b: Int): (Int, Int, Int) = {
    if (a == 0) {
      (b, 0, 1)
    } else {
      val (b_new, x_old, y_old) = gcd_extended(b % a, a)
      (b_new, y_old - b / a * x_old, x_old)
    }
  }

  def mult_inv(g: Int, n: Int): Int = {
    (gcd_extended(g, n)._2 % n + n) % n
  }

  def primitive_root(n: Int): Int = {
    val powers = factorize(n - 1).map((n - 1) / _)
    (2 until n).toList.find(x => {
      val modded = powers.map(scala.math.pow(x, _) % n)
      !modded.contains(1)
    }).getOrElse(0) // TODO: Raise error if not found?
  }

  def is_prime(i :Int) : Boolean = {
    if (i <= 1) false
    else if (i == 2) true
    else !(2 to (i - 1)).exists(x => i % x == 0)
  }
}