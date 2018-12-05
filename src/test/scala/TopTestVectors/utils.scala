package modem

import breeze.math.{Complex}

object SSVConverter{
  def toComplex(realFilename: String, imagFilename: String): Seq[Complex] = {
    var outVec = Seq.empty[Complex]
    val reals = toDoubleSeq(realFilename)
    val imags = toDoubleSeq(imagFilename)
    val cmplxs = reals zip imags
    val complexes = cmplxs.map{case (real, imag) => Complex(real, imag)}
    complexes.foreach{x => outVec = outVec :+ x}
    outVec
  }

  def toIntSeq(filename: String): Seq[Int] = {
    var outVec = Seq.empty[Int]
    val file = io.Source.fromFile(filename)
    val lines = file.getLines
    for (line <- lines){
      val ints = line.split(" ").map{_.trim}.map{_.toInt}
      ints.foreach{x => outVec = outVec :+ x}
    }
    outVec
  }

  def toDoubleSeq(filename: String): Seq[Double] = {
    var outVec = Seq.empty[Double]
    val file = io.Source.fromFile(filename)
    val lines = file.getLines
    for (line <- lines){
      val doubles = line.split(" ").map{_.trim}.map{_.toDouble}
      doubles.foreach{x => outVec = outVec :+ x}
    }
    outVec
  }
}

object CSVConverter{
  def toComplex(realFilename: String, imagFilename: String): Seq[Complex] = {
    var outVec = Seq.empty[Complex]
    val reals = toDoubleSeq(realFilename)
    val imags = toDoubleSeq(imagFilename)
    val cmplxs = reals zip imags
    val complexes = cmplxs.map{case (real, imag) => Complex(real, imag)}
    complexes.foreach{x => outVec = outVec :+ x}
    outVec
  }

  def toIntSeq(filename: String): Seq[Int] = {
    var outVec = Seq.empty[Int]
    val file = io.Source.fromFile(filename)
    val lines = file.getLines
    for (line <- lines){
      val ints = line.split(",").map{_.trim}.map{_.toInt}
      ints.foreach{x => outVec = outVec :+ x}
    }
    outVec
  }

  def toDoubleSeq(filename: String): Seq[Double] = {
    var outVec = Seq.empty[Double]
    val file = io.Source.fromFile(filename)
    val lines = file.getLines
    for (line <- lines){
      val doubles = line.split(",").map{_.trim}.map{_.toDouble}
      doubles.foreach{x => outVec = outVec :+ x}
    }
    outVec
  }
}
