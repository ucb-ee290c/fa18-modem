package Coding

import chisel3._

class Trellis[T <: Data](params: CodingParams[T]){
  require(params.m > 1)
  require(params.k > 0)
  require(params.n > 0)

  val numInputs   = math.pow(2.0, params.k.asInstanceOf[Double]).asInstanceOf[Int]
  val output_table = Array.ofDim[Int](params.nStates, numInputs, params.n)       // array storing outputs
  val nextstate_table = Array.ofDim[Int](params.nStates, numInputs, params.m)    // array storing next states
  val nextstate_dec = Array.ofDim[Int](params.nStates, numInputs)       // array storing next states in decimal
  val outbits = Array.fill(params.n){0}
  val shiftReg = Array.fill(params.m){0}
  val generatorArray = Array.fill(params.K){0}

  for (currentStates <- 0 until params.nStates){
    for (currentInputs <- 0 until numInputs){
      (0 until params.n).map(i => {outbits(i) = 0 })            // reset outbits to zeros
      for (r <- 0 until params.n){
        (0 until params.m).map(i => {shiftReg(i) = CodingUtils.dec2bitarray_unit(currentStates, params.m)(i) })
        (0 until params.K).map(i => {generatorArray(i) = CodingUtils.dec2bitarray_unit(params.genPolynomial(r), params.K)(i)})
        for (i <- 0 until params.m){
          outbits(r) = (outbits(r) + shiftReg(i)*generatorArray(i+1)) % 2
        }
        (1 until params.m).reverse.map(i => {shiftReg(i) = shiftReg(i-1) })  // start bit shifting
        shiftReg(0) = currentInputs

        outbits(r) = (outbits(r) + (currentInputs * generatorArray(0))) % 2
        output_table(currentStates)(currentInputs)(r) = outbits(r)
        nextstate_table(currentStates)(currentInputs)(r) = shiftReg(r)
      }
      nextstate_dec(currentStates)(currentInputs) = CodingUtils.bitarray2dec(nextstate_table(currentStates)(currentInputs))
    }
  }
  /*
Following is for G=(7, 5)
State | In  | Out | Next State
0     | 0   | 00  | 0
0     | 1   | 11  | 2

2     | 0   | 10  | 1
2     | 1   | 01  | 3

1     | 0   | 11  | 0
1     | 1   | 00  | 2

3     | 0   | 01  | 1
3     | 1   | 10  | 3
*/
//  val currentstate_dec = Array.ofDim[Int](params.nStates, numInputs)    // array storing current states
//  for (currentInputs <- 0 until numInputs){
//    for (currentStates <- 0 until params.nStates){
//      for (nextStates <- 0 until params.nStates){
//        if (nextstate_dec(currentStates)(currentInputs) == nextStates) {
//          currentstate_dec(nextStates)(currentStates) = nextStates
//        }
//      }
//    }
//  }



}
