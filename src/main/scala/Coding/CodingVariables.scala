package modem

/**
 * Object for computing useful constants
 */
object CodingVariables {
  // ********** Define matrix for puncturing process **********
  // puncturing matrix
  val punctureList1       = Array(Array(1, 0, 0, 0, 0, 0, 0), Array(1, 0, 0, 0, 0, 0, 0))   // rate = 1/2
  val punctureList2       = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(1, 0, 0, 0, 0, 0, 0))   // rate = 2/3
  val punctureList3       = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(1, 0, 1, 0, 0, 0, 0))   // rate = 3/4
  val punctureList4       = Array(Array(1, 1, 0, 1, 0, 0, 0), Array(1, 0, 1, 0, 1, 0, 0))   // rate = 5/6 -> not in 802.11a
  val punctureList5       = Array(Array(1, 1, 1, 1, 0, 1, 0), Array(1, 0, 0, 0, 1, 0, 1))   // rate = 7/8 -> not in 802.11a

  // puncIndices contains buffer address offset
  // ex) [1,1,0],[1,0,1] -> [1,1,0],[2,1,1] : accumulate over rows
  val puncIndices1        = Array(Array(1, 0, 0, 0, 0, 0, 0), Array(2, 0, 0, 0, 0, 0, 0))   // rate = 1/2
  val puncIndices2        = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(2, 1, 0, 0, 0, 0, 0))   // rate = 2/3
  val puncIndices3        = Array(Array(1, 1, 0, 0, 0, 0, 0), Array(2, 1, 1, 0, 0, 0, 0))   // rate = 3/4
  val puncIndices4        = Array(Array(1, 1, 0, 1, 0, 0, 0), Array(2, 1, 1, 1, 1, 0, 0))   // rate = 5/6 -> not in 802.11a
  val puncIndices5        = Array(Array(1, 1, 1, 1, 0, 1, 0), Array(2, 1, 1, 1, 1, 1, 1))   // rate = 7/8 -> not in 802.11a

  // puncListColSum contains summation over rows
  // ex) [1,1,0], [1,0,1] -> [2,1,1]
  val puncListColSum1     = Array(2, 0, 0, 0, 0, 0, 0)  // rate = 1/2
  val puncListColSum2     = Array(2, 1, 0, 0, 0, 0, 0)  // rate = 2/3
  val puncListColSum3     = Array(2, 1, 1, 0, 0, 0, 0)  // rate = 3/4
  val puncListColSum4     = Array(2, 1, 1, 1, 1, 0, 0)  // rate = 5/6 -> not in 802.11a
  val puncListColSum5     = Array(2, 1, 1, 1, 1, 1, 1)  // rate = 7/8 -> not in 802.11a

  val puncMatBitWidth1    = 1
  val puncMatBitWidth2    = 2
  val puncMatBitWidth3    = 3
  val puncMatBitWidth4    = 5
  val puncMatBitWidth5    = 7

}
