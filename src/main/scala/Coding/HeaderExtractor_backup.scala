//package modem
//
//import chisel3._
//import chisel3.util._
//import dsptools.numbers._
////import freechips.rocketchip.diplomacy.LazyModule
////import freechips.rocketchip.subsystem.BaseSubsystem
//
//// Written by Kunmo Kim : kunmok@berkeley.edu
//// Description: 48-bit minimum latency Viterbi-Decoder to extract information from header block
//class HeaderExtractor[T <: Data: Real](params: CodingParams[T]) extends Module {
//  val io = IO(new Bundle {
//    val in        = Input(Vec((params.n * params.H), SInt(2.W)))      // from De-Puncturing
//    val isHead    = Input(Bool())                                     // from arbiter
//    val headInfo  = Decoupled(DecodeHeadBundle())                     // to De-Puncturing
//  })
//  // ***************************************************************************************************
//  // ************************************* Path Metric Calculation *************************************
//  // ***************************************************************************************************
//  val H                   = params.H          // # of bits in header block
//  val m                   = params.m          // # of memory for convolutional coding
//  val N                   = params.nStates    // # of possible states
//  val branchMetricModule  = VecInit(Seq.fill(H)(Module(new BranchMetric[T](params)).io))
//  (0 until (params.n*H)).map(i => { branchMetricModule(i/params.n).in(i % (params.n)) := io.in(i) })
//
//  val numRows             = math.pow(2.0, (params.m-1).asInstanceOf[Double]).asInstanceOf[Int]
//  val tmpSP               = Wire(Vec(H, Vec(N, Vec(params.numInputs, UInt(params.m.W)))))
//  for (hdr <- 0 until H)
//    for (currentInput <- 0 until params.numInputs)
//      for (currentStates <- 0 until N)
//        tmpSP(hdr)(currentStates/2+currentInput*numRows)(currentStates%2) := currentStates.U
//
//  val survivalPath        = RegInit(VecInit(Seq.fill(H)(VecInit(Seq.fill(N)(0.U(params.m.W))))))
//  val pmRegs              = RegInit(VecInit(Seq.fill(H+1)(VecInit(Seq.fill(N)(0.U(params.pmBits.W))))))
//  val SPcalcCompleted     = RegInit(false.B)    // flag for SP & PM calculation
//
//  when (io.isHead === true.B){
//    // temporary matrix for Path Metric calculation
//    val tmpPM   = Wire(Vec(H+1, Vec(N, Vec(params.numInputs, UInt(params.pmBits.W)))))
//    // temporary matrix for Branch Metric calculation
//    val tmpBM   = Wire(Vec(H+1, Vec(N, Vec(params.numInputs, UInt((log2Ceil(params.n)+1).W)))))
//    // temporary matrix for ACS (mainly for accumulation)
//    val tmpAcc  = Wire(Vec(H+1, Vec(N, Vec(params.numInputs, UInt(params.pmBits.W)))))
//    val pmWire  = Wire(Vec(H+1, Vec(N, UInt(params.pmBits.W))))
//
//    // for hdr = 0
//    for (currentInput <- 0 until params.numInputs) {
//      for (currentStates <- 0 until N) {
//        if (currentStates == 0) {
//          tmpPM(0)(currentStates/2 + currentInput*numRows)(currentStates % 2) := 0.U
//          pmRegs(0)(currentStates) := 0.U
//          pmWire(0)(currentStates) := 0.U
//        } else {
//          tmpPM(0)(currentStates/2 + currentInput*numRows)(currentStates % 2) := 100.U
//          pmRegs(0)(currentStates) := 100.U
//          pmWire(0)(currentStates) := 100.U
//        }
//        tmpBM(0)(currentStates/2 + currentInput*numRows)(currentStates % 2) := 0.U
//        tmpAcc(0)(currentStates)(currentInput) := tmpPM(0)(currentStates)(currentInput) + tmpBM(0)(currentStates)(currentInput)
//      }
//    }
//
//    for(hdr <- 1 until H+1) {
//      for (currentInput <- 0 until params.numInputs){
//        for (currentStates <- 0 until N){
//          tmpPM(hdr)(currentStates/2 + currentInput*numRows)(currentStates%2) := pmWire(hdr-1)(currentStates)
//          tmpBM(hdr)(currentStates/2 + currentInput*numRows)(currentStates%2) := branchMetricModule(hdr-1).out_dec(currentStates)(currentInput)
//          tmpAcc(hdr)(currentStates)(currentInput) := tmpPM(hdr)(currentStates)(currentInput) + tmpBM(hdr)(currentStates)(currentInput)
//        }
//      }
//
//      if(hdr < H+1) {
//        for (nRow <- 0 until N){
//          when(tmpAcc(hdr)(nRow)(0) < tmpAcc(hdr)(nRow)(1)){
//            pmRegs(hdr)(nRow)         := tmpAcc(hdr)(nRow)(0)
//            pmWire(hdr)(nRow)         := tmpAcc(hdr)(nRow)(0)
//            survivalPath(hdr-1)(nRow) := tmpSP(hdr-1)(nRow)(0)
//          }.otherwise{
//            pmRegs(hdr)(nRow)         := tmpAcc(hdr)(nRow)(1)
//            pmWire(hdr)(nRow)         := tmpAcc(hdr)(nRow)(1)
//            survivalPath(hdr-1)(nRow) := tmpSP(hdr-1)(nRow)(1)
//          }
//        }
//      }
//    }
//    SPcalcCompleted := true.B           // need to reset this later
//  }
//  // *************************************************************************************
//  // ************************************* Traceback *************************************
//  // *************************************************************************************
//  // declare variables for decoding process
//  val outValid          = RegInit(false.B)
//  val trackValid        = RegInit(VecInit(Seq.fill(3)(0.U(1.W))))   // to relax time constraint of critical timing path
//  val decodeReg         = Reg(Vec(H, UInt(params.k.W)))
//  val lengthInfoWire    = Wire(Vec(12, UInt(12.W)))
//
//  // start decoding !
//  when((io.isHead === true.B) && (SPcalcCompleted === true.B)) {
//    val tmpPMMin          = Wire(Vec(N - 1, UInt(m.W)))
//    val tmpPMMinIndex     = Wire(Vec(N - 1, UInt(m.W)))
//    val tmpSPforTB  = Wire(Vec(H-1, UInt(m.W)))
//
//    // find minimum in PM list
//    tmpPMMin(0)           := Mux(pmRegs(H)(0) < pmRegs(H)(1), pmRegs(H)(0), pmRegs(H)(1))
//    tmpPMMinIndex(0)      := Mux(pmRegs(H)(0) < pmRegs(H)(1), 0.U, 1.U)
//    for (i <- 1 until N - 1) {
//      tmpPMMin(i)         := Mux(tmpPMMin(i-1) < pmRegs(H)(i+1), tmpPMMin(i-1), pmRegs(H)(i+1))
//      tmpPMMinIndex(i)    := Mux(tmpPMMin(i-1) < pmRegs(H)(i+1), tmpPMMinIndex(i-1), (i+1).U)
//    }
//
//    trackValid(0)       := 1.U                            // trackValid is used to raise/lower io.out.valid signal
//    decodeReg(H-1)      := tmpPMMinIndex(N-2) >> (m-1)    // grab the minimum PM
//
//    tmpSPforTB(H-2) := survivalPath(H-1)(tmpPMMinIndex(N - 2))  // grab the minimum PM
//    decodeReg(H-2)  := tmpSPforTB(H-2) >> (m-1)           // decode the 2nd MSB of header information
//    for (i <- (0 until H-2).reverse) {
//      tmpSPforTB(i) := survivalPath(i+1)(tmpSPforTB(i+1))
//      decodeReg(i)  := tmpSPforTB(i) >> (m-1) // get MSB
//    }
//  }
//  // tracking should be completed within 3 clock cycles
//  when(trackValid(0) === 1.U){
//    (2 to 1 by -1).map(i => {trackValid(i) := trackValid(i-1)})
//  }
//
//  when(trackValid(2) === 1.U) {
//    outValid        := true.B
//  }
//
//  when(io.headInfo.fire() === true.B && io.isHead === false.B){
//    SPcalcCompleted := false.B
//    outValid        := false.B
//  }
//  for(i <- (0 until H).reverse){
////    printf(p"**************** i = ${i}, decodeReg = ${decodeReg(i)} **************** \n")
//  }
//  // normal operation mode
//  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := decodeReg(i)             })
//  (0 until 12).map(i  => { lengthInfoWire(i)         := decodeReg(5 + i) << (i)  })
//  io.headInfo.bits.dataLen    := lengthInfoWire.reduce(_ + _) << 3  // contains number of data "octets" in a packet
//  io.headInfo.valid           := outValid
//
//  // test mode (H=6)
////  (0 until 4).map(i   => { io.headInfo.bits.rate(i)  := 0.U             })
////  (0 until 12).map(i  => { lengthInfoWire(i)  := 0.U             })
////  io.headInfo.bits.dataLen    := 4.U
////  io.headInfo.valid           := true.B
//}
//
