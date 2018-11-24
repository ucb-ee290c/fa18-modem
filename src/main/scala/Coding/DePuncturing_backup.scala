//package modem
//
//import chisel3._
//import chisel3.util._
//import chisel3.experimental.FixedPoint
//import dsptools.numbers._
//import freechips.rocketchip.diplomacy.LazyModule
//import freechips.rocketchip.subsystem.BaseSubsystem


//when(io.stateIn =/= sDone && io.inReady === 1.U && io.isHead === 0.U){          // received data block
//  for (i <- 0 until params.n) {
//    when(punctureVecReg((o_cnt+i.U) % params.n.U)((o_cnt / params.n.U) % puncMatBitWidth) === 1.U) {
//      bufInterleaver(i.U) := io.in_hard(p_cnt - 1.U + puncIndicesReg((o_cnt+i.U) % params.n.U)(((o_cnt+i.U) / params.n.U) % puncMatBitWidth))
//      // o_cnt + i.U / params.n.U % puncMatBitWidth.U must reach to puncMatBitWidth.U-1
//    }.otherwise{
//      bufInterleaver(i.U) := 0.S
//    }
//  }
//  p_cnt := p_cnt + puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth)
//  o_cnt := o_cnt + params.n.U
//  when(p_cnt >= (params.O.U - puncListColSumReg((o_cnt/params.n.U) % puncMatBitWidth))) {
//    stateWire := sDone
//    p_cnt := 0.U
//  }
//  when((o_cnt >= (params.O - params.n).U) && ((((o_cnt+1.U) / params.n.U) % puncMatBitWidth) === (puncMatBitWidth -1.U))) {
//    o_cnt := 0.U
//  }
//}.elsewhen(io.stateIn =/= sDone && io.inReady === 1.U && io.isHead === 1.U){    // received header block
//  (0 until params.n).map(i => { bufInterleaver(i.U) := io.in_hard(o_cnt + i.U) })
//  o_cnt := o_cnt + params.n.U
//  when(o_cnt === (params.O - params.n).U) {
//    stateWire := sDone
//  }
//  when(o_cnt >= (params.O - params.n).U){
//    o_cnt := 0.U
//  }
//}
