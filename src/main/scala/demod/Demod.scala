package modem

import chisel3._
import chisel3.util._
import dsptools.numbers._
import chisel3.experimental.FixedPoint
import chisel3.experimental.withClock
import chisel3.util.Decoupled
import chisel3.util._
import dsptools.numbers._
import breeze.numerics.{atan, pow, sqrt, abs,floor}
import breeze.numerics.constants.{Pi}

import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem


// deinterleaver
class Deinterleaver[T <: Data, U <: Data](params: DemodulationParams[T,U]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(Vec(params.Ncbps,Bool())))
    val out = Decoupled(Vec(params.Ncbps,Bool()))
      })
     val rin = Reg(Vec(params.Ncbps, Bool()))
     val sInit = 0.U(1.W)
     val sDone = 1.U(1.W)
     val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  for (i <- 0 until params.Ncbps) {
              rin(i) := io.in.bits(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   //io.out.bits := rin
   val s = floor( (params.Nbpsc+1)/2 )
   val perm1 = Wire(Vec(params.Ncbps,Bool()))
   for (j <- 0 until params.Ncbps) {
      perm1 (s*floor(j/s)+(j+floor(16* j/params.Ncbps)) %s ) := rin(j)
    
    }
   for (i <- 0 until params.Ncbps) {
    
      io.out.bits( 16*i -(params.Ncbps -1)* floor(16* i/params.Ncbps) ) := perm1(i)
   }


}

// deinterleaver modify
class Deinterleaver1[T <: Data,U <: Data ](params: DemodulationParams[T,U]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(BitsBundle2(params)))
    val out = Decoupled(BitsBundle2(params))
      })
     val rin = Reg(Vec(params.Ncbps, Bool()))
     val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())

     val sInit = 0.U(1.W)
     val sDone = 1.U(1.W)
     val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
          reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	  for (i <- 0 until params.Ncbps) {
              rin(i) := io.in.bits.bits(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend

   //io.out.bits := rin
   val s = floor( (params.Nbpsc+1)/2 )
   val perm1 = Wire(Vec(params.Ncbps,Bool()))
   for (j <- 0 until params.Ncbps) {
      perm1 (s*floor(j/s)+(j+floor(16* j/params.Ncbps)) %s ) := rin(j)
    
    }
   for (i <- 0 until params.Ncbps) {
    
      io.out.bits.bits( 16*i -(params.Ncbps -1)* floor(16* i/params.Ncbps) ) := perm1(i)
   }


}

// deinterleaver modify soft 
class Deinterleaver1s[T <: Data,U <: Data](params: DemodulationParams[T,U]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(PacketBundle(params.Ncbps,params.protoIQ.cloneType )))

    //Flipped(Decoupled(BitsBundle2(params)))
    val out = Decoupled(PacketBundle(params.Ncbps,params.protoIQ.cloneType ))
    //Decoupled(BitsBundle2(params))
      })
     val rin = Reg(Vec(params.Ncbps,params.protoIQ.cloneType ))
     val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())

     val sInit = 0.U(1.W)
     val sDone = 1.U(1.W)
     val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
          reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	  for (i <- 0 until params.Ncbps) {
              rin(i) := io.in.bits.iq(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend

   //io.out.bits := rin
   val s = floor( (params.Nbpsc+1)/2 )
   val perm1 = Wire(Vec(params.Ncbps, params.protoIQ.cloneType))
   for (j <- 0 until params.Ncbps) {
      perm1 (s*floor(j/s)+(j+floor(16* j/params.Ncbps)) %s ) := rin(j)
    
    }
   for (i <- 0 until params.Ncbps) {
    
      io.out.bits.iq( 16*i -(params.Ncbps -1)* floor(16* i/params.Ncbps) ) := perm1(i)
   }


}


// QPSK DEMAPPER
class QPSKDemapper[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
        val out = Decoupled(Vec(params.Ncbps, Bool()))
     })
     val rin = Reg(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType ))

      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  for (i <- 0 until params.Ncbps/params.Nbpsc) {
              rin(i) := io.in.bits(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   val z2d = ConvertableTo[T].fromDouble(0.633)
   val z0 = Ring[T].zero
   for (i <- 0 until params.Ncbps/params.Nbpsc) {
        when (rin(i).real < z0 && rin(i).imag < z0){
	  io.out.bits(2*i) := false.B
          io.out.bits(2*i+1) := false.B
	  }.elsewhen(rin(i).real < z0 && rin(i).imag >= z0){
	   //io.out :=2.U
	   io.out.bits(2*i) := false.B
           io.out.bits(2*i+1) := true.B

	  }.elsewhen(rin(i).real >= z0 && rin(i).imag < z0){
	   //io.out := 1.U
	   io.out.bits(2*i) := true.B
           io.out.bits(2*i+1) := false.B

	   }.otherwise{
	   io.out.bits(2*i) := true.B
           io.out.bits(2*i+1) := true.B

	   }

       }
}

// QPSK DEMAPPER modify
  //val in = Flipped(Decoupled(DeserialPacketBundle(params)))
  //val out = Decoupled(BitsBundle1(params))
class QPSKDemapper1[T <: Data :Real:BinaryRepresentation,U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48,params.protoIQ.cloneType )))
        val out = Decoupled(BitsBundle2(params))
     })
     val rin = Reg(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType ))
     val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())

      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd

	  for (i <- 0 until params.Ncbps/params.Nbpsc) {
              rin(i) := io.in.bits.iq(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend
   val z2d = ConvertableTo[T].fromDouble(0.633)
   val z0 = Ring[T].zero
   for (i <- 0 until params.Ncbps/params.Nbpsc) {
        when (rin(i).real < z0 && rin(i).imag < z0){
	  io.out.bits.bits(2*i) := false.B
          io.out.bits.bits(2*i+1) := false.B
	  }.elsewhen(rin(i).real < z0 && rin(i).imag >= z0){
	   //io.out :=2.U
	   io.out.bits.bits(2*i) := false.B
           io.out.bits.bits(2*i+1) := true.B

	  }.elsewhen(rin(i).real >= z0 && rin(i).imag < z0){
	   //io.out := 1.U
	   io.out.bits.bits(2*i) := true.B
           io.out.bits.bits(2*i+1) := false.B

	   }.otherwise{
	   io.out.bits.bits(2*i) := true.B
           io.out.bits.bits(2*i+1) := true.B

	   }

       }
}
// QPSK SOFT demod
class QPSKDemapper1s[T <: Data :Real:BinaryRepresentation,U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48, params.protoIQ.cloneType)))
        val out = Decoupled(PacketBundle(params.Ncbps, params.protoIQ.cloneType))
     })
     val rin = Reg(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType ))
     val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())

      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd

	  for (i <- 0 until params.Ncbps/params.Nbpsc) {
              rin(i) := io.in.bits.iq(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend
   val zd = ConvertableTo[T].fromDouble(0.707)
   //channel estimation
   val h = ConvertableTo[T].fromDouble(1.00)
   val z0 = Ring[T].zero
   for (i <- 0 until params.Ncbps/params.Nbpsc) {
       io.out.bits.iq(2*i).real := zd*rin(i).real
       io.out.bits.iq(2*i).imag := z0
       io.out.bits.iq(2*i + 1).real := zd*rin(i).imag
       io.out.bits.iq(2*i + 1).imag := z0
       }
}

// 16QAM DEMAPPER pkt
// 
class QAM16Demapper1[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48, params.protoIQ.cloneType)))
	//Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
        val out = Decoupled(BitsBundle2(params))
	//Decoupled(Vec(params.Ncbps, Bool()))
     })
     val rin = Reg(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType ))
      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
     val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	  for (i <- 0 until params.Ncbps/params.Nbpsc) {
              rin(i) := io.in.bits.iq(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   val z2d = ConvertableTo[T].fromDouble(0.633)
   val z0 = Ring[T].zero
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend

   for (i <- 0 until params.Ncbps/params.Nbpsc) {
        when(rin(i).real > z0&& rin(i).real <= z2d && rin(i).imag > z0 && rin(i).imag <=z2d){
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := true.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real > z0&& rin(i).real <= z2d &&  rin(i).imag > z2d) {
           //io.out := 7.U
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := true.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := false.B

       }.elsewhen(rin(i).real > z2d && rin(i).imag > z0 && rin(i).imag <= z2d) {
           //io.out := 13.U
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := true.B

       }.elsewhen(rin(i).real > z2d && rin(i).imag > z2d) {
           //io.out := 5.U
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := false.B
       }.elsewhen(rin(i).real > z0&& rin(i).real <= z2d && rin(i).imag <= -z2d){
           //io.out := 3.U
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := true.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := false.B
       }.elsewhen(rin(i).real > z0&& rin(i).real <= z2d && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out := 11.U
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := true.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real >  z2d && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out := 9.U
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real >  z2d && rin(i).imag <= -z2d){
           //io.out := 1.U
           io.out.bits.bits(4*i) := true.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := false.B
       
       }.elsewhen(rin(i).real > -z2d&& rin(i).real <= z0 && rin(i).imag > z0 && rin(i).imag <= z2d){
           //io.out := 14.U
           io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := true.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := true.B

       }.elsewhen(rin(i).real > -z2d&& rin(i).real <= z0 && rin(i).imag > z2d){
           //io.out := 6.U
           io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := true.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := false.B

       }.elsewhen(rin(i).real <= -z2d && rin(i).imag > z0 && rin(i).imag <=z2d){
           //io.out := 12.U
           io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real <= -z2d && rin(i).imag > z2d){
           //io.out := 4.U
           io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := true.B
	   io.out.bits.bits(4*i +3 ) := false.B

       }.elsewhen(rin(i).real <= -z2d && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out := 8.U
           io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := true.B

       }.elsewhen(rin(i).real > -z2d && rin(i).real <= z0 && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out :=10.U
           io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real <= -z2d && rin(i).imag  <= -z2d){
           //io.out := 0.U
           io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := false.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := false.B

       }.otherwise{
           //io.out := 2.U
            io.out.bits.bits(4*i) := false.B
           io.out.bits.bits(4*i +1 ) := true.B
           io.out.bits.bits(4*i +2 ) := false.B
	   io.out.bits.bits(4*i +3 ) := false.B

       } 
     
       }
}
// 16QAM DEMAPPER pkt
// 
class QAM16Demapper1s[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48, params.protoIQ.cloneType)))
	//Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
        val out = Decoupled(PacketBundle(params.Ncbps, params.protoIQ.cloneType))
	//Decoupled(Vec(params.Ncbps, Bool()))
     })
     val rin = Reg(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType ))
      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
     val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	  for (i <- 0 until params.Ncbps/params.Nbpsc) {
              rin(i) := io.in.bits.iq(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   val z2d = ConvertableTo[T].fromDouble(0.633)
   val zd = ConvertableTo[T].fromDouble(0.316)
   val z0 = Ring[T].zero
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend
    
   for (i <- 0 until params.Ncbps/params.Nbpsc) {
           io.out.bits.iq(4*i).real := Mux(rin(i).real < -z2d , z2d*(rin(i).real + zd),Mux(rin(i).real > z2d, z2d*(rin(i).real - zd), rin(i).real*zd))
	   //rin(i).real
           io.out.bits.iq(4*i).imag := z0
	   io.out.bits.iq(4*i+1).real := Mux( rin(i).real>=0,(-rin(i).real + z2d)*zd, (rin(i).real + z2d)*zd )
	   io.out.bits.iq(4*i +1).imag := z0
	   io.out.bits.iq(4*i + 2).real := Mux(rin(i).imag < -z2d , z2d*(rin(i).imag + zd),Mux(rin(i).imag > z2d, z2d*(rin(i).imag - zd), rin(i).imag*zd))
           io.out.bits.iq(4*i + 2).imag := z0
	   io.out.bits.iq(4*i+3).real :=  Mux( rin(i).imag>=0,(-rin(i).imag + z2d)*zd, (rin(i).imag + z2d)*zd )
	   io.out.bits.iq(4*i +3).imag := z0
     
         }
  


}
// 16QAM DEMAPPER
// 
class QAM16Demapper[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
        val out = Decoupled(Vec(params.Ncbps, Bool()))
     })
     val rin = Reg(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType ))
      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  for (i <- 0 until params.Ncbps/params.Nbpsc) {
              rin(i) := io.in.bits(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   val z2d = ConvertableTo[T].fromDouble(0.633)
   val z0 = Ring[T].zero
   for (i <- 0 until params.Ncbps/params.Nbpsc) {
        when(rin(i).real > z0&& rin(i).real <= z2d && rin(i).imag > z0 && rin(i).imag <=z2d){
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := true.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real > z0&& rin(i).real <= z2d &&  rin(i).imag > z2d) {
           //io.out := 7.U
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := true.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := false.B

       }.elsewhen(rin(i).real > z2d && rin(i).imag > z0 && rin(i).imag <= z2d) {
           //io.out := 13.U
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := true.B

       }.elsewhen(rin(i).real > z2d && rin(i).imag > z2d) {
           //io.out := 5.U
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := false.B
       }.elsewhen(rin(i).real > z0&& rin(i).real <= z2d && rin(i).imag <= -z2d){
           //io.out := 3.U
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := true.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := false.B
       }.elsewhen(rin(i).real > z0&& rin(i).real <= z2d && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out := 11.U
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := true.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real >  z2d && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out := 9.U
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real >  z2d && rin(i).imag <= -z2d){
           //io.out := 1.U
           io.out.bits(4*i) := true.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := false.B
       
       }.elsewhen(rin(i).real > -z2d&& rin(i).real <= z0 && rin(i).imag > z0 && rin(i).imag <= z2d){
           //io.out := 14.U
           io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := true.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := true.B

       }.elsewhen(rin(i).real > -z2d&& rin(i).real <= z0 && rin(i).imag > z2d){
           //io.out := 6.U
           io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := true.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := false.B

       }.elsewhen(rin(i).real <= -z2d && rin(i).imag > z0 && rin(i).imag <=z2d){
           //io.out := 12.U
           io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real <= -z2d && rin(i).imag > z2d){
           //io.out := 4.U
           io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := true.B
	   io.out.bits(4*i +3 ) := false.B

       }.elsewhen(rin(i).real <= -z2d && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out := 8.U
           io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := true.B

       }.elsewhen(rin(i).real > -z2d && rin(i).real <= z0 && rin(i).imag > -z2d && rin(i).imag <= z0){
           //io.out :=10.U
           io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := true.B
       }.elsewhen(rin(i).real <= -z2d && rin(i).imag  <= -z2d){
           //io.out := 0.U
           io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := false.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := false.B

       }.otherwise{
           //io.out := 2.U
            io.out.bits(4*i) := false.B
           io.out.bits(4*i +1 ) := true.B
           io.out.bits(4*i +2 ) := false.B
	   io.out.bits(4*i +3 ) := false.B

       } 
     
       }
}

// BPSK DEMAPPER

class BPSKDemapper[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
        val out = Decoupled(Vec(params.Ncbps, Bool()))
     })
     val rin = Reg(Vec(params.Ncbps,params.protoIQ.cloneType ))
      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  for (i <- 0 until params.Ncbps) {
              rin(i) := io.in.bits(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   val z2d = ConvertableTo[T].fromDouble(0.633)
   val z0 = Ring[T].zero
   for (i <- 0 until params.Ncbps) {
     when(rin(i).real < z0){
	  io.out.bits(i) := false.B 
	}.otherwise {
	  io.out.bits(i) := true.B 
	}
   }
}

// BPSK DEMAPPER modify

class BPSKDemapper1[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
         val in  = Flipped(Decoupled(PacketBundle(48, params.protoIQ.cloneType)))
        val out = Decoupled(BitsBundle2(params))
        //val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
        //val out = Decoupled(Vec(params.Ncbps, Bool()))
     })
     val rin = Reg(Vec(params.Ncbps,params.protoIQ.cloneType ))
      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
      val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())
      
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	  for (i <- 0 until params.Ncbps) {
              rin(i) := io.in.bits.iq(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend
   val z2d = ConvertableTo[T].fromDouble(0.633)
   val z0 = Ring[T].zero
   for (i <- 0 until params.Ncbps) {
     when(rin(i).real < z0){
	  io.out.bits.bits(i) := false.B 
	}.otherwise {
	  io.out.bits.bits(i) := true.B 
	}
   }
}

class BPSKDemapper1s[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
   val io = IO(new Bundle {
         val in  = Flipped(Decoupled(PacketBundle(48, params.protoIQ.cloneType)))
        val out = Decoupled(PacketBundle(params.Ncbps, params.protoIQ.cloneType))
        //val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
        //val out = Decoupled(Vec(params.Ncbps, Bool()))
     })
     val rin = Reg(Vec(params.Ncbps,params.protoIQ.cloneType ))
      val sInit = 0.U(1.W)
      val sDone = 1.U(1.W)
      val state = RegInit(sInit)
      val reg_pktstart = Reg(Bool())
     val reg_pktend = Reg(Bool())
      
   
   
      when (state === sInit && io.in.fire()) {
          state := sDone
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	  for (i <- 0 until params.Ncbps) {
              rin(i) := io.in.bits.iq(i)  }
      }
      when (state === sDone && io.out.fire()) {
          state := sInit
      }
      
   //mapping.io.in := rin.asUInt
   io.in.ready := state === sInit 
   io.out.valid := state === sDone
   io.out.bits.pktStart := reg_pktstart
   io.out.bits.pktEnd := reg_pktend
   val zd = ConvertableTo[T].fromDouble(1.0)
   val z0 = Ring[T].zero
   for (i <- 0 until params.Ncbps) {
       
      io.out.bits.iq(i).real := rin(i).real *zd
      io.out.bits.iq(i).imag := z0
     }
}


// deserial input(64)
trait DemodParams[T <: Data] extends PacketBundleParams[T] {
//with BitsBundleParams[T] {
      val bitsWidth: Int
      val Ncbps: Int
      val Nbpsc: Int
}

object DemodParams {
  def apply[T <: Data](old_params: DemodParams[T]): DemodParams[T] = new DemodParams[T] {
    val protoIQ = old_params.protoIQ
    val width = old_params.width
    val bitsWidth = old_params.bitsWidth
    //val protoBits = old_params.protoBits
    val Ncbps = old_params.Ncbps
    val Nbpsc = old_params.Nbpsc
  }
}

// demod params modify
trait DemodulationParams[T <: Data, U <: Data] extends PacketBundleParams[T] with BitsBundleParams[U] {
      val bitsWidth: Int
      val Ncbps: Int
      val Nbpsc: Int
      val hsmod: Int
}

object DemodulationParams {
  def apply[T <: Data, U <: Data ](old_params: DemodulationParams[T,U]): DemodulationParams[T,U] = new DemodulationParams[T,U] {
    val protoIQ = old_params.protoIQ
    val width = old_params.width
    val bitsWidth = old_params.bitsWidth
    val protoBits = old_params.protoBits
    val Ncbps = old_params.Ncbps
    val Nbpsc = old_params.Nbpsc
    val hsmod = old_params.hsmod
  }
}

case class HardDemodParams(
  // width of Input and Output
  datawidth: Int,
  width: Int,
  bitsWidth: Int,
  hsmod: Int,
  Ncbps: Int,
  Nbpsc: Int
 ) extends DemodulationParams[FixedPoint, SInt] {
  val protoIQ = DspComplex(FixedPoint(datawidth.W, (datawidth-2).BP))
  val protoBits = SInt(2.W)
 
}

case class SoftDemodParams(
  // width of Input and Output
  datawidth: Int,
  width: Int,
  bitsWidth: Int,
  hsmod: Int,
  Ncbps: Int,
  Nbpsc: Int
 ) extends DemodulationParams[FixedPoint,FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(datawidth.W, (datawidth-2).BP))
  val protoBits = FixedPoint(datawidth.W, (datawidth-2).BP)  
}



case class FixedDemodParams(
  // width of Input and Output
  datawidth: Int,
  width: Int,
  bitsWidth: Int,
  Ncbps: Int,
  Nbpsc: Int
 ) extends DemodParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(datawidth.W, (datawidth-2).BP))
  //val protoBits = SInt(2.W)
 
}

class Demodulator[T <: Data:Real:BinaryRepresentation,U <: Data:Real:BinaryRepresentation](val params: DemodulationParams[T,U]) extends Module {
     val io = IO(new Bundle{
         val in = Flipped(Decoupled(DeserialPacketBundle(params)))
         val out = Decoupled(BitsBundle(params))     
        })
   if ( params.hsmod==1){
    if(params.Nbpsc ==1){
       val bpskhdemod = Module( new BPSKDemodulator1(params) )
       bpskhdemod.io.in.bits := io.in.bits
       bpskhdemod.io.in.valid := io.in.valid
       bpskhdemod.io.out.ready := io.out.ready
       io.out.bits := bpskhdemod.io.out.bits
       io.out.valid := bpskhdemod.io.out.valid
       io.in.ready := bpskhdemod.io.in.ready
    } else if(params.Nbpsc ==2){
       val qpskhdemod =  Module( new QPSKDemodulatorSer1(params) )
       qpskhdemod.io.in.bits := io.in.bits
       qpskhdemod.io.in.valid := io.in.valid
       qpskhdemod.io.out.ready := io.out.ready
       io.out.bits := qpskhdemod.io.out.bits
       io.out.valid := qpskhdemod.io.out.valid
       io.in.ready := qpskhdemod.io.in.ready
      } else{
        val qam16hdemod = Module( new QAM16DemodulatorSer1(params) )        
	qam16hdemod.io.in.bits := io.in.bits
        qam16hdemod.io.in.valid := io.in.valid
	qam16hdemod.io.out.ready := io.out.ready
        io.out.bits := qam16hdemod.io.out.bits
        io.out.valid := qam16hdemod.io.out.valid
        io.in.ready := qam16hdemod.io.in.ready    
      }
   
   } else{
     if(params.Nbpsc ==1){
       val bpsksdemod = Module( new BPSKDemodulator1s(params) )
       bpsksdemod.io.in.bits := io.in.bits
       bpsksdemod.io.in.valid := io.in.valid
       bpsksdemod.io.out.ready := io.out.ready
       for (i <- 0 until params.Ncbps / params.Nbpsc){
          io.out.bits.bits(i) := bpsksdemod.io.out.bits.iq(i).real }
       io.out.bits.pktStart := bpsksdemod.io.out.bits.pktStart
       io.out.bits.pktEnd := bpsksdemod.io.out.bits.pktEnd
       io.out.valid := bpsksdemod.io.out.valid
       io.in.ready := bpsksdemod.io.in.ready
     } else if(params.Nbpsc ==2){
       val qpsksdemod = Module( new QPSKDemodulatorSer1s(params) )
       qpsksdemod.io.in.bits := io.in.bits
       qpsksdemod.io.in.valid := io.in.valid
       qpsksdemod.io.out.ready := io.out.ready
       for (i <- 0 until params.Ncbps / params.Nbpsc){
          io.out.bits.bits(i) := qpsksdemod.io.out.bits.iq(i).real }
       io.out.bits.pktStart := qpsksdemod.io.out.bits.pktStart
       io.out.bits.pktEnd := qpsksdemod.io.out.bits.pktEnd
       io.out.valid := qpsksdemod.io.out.valid
       io.in.ready := qpsksdemod.io.in.ready
     } else{
       val qam16sdemod = Module( new QAM16DemodulatorSer1s(params) )
       qam16sdemod.io.in.bits := io.in.bits
       qam16sdemod.io.in.valid := io.in.valid
       qam16sdemod.io.out.ready := io.out.ready
       for (i <- 0 until params.Ncbps / params.Nbpsc){
          io.out.bits.bits(i) := qam16sdemod.io.out.bits.iq(i).real }
       io.out.bits.pktStart := qam16sdemod.io.out.bits.pktStart
       io.out.bits.pktEnd := qam16sdemod.io.out.bits.pktEnd
       io.out.valid := qam16sdemod.io.out.valid
       io.in.ready := qam16sdemod.io.in.ready
        
     }


   
   }   
  
   

}

class BitsBundle1[T<:Data,U <: Data](params: DemodulationParams[T,U] ) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
 
  val bits = Vec(params.Ncbps/params.Nbpsc, SInt(2.W) )
  override def cloneType: this.type = BitsBundle1(params).asInstanceOf[this.type]
}

object BitsBundle1  {
  def apply[T <: Data, U <: Data](params: DemodulationParams[T,U]): BitsBundle1[T,U] =
    new BitsBundle1(params)
}

class BitsBundle1b[T<:Data, U <: Data](params: DemodulationParams[T,U] ) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
 
  val bits = Vec(params.Ncbps/params.Nbpsc, Bool() )
  override def cloneType: this.type = BitsBundle1b(params).asInstanceOf[this.type]
}

object BitsBundle1b  {
  def apply[T <: Data, U <: Data](params: DemodulationParams[T,U]): BitsBundle1b[T,U] =
    new BitsBundle1b(params)
}

class BitsBundle1bt[T<:Data](params: ModFFTParams[T] ) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
 
  val bits = Vec(params.Ncbps/params.Nbpsc, Bool() )
  override def cloneType: this.type = BitsBundle1bt(params).asInstanceOf[this.type]
}

object BitsBundle1bt  {
  def apply[T <: Data](params: ModFFTParams[T]): BitsBundle1bt[T] =
    new BitsBundle1bt(params)
}


class BitsBundle1s[T<:Data, U <: Data](params: DemodulationParams[T,U] ) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
 
  val bits = Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType  )
  override def cloneType: this.type = BitsBundle1b(params).asInstanceOf[this.type]
}

object BitsBundle1s  {
  def apply[T <: Data, U <: Data](params: DemodulationParams[T,U]): BitsBundle1s[T,U] =
    new BitsBundle1s(params)
}


class BitsBundle2[T<:Data, U <: Data](params: DemodulationParams[T,U] ) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
 
  val bits = Vec(params.Ncbps, Bool() )
  override def cloneType: this.type = BitsBundle2(params).asInstanceOf[this.type]
}

object BitsBundle2  {
  def apply[T <: Data, U <: Data](params: DemodulationParams[T,U]): BitsBundle2[T,U] =
    new BitsBundle2(params)
}

class BitsBundle2t[T<:Data](params: ModFFTParams[T] ) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
 
  val bits = Vec(params.Ncbps, Bool() )
  override def cloneType: this.type = BitsBundle2t(params).asInstanceOf[this.type]
}

object BitsBundle2t  {
  def apply[T <: Data](params: ModFFTParams[T]): BitsBundle2t[T] =
    new BitsBundle2t(params)
}


class BitsBundle2s[T<:Data, U <: Data](params: DemodulationParams[T,U] ) extends Bundle {
  val pktStart: Bool = Bool()
  val pktEnd: Bool = Bool()
 
  val bits = Vec(params.Ncbps, params.protoIQ.cloneType )
  override def cloneType: this.type = BitsBundle2s(params).asInstanceOf[this.type]
}

object BitsBundle2s  {
  def apply[T <: Data, U <: Data](params: DemodulationParams[T,U]): BitsBundle2s[T,U] =
    new BitsBundle2s(params)
}




class DEMODIO[T <: Data, U <: Data](params: DemodulationParams[T,U]) extends Bundle {
  val in = Flipped(Decoupled(DeserialPacketBundle(params)))
  val out = Decoupled(BitsBundle1(params))

  override def cloneType: this.type = DEMODIO(params).asInstanceOf[this.type]
}
object DEMODIO {
  def apply[T <: Data, U <: Data](params: DemodulationParams[T,U]): DEMODIO[T,U] = new DEMODIO(params)
}
class DEMODIOS[T <: Data, U <: Data](params: DemodulationParams[T,U]) extends Bundle {
  val in = Flipped(Decoupled(DeserialPacketBundle(params)))
  val out = Decoupled(PacketBundle(48,params.protoIQ.cloneType ))

  override def cloneType: this.type = DEMODIOS(params).asInstanceOf[this.type]
}
object DEMODIOS {
  def apply[T <: Data, U <: Data](params: DemodulationParams[T,U]): DEMODIOS[T,U] = new DEMODIOS(params)
}


// BPSK DEMOUDLATOR
class BPSKDemodulator[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
  val io = IO(DEMODIO(params))
  val demapping = Module( new BPSKDemapper(params) )
  val deinterleaving =  Module( new Deinterleaver(params) )
  //for (i <- 0 until 48){
     //demapping.io.in.bits(i) := io.in.bits.iq(i)
  //}

    for (i <- 0 until 5){
     demapping.io.in.bits(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demapping.io.in.bits(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demapping.io.in.bits(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demapping.io.in.bits(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demapping.io.in.bits(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
     demapping.io.in.bits(i) := io.in.bits.iq(i-21)
  }

 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.out.ready := io.out.ready
 for (i <- 0 until 48) {
  io.out.bits.bits(i) := Mux(deinterleaving.io.out.bits(i), 1.S, -1.S)
 }
 //io.out.bits := deinterleaving.io.out.bits
 io.out.bits.pktStart :=io.in.bits.pktStart
 io.out.bits.pktEnd :=io.in.bits.pktEnd

 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}

// BPSK DEMOUDLATOR modify
class BPSKDemodulator1[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
  val io = IO(DEMODIO(params))
  val demapping = Module( new BPSKDemapper1(params) )
  val deinterleaving =  Module( new Deinterleaver1(params) )
  //for (i <- 0 until 48){
     //demapping.io.in.bits(i) := io.in.bits.iq(i)
  //}

    for (i <- 0 until 5){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i-21)
  }

 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready
 demapping.io.in.bits.pktStart := io.in.bits.pktStart
  demapping.io.in.bits.pktEnd := io.in.bits.pktEnd

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.in.bits.pktStart := demapping.io.out.bits.pktStart
 deinterleaving.io.in.bits.pktEnd := demapping.io.out.bits.pktEnd


 deinterleaving.io.out.ready := io.out.ready
 for (i <- 0 until 48) {
  io.out.bits.bits(i) := Mux(deinterleaving.io.out.bits.bits(i), 1.S, -1.S)
 }
 //io.out.bits := deinterleaving.io.out.bits
 io.out.bits.pktStart :=deinterleaving.io.out.bits.pktStart
 io.out.bits.pktEnd :=deinterleaving.io.out.bits.pktEnd

 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}


// BPSK DEMOUDLATOR modify soft
class BPSKDemodulator1s[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
  val io = IO(DEMODIOS(params))
  val demapping = Module( new BPSKDemapper1s(params) )
  val deinterleaving =  Module( new Deinterleaver1s(params) )
  //for (i <- 0 until 48){
     //demapping.io.in.bits(i) := io.in.bits.iq(i)
  //}

    for (i <- 0 until 5){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
     demapping.io.in.bits.iq(i) := io.in.bits.iq(i-21)
  }

 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready
 demapping.io.in.bits.pktStart := io.in.bits.pktStart
  demapping.io.in.bits.pktEnd := io.in.bits.pktEnd

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.in.bits.pktStart := demapping.io.out.bits.pktStart
 deinterleaving.io.in.bits.pktEnd := demapping.io.out.bits.pktEnd


 deinterleaving.io.out.ready := io.out.ready
 //for (i <- 0 until 48) {
  //io.out.bits.bits(i) := Mux(deinterleaving.io.out.bits.bits(i), 1.S, -1.S)
 //}
 io.out.bits := deinterleaving.io.out.bits
 //io.out.bits.pktStart :=deinterleaving.io.out.bits.pktStart
 //io.out.bits.pktEnd :=deinterleaving.io.out.bits.pktEnd

 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}


class QPSKDemodulatorSer[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {

  val io = IO(DEMODIO(params))
  val demod = Module( new QPSKDemodulator (params) )
  val ser =  Module( new Serilizerm(params) )
  for (i <- 0 until 5){
     demod.io.in.bits(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demod.io.in.bits(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demod.io.in.bits(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demod.io.in.bits(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demod.io.in.bits(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
    demod.io.in.bits(i) := io.in.bits.iq(i-21)
  }
  //for (i <- 0 until 48){
    // demod.io.in.bits(i) := io.in.bits.iq(i)
  //}

  demod.io.in.valid := io.in.valid
  demod.io.out.ready := ser.io.in.ready

  ser.io.in.valid := demod.io.out.valid
  ser.io.in.bits := demod.io.out.bits
  ser.io.out.ready := io.out.ready
  for (i <- 0 until 48) {
      io.out.bits.bits(i) := Mux(ser.io.out.bits(i), 1.S, -1.S)
  }
  //io.out.bits := ser.io.out.bits
  io.out.valid := ser.io.out.valid
  io.in.ready := demod.io.in.ready
  io.out.bits.pktStart :=io.in.bits.pktStart
  io.out.bits.pktEnd :=io.in.bits.pktEnd


}

class QAM16DemodulatorSer1[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {

  val io = IO(DEMODIO(params))
  val demod = Module( new QAM16Demodulator1 (params) )
  val ser =  Module( new Serilizerm1(params) )
  for (i <- 0 until 5){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
    demod.io.in.bits.iq(i) := io.in.bits.iq(i-21)
  }
  //for (i <- 0 until 48){
    // demod.io.in.bits(i) := io.in.bits.iq(i)
  //}

  demod.io.in.valid := io.in.valid
  demod.io.in.bits.pktStart := io.in.bits.pktStart
  demod.io.in.bits.pktEnd := io.in.bits.pktEnd
  demod.io.out.ready := ser.io.in.ready

  ser.io.in.valid := demod.io.out.valid
  ser.io.in.bits.pktStart := demod.io.out.bits.pktStart
  ser.io.in.bits.pktEnd := demod.io.out.bits.pktEnd

  ser.io.in.bits := demod.io.out.bits
  ser.io.out.ready := io.out.ready
  for (i <- 0 until 48) {
      io.out.bits.bits(i) := Mux(ser.io.out.bits.bits(i), 1.S, -1.S)
  }
  io.out.bits.pktStart := ser.io.out.bits.pktStart
  io.out.bits.pktEnd := ser.io.out.bits.pktEnd

  io.out.valid := ser.io.out.valid
  io.in.ready := demod.io.in.ready
  
}


class QPSKDemodulatorSer1[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {

  val io = IO(DEMODIO(params))
  val demod = Module( new QPSKDemodulator1 (params) )
  val ser =  Module( new Serilizerm1(params) )
  for (i <- 0 until 5){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
    demod.io.in.bits.iq(i) := io.in.bits.iq(i-21)
  }
  //for (i <- 0 until 48){
    // demod.io.in.bits(i) := io.in.bits.iq(i)
  //}

  demod.io.in.valid := io.in.valid
  demod.io.in.bits.pktStart := io.in.bits.pktStart
  demod.io.in.bits.pktEnd := io.in.bits.pktEnd
  demod.io.out.ready := ser.io.in.ready

  ser.io.in.valid := demod.io.out.valid
  ser.io.in.bits.pktStart := demod.io.out.bits.pktStart
  ser.io.in.bits.pktEnd := demod.io.out.bits.pktEnd

  ser.io.in.bits := demod.io.out.bits
  ser.io.out.ready := io.out.ready
  for (i <- 0 until 48) {
      io.out.bits.bits(i) := Mux(ser.io.out.bits.bits(i), 1.S, -1.S)
  }
  io.out.bits.pktStart := ser.io.out.bits.pktStart
  io.out.bits.pktEnd := ser.io.out.bits.pktEnd

  io.out.valid := ser.io.out.valid
  io.in.ready := demod.io.in.ready
  
}


class QAM16DemodulatorSer1s[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {

  val io = IO(DEMODIOS(params))
  val demod = Module( new QAM16Demodulator1s (params) )
  val ser =  Module( new Serilizerms1(params) )
  for (i <- 0 until 5){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
    demod.io.in.bits.iq(i) := io.in.bits.iq(i-21)
  }
  //for (i <- 0 until 48){
    // demod.io.in.bits(i) := io.in.bits.iq(i)
  //}

  demod.io.in.valid := io.in.valid
  demod.io.in.bits.pktStart := io.in.bits.pktStart
  demod.io.in.bits.pktEnd := io.in.bits.pktEnd
  demod.io.out.ready := ser.io.in.ready

  ser.io.in.valid := demod.io.out.valid
  ser.io.in.bits.pktStart := demod.io.out.bits.pktStart
  ser.io.in.bits.pktEnd := demod.io.out.bits.pktEnd

  ser.io.in.bits := demod.io.out.bits
  ser.io.out.ready := io.out.ready
  //for (i <- 0 until 48) {
      //io.out.bits.bits(i) := Mux(ser.io.out.bits.bits(i), 1.S, -1.S)
 // }
  io.out.bits := ser.io.out.bits
  //io.out.bits.pktEnd := ser.io.out.bits.pktEnd

  io.out.valid := ser.io.out.valid
  io.in.ready := demod.io.in.ready
  
}


class QPSKDemodulatorSer1s[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {

  val io = IO(DEMODIOS(params))
  val demod = Module( new QPSKDemodulator1s (params) )
  val ser =  Module( new Serilizerms1(params) )
  for (i <- 0 until 5){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demod.io.in.bits.iq(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
    demod.io.in.bits.iq(i) := io.in.bits.iq(i-21)
  }
  //for (i <- 0 until 48){
    // demod.io.in.bits(i) := io.in.bits.iq(i)
  //}

  demod.io.in.valid := io.in.valid
  demod.io.in.bits.pktStart := io.in.bits.pktStart
  demod.io.in.bits.pktEnd := io.in.bits.pktEnd
  demod.io.out.ready := ser.io.in.ready

  ser.io.in.valid := demod.io.out.valid
  ser.io.in.bits.pktStart := demod.io.out.bits.pktStart
  ser.io.in.bits.pktEnd := demod.io.out.bits.pktEnd

  ser.io.in.bits := demod.io.out.bits
  ser.io.out.ready := io.out.ready
  //for (i <- 0 until 48) {
      //io.out.bits.bits(i) := Mux(ser.io.out.bits.bits(i), 1.S, -1.S)
 // }
  io.out.bits := ser.io.out.bits
  //io.out.bits.pktEnd := ser.io.out.bits.pktEnd

  io.out.valid := ser.io.out.valid
  io.in.ready := demod.io.in.ready
  
}



class QPSKDemodulator[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
      val io = IO(new Bundle {
       val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
       val out = Decoupled(Vec(params.Ncbps, Bool()))
  })
 val demapping = Module( new QPSKDemapper(params) )
 val deinterleaving =  Module( new Deinterleaver(params) )
 demapping.io.in.bits := io.in.bits
 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.out.ready := io.out.ready
 io.out.bits := deinterleaving.io.out.bits
 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}
class QPSKDemodulator1[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
      val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48,params.protoIQ.cloneType )))
        val out = Decoupled(BitsBundle2(params))
       //val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
       //val out = Decoupled(Vec(params.Ncbps, Bool()))
  })
 val demapping = Module( new QPSKDemapper1(params) )
 val deinterleaving =  Module( new Deinterleaver1(params) )
 demapping.io.in.bits := io.in.bits
 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.out.ready := io.out.ready
 io.out.bits := deinterleaving.io.out.bits
 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}

class QAM16Demodulator1[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
      val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48,params.protoIQ.cloneType )))
        val out = Decoupled(BitsBundle2(params))
       //val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
       //val out = Decoupled(Vec(params.Ncbps, Bool()))
  })
 val demapping = Module( new QAM16Demapper1(params) )
 val deinterleaving =  Module( new Deinterleaver1(params) )
 demapping.io.in.bits := io.in.bits
 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.out.ready := io.out.ready
 io.out.bits := deinterleaving.io.out.bits
 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}


class QPSKDemodulator1s[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
      val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48,params.protoIQ.cloneType )))
        val out = Decoupled(PacketBundle(params.Ncbps,params.protoIQ.cloneType ))
       //val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
       //val out = Decoupled(Vec(params.Ncbps, Bool()))
  })
 val demapping = Module( new QPSKDemapper1s(params) )
 val deinterleaving =  Module( new Deinterleaver1s(params) )
 demapping.io.in.bits := io.in.bits
 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.out.ready := io.out.ready
 io.out.bits := deinterleaving.io.out.bits
 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}

class QAM16Demodulator1s[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
      val io = IO(new Bundle {
        val in  = Flipped(Decoupled(PacketBundle(48,params.protoIQ.cloneType )))
        val out = Decoupled(PacketBundle(params.Ncbps,params.protoIQ.cloneType ))
       //val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
       //val out = Decoupled(Vec(params.Ncbps, Bool()))
  })
 val demapping = Module( new QAM16Demapper1s(params) )
 val deinterleaving =  Module( new Deinterleaver1s(params) )
 demapping.io.in.bits := io.in.bits
 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.out.ready := io.out.ready
 io.out.bits := deinterleaving.io.out.bits
 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}



// QAM demodulator
class QAM16Demodulator[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {
      val io = IO(new Bundle {
       val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType)))
       val out = Decoupled(Vec(params.Ncbps, Bool()))
  })
 val demapping = Module( new QAM16Demapper(params) )
 val deinterleaving =  Module( new Deinterleaver(params) )
 demapping.io.in.bits := io.in.bits
 demapping.io.in.valid := io.in.valid
 demapping.io.out.ready := deinterleaving.io.in.ready

 deinterleaving.io.in.valid := demapping.io.out.valid
 deinterleaving.io.in.bits := demapping.io.out.bits
 deinterleaving.io.out.ready := io.out.ready
 io.out.bits := deinterleaving.io.out.bits
 io.out.valid := deinterleaving.io.out.valid
 io.in.ready := demapping.io.in.ready
 
}

class QAM16DemodulatorSer[T <: Data :Real:BinaryRepresentation, U <: Data](val params: DemodulationParams[T,U]) extends Module {

  val io = IO(DEMODIO(params))
  val demod = Module( new QAM16Demodulator (params) )
  val ser =  Module( new Serilizerm(params) )
  for (i <- 0 until 5){
     demod.io.in.bits(i) := io.in.bits.iq(i+38)
  }
  for (i <- 5 until 18){
     demod.io.in.bits(i) := io.in.bits.iq(i+39)
  }
  for (i <- 18 until 24){
     demod.io.in.bits(i) := io.in.bits.iq(i+40)
  }
  for (i <- 24 until 30){
     demod.io.in.bits(i) := io.in.bits.iq(i-23)
  }
  for (i <- 30 until 43){
     demod.io.in.bits(i) := io.in.bits.iq(i-22)
  }
  for (i <- 43 until 48){
     demod.io.in.bits(i) := io.in.bits.iq(i-21)
  }
  demod.io.in.valid := io.in.valid
  demod.io.out.ready := ser.io.in.ready

  ser.io.in.valid := demod.io.out.valid
  ser.io.in.bits := demod.io.out.bits
  ser.io.out.ready := io.out.ready
  for (i <- 0 until 48) {
      io.out.bits.bits(i) := Mux(ser.io.out.bits(i), 1.S, -1.S)
  }
  //io.out.bits := ser.io.out.bits
  io.out.valid := ser.io.out.valid
  io.in.ready := demod.io.in.ready
  //val reg_pktStart= Reg(Bool())
  //val reg_pktEnd= Reg(Bool())

  io.out.bits.pktStart :=io.in.bits.pktStart
  io.out.bits.pktEnd :=io.in.bits.pktEnd


}


// serilizer modify 
class Serilizerm[T <: Data, U <: Data](params: DemodulationParams[T,U]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(Vec(params.Ncbps,Bool())))
    val out = Decoupled(Vec(params.Ncbps/params.Nbpsc,Bool()))
    //val cnt = Output(UInt(8.W))
    //val sat = Output(UInt(2.W))
  })
  val pout = Reg(Vec(params.Ncbps,Bool()))
  //val cnt = Reg(UInt(8.W))
  val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)
  //io.out(0):= RegNext(io.in)
  val ser = Reg(Vec(params.Ncbps/params.Nbpsc,Bool()))
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  pout := io.in.bits
	  	                      
	 
  }
  when (state === sWork ) {
         val iterNext = iter + 1.U
         iter := iterNext
	 for ( i <- 0 until params.Ncbps/params.Nbpsc ) {
           ser(i) := pout( (params.Ncbps/params.Nbpsc) * (params.Nbpsc - iter -1) + i )
         }	
         when (iterNext >= (params.Nbpsc).U) {
            state := sDone}
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }
  io.in.ready := state === sInit 
  io.out.valid :=  (state === sWork && iter >= 1.U) || state === sDone
  io.out.bits := ser


}
// serilizer modify packetstart/end
class Serilizerm1[T <: Data, U <: Data](params: DemodulationParams[T,U]) extends Module {
    val io = IO(new Bundle {
    val in = Flipped( Decoupled(BitsBundle2(params)))
    val out =  Decoupled(BitsBundle1b(params))

    //val in  = Flipped(Decoupled(Vec(params.Ncbps,Bool())))
    //val out = Decoupled(Vec(params.Ncbps/params.Nbpsc,Bool()))
    //val cnt = Output(UInt(8.W))
    //val sat = Output(UInt(2.W))
  })
  val pout = Reg(Vec(params.Ncbps,Bool()))
  val reg_pktstart = Reg(Bool())
  val reg_pktend = Reg(Bool())
  //val cnt = Reg(UInt(8.W))
  val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)
  //io.out(0):= RegNext(io.in)
  val ser = Reg(Vec(params.Ncbps/params.Nbpsc,Bool()))
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  reg_pktstart := io.in.bits.pktStart
	  reg_pktend := io.in.bits.pktEnd
	  pout := io.in.bits.bits
	  	                      
	 
  }
  when (state === sWork ) {
         val iterNext = iter + 1.U
         iter := iterNext
	 for ( i <- 0 until params.Ncbps/params.Nbpsc ) {
           ser(i) := pout( (params.Ncbps/params.Nbpsc) * (params.Nbpsc - iter -1) + i )
         }	
         when (iterNext >= (params.Nbpsc).U) {
            state := sDone}
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }
  io.in.ready := state === sInit 
  io.out.valid :=  (state === sWork && iter >= 1.U) || state === sDone
  io.out.bits.pktStart := reg_pktstart
  io.out.bits.pktEnd := reg_pktend

  io.out.bits.bits := ser


}

// serilizer modify packetstart/end
class Serilizerms1[T <: Data, U <: Data](params: DemodulationParams[T,U]) extends Module {
    val io = IO(new Bundle {
    //val in = Flipped( Decoupled(BitsBundle2(params)))
    //val out =  Decoupled(BitsBundle1b(params))
    val in  = Flipped(Decoupled(PacketBundle(params.Ncbps, params.protoIQ.cloneType)))
    val out = Decoupled(PacketBundle(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))

    
    //val cnt = Output(UInt(8.W))
    //val sat = Output(UInt(2.W))
  })
  val pout = Reg(Vec(params.Ncbps,params.protoIQ.cloneType))
  val reg_pktstart = Reg(Bool())
  val reg_pktend = Reg(Bool())
  //val cnt = Reg(UInt(8.W))
  val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)
  //io.out(0):= RegNext(io.in)
  val ser = Reg(Vec(params.Ncbps/params.Nbpsc,params.protoIQ.cloneType))
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  reg_pktstart := io.in.bits.pktStart
	  reg_pktend := io.in.bits.pktEnd
	  pout := io.in.bits.iq
	  	                      
	 
  }
  when (state === sWork ) {
         val iterNext = iter + 1.U
         iter := iterNext
	 for ( i <- 0 until params.Ncbps/params.Nbpsc ) {
           ser(i) := pout( (params.Ncbps/params.Nbpsc) * (params.Nbpsc - iter -1) + i )
         }	
         when (iterNext >= (params.Nbpsc).U) {
            state := sDone}
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }
  io.in.ready := state === sInit 
  io.out.valid :=  (state === sWork && iter >= 1.U) || state === sDone
  io.out.bits.pktStart := reg_pktstart
  io.out.bits.pktEnd := reg_pktend

  io.out.bits.iq := ser


}





//-- Interleaver modify(serial input)
class Deserilizer[T <: Data, U <: Data](params: DemodulationParams[T,U]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(Bool()))
    val out = Decoupled(Vec(params.Ncbps/params.Nbpsc,Bool()))
    val cnt = Output(UInt(8.W))
    val sat = Output(UInt(2.W))
  })
  //val s = floor( (params.Nbpsc+1)/2 )
  val pout = Reg(Vec(params.Ncbps/params.Nbpsc,Bool()))
  val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)
  
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  pout(0) := io.in.bits
	  
                    
	 
  }
  when (state === sWork && io.in.fire()) {
         val iterNext = iter + 1.U
         iter := iterNext
         pout(0) := io.in.bits

        for (j <- 1 until params.Ncbps/params.Nbpsc){
          pout(j) := pout(j-1)
         }  
	
         when (iterNext >= (params.Ncbps/params.Nbpsc-1).U) {
            state := sDone
                      }
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }

  io.in.ready := state === sInit || state === sWork
  io.out.valid := state === sDone
  io.out.bits := pout
  io.cnt := iter
  io.sat := state
  //val perm1 = Wire(Vec(params.Ncbps,Bool()))
  io.out.bits := pout

   
}


// serilizer + deserilizer

