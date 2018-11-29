//package cordic
package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.experimental.withClock
import chisel3.util.Decoupled
import chisel3.util._
//import chisel3.core.data
import dsptools.numbers._
import breeze.numerics.{atan, pow, sqrt, abs,floor}
import breeze.numerics.constants.{Pi}

import dsptools.numbers._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem



trait P2sParams[T <: Data] {
  val gen: T
  val n: Int
}

trait InterleavParams[T <: Data] {
  val gen: T
  val Ncbps: Int
  val Nbpsc: Int
}

case class UintInterleavParams(
    mwidth: Int,
    Ncbps: Int,
    Nbpsc: Int
) extends InterleavParams[UInt] {
    val gen = UInt(mwidth.W)
}

//trait IQBundleParams[T <: Data] {
  //val protoIQ: DspComplex[T]
//}

//object IQBundleParams {
 // def apply[T <: Data](proto: DspComplex[T]): IQBundleParams[T] = new IQBundleParams[T] { val protoIQ = proto }
//}

trait ModParams[T <: Data] extends IQBundleParams[T] {
  val n: Int
}
trait BPSKModParams[T <: Data] extends IQBundleParams[T] {
  val Ncbps: Int
  val Nbpsc: Int
}
case class FixedModParams(
  n: Int,
  // width of Input and Output
  dataWidth: Int
) extends ModParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, (dataWidth - 2 ).BP))
}
case class FixedBPSKModParams(
  Ncbps: Int,
  Nbpsc: Int,
  // width of Input and Output
  dataWidth: Int
) extends BPSKModParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, (dataWidth - 2 ).BP))
}

trait MapParams[T <: Data]{
  val n: Int
  val protoIQ: T
}
case class Uintp2sParams(
    mwidth: Int,
    n: Int
) extends P2sParams[UInt] {
    val gen = UInt(mwidth.W)
}

trait DemapParams[T <: Data] {
  val protoiq: T
  val n: Int
}

case class FixedDemapParams(
    iqwidth: Int,
    n: Int
) extends DemapParams[FixedPoint] {
    val protoiq = FixedPoint(iqwidth.W, (iqwidth-2).BP)
}

case class FixedMapParams(
    iqwidth: Int,
    n: Int
) extends MapParams[FixedPoint] {
    val protoIQ = FixedPoint(iqwidth.W, (iqwidth-2).BP)
}

class ShiftRegisterIO[T <: Data](params: P2sParams[T] ) extends Bundle {
    require (params.n >= 0, "Shift register must have non-negative shift")
    
    //val in = Input(params.gen.cloneType)
    val sin = Flipped(Decoupled(params.gen.cloneType))
    //val out = Output(Vec(params.n + 1, params.gen.cloneType))// + 1 because in is included in out
    val pout = Vec(params.n, Decoupled(params.gen.cloneType))
    override def cloneType: this.type = ShiftRegisterIO(params).asInstanceOf[this.type]
}
object ShiftRegisterIO {
  def apply[T <: Data](params: P2sParams[T]): ShiftRegisterIO[T] =
    new ShiftRegisterIO(params)
}

class DemapperIO[T <: Data](params: DemapParams[T] ) extends Bundle {
    require (params.n >= 0, "Shift register must have non-negative shift")
    
    val ini = Input(params.protoiq.cloneType)
    val inq = Input(params.protoiq.cloneType)
    
    val out = Output(UInt(params.n.W))
    //val pout = Vec(params.n, Decoupled(params.gen.cloneType))
    override def cloneType: this.type = DemapperIO(params).asInstanceOf[this.type]
}
object DemapperIO {
  def apply[T <: Data](params: DemapParams[T]): DemapperIO[T] =
    new DemapperIO(params)
}

class MapperIO[T <: Data](params: MapParams[T]) extends Bundle {
    require (params.n >= 0, "Shift register must have non-negative shift")
    val in = Input(UInt(params.n.W))

    val out_i = Output(params.protoIQ.cloneType)
    val out_q = Output(params.protoIQ.cloneType)
    
    
    override def cloneType: this.type = MapperIO(params).asInstanceOf[this.type]
}
object MapperIO {
  def apply[T <: Data](params: MapParams[T]): MapperIO[T] =
    new MapperIO(params)
}
class ModBundle[T <: Data](params: ModParams[T]) extends Bundle {
  val opiq: DspComplex[T] = params.protoIQ.cloneType
  //val opq: T = params.protoIQ.cloneType
  
  override def cloneType: this.type = ModBundle(params).asInstanceOf[this.type]
}
object ModBundle {
  def apply[T <: Data](params: ModParams[T]): ModBundle[T] = new ModBundle(params)
}

class ModFFTBundle[T <: Data](params:  ModFFTParams[T]) extends Bundle {
   val pktStart: Bool = Bool()
   val pktEnd: Bool = Bool()
   val fec: Bool = Bool()

  override def cloneType: this.type = ModFFTBundle(params).asInstanceOf[this.type]
}
object ModFFTBundle {
  def apply[T <: Data](params:  ModFFTParams[T]): ModFFTBundle[T] = new ModFFTBundle(params)
}

class ModulatorIO[T <: Data](params: ModParams[T]) extends Bundle {
    val in = Flipped(Decoupled(Bool()))
    val par = Output(UInt(params.n.W))
    val sta = Output(UInt(2.W))
    val cnt = Output(UInt(5.W))
    val out = Decoupled(ModBundle(params))
    
    
    override def cloneType: this.type = ModulatorIO(params).asInstanceOf[this.type]
}

object ModulatorIO {
  def apply[T <: Data](params: ModParams[T]): ModulatorIO[T] =
    new ModulatorIO(params)
}



//---Interleaver

class Interleav[T <: Data](params: InterleavParams[T]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Input(Vec(params.Ncbps,Bool()))
    val out = Output(Vec(params.Ncbps,Bool()))
  })
  val s = floor( (params.Nbpsc+1)/2 )
  val perm1 = Reg(Vec(params.Ncbps,Bool()))
  //for (k <- 0 until params.Ncbps) {
    
    //perm1(floor(params.Ncbps/16) * (k % 16) + floor(k/16)):= io.in(k)
  //}
 for (i <- 0 until params.Ncbps) {
    val k= floor(params.Ncbps/16) * (i % 16) + floor(i/16)
    io.out( s*floor(k/s)+(k+params.Ncbps-floor(16* (k/params.Ncbps)) ) %s ):= RegNext(io.in(i))
  }

}

//-- Interleaver modify(serial input)
class Interleaver[T <: Data](params: ModFFTParams[T]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(Bool()))
    val out = Decoupled(Vec(params.Ncbps,Bool()))
    val cnt = Output(UInt(8.W))
    val sat = Output(UInt(2.W))
  })
  val s = floor( (params.Nbpsc+1)/2 )
  val pout = Reg(Vec(params.Ncbps,Bool()))
  //val cnt = Reg(UInt(8.W))
  val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)
  //io.out(0):= RegNext(io.in)
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  pout(0) := io.in.bits
	  
                    
	 
  }
  when (state === sWork && io.in.fire()) {
         val iterNext = iter + 1.U
         iter := iterNext
         pout(0) := io.in.bits

        for (j <- 1 until params.Ncbps){
          pout(j) := pout(j-1)
         }  
	
         when (iterNext >= (params.Ncbps-1).U) {
            state := sDone
                      }
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }

  io.in.ready := state === sInit || state === sWork
  io.out.valid := state === sDone
  //io.out.bits := pout
  io.cnt := iter
  io.sat := state
  val perm1 = Wire(Vec(params.Ncbps,Bool()))

   for (k <- 0 until params.Ncbps) {
    
     perm1(floor(params.Ncbps/16) * (k % 16) + floor(k/16)):= pout(k)
   }
   for (i <- 0 until params.Ncbps) {
       io.out.bits( s*floor(i/s)+(i+params.Ncbps-floor(16* (i/params.Ncbps)) ) %s ):= perm1(i)  }

}
 

//-- Interleaver modify2(48bits input: Nbpsc = 1)
class Interleaverds1[T <: Data:Real:BinaryRepresentation](params: ModFFTParams[T]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,Bool())))
    val out = Decoupled(Vec(params.Ncbps,Bool()))
   // val cnt = Output(UInt(8.W))
    val sat = Output(UInt(2.W))
  })
  val s = floor( (params.Nbpsc+1)/2 )
  val pout = Reg(Vec(params.Ncbps,Bool()))
  //val cnt = Reg(UInt(8.W))
  //val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(1.W)
 // val sWork = 1.U(2.W)
  val sDone = 1.U(1.W)
  val state = RegInit(sInit)
  //io.out(0):= RegNext(io.in)
 
   
  when (state === sInit && io.in.fire()) {
          state := sDone
          //iter := 0.U
	  pout := io.in.bits                    
	 
  }
    when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }

  io.in.ready := state === sInit
  io.out.valid := state === sDone
  io.out.bits := pout
  //io.cnt := iter
  io.sat := state
 // val perm1 = Wire(Vec(params.Ncbps,Bool()))

   //for (k <- 0 until params.Ncbps) {
    
     //perm1(floor(params.Ncbps/16) * (k % 16) + floor(k/16)):= pout(k)
   //}
   //for (i <- 0 until params.Ncbps) {
       //io.out.bits( s*floor(i/s)+(i+params.Ncbps-floor(16* (i/params.Ncbps)) ) %s ):= perm1(i)  
      // }

}
//-- Interleaver modify2(48bits input: Nbpsc = 1)
class Interleaverds1b[T <: Data:Real:BinaryRepresentation](params: ModFFTParams[T]) extends Module {
    val io = IO(new Bundle {
    val in = Flipped(Decoupled(BitsBundle1bt(params)))
    val out = Decoupled(BitsBundle2t(params))

    
    //val in  = Flipped(Decoupled(Vec(params.Ncbps/params.Nbpsc,Bool())))
    //val out = Decoupled(Vec(params.Ncbps,Bool()))
   
    val sat = Output(UInt(2.W))
  })
  val s = floor( (params.Nbpsc+1)/2 )
  val pout = Reg(Vec(params.Ncbps,Bool()))
  
   // Make states for state machine
  val sInit = 0.U(1.W)
  val sDone = 1.U(1.W)
  val state = RegInit(sInit)
  val reg_pktstart = Reg(Bool())
  val reg_pktend = Reg(Bool())
  
 
   
  when (state === sInit && io.in.fire()) {
          state := sDone
          //iter := 0.U
	  for (i <- 0 until params.Ncbps) {
              pout(i) := io.in.bits.bits(i)  }

	  //pout := io.in.bits.bits 
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	 
  }
    when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }

  io.in.ready := state === sInit
  io.out.valid := state === sDone
  io.out.bits.pktStart := reg_pktstart
  io.out.bits.pktEnd := reg_pktend
  io.out.bits.bits := pout
  //io.cnt := iter
  io.sat := state
 // val perm1 = Wire(Vec(params.Ncbps,Bool()))

   //for (k <- 0 until params.Ncbps) {
    
     //perm1(floor(params.Ncbps/16) * (k % 16) + floor(k/16)):= pout(k)
   //}
   //for (i <- 0 until params.Ncbps) {
       //io.out.bits.bits( s*floor(i/s)+(i+params.Ncbps-floor(16* (i/params.Ncbps)) ) %s ):= perm1(i)  
      // }

}

//-- Interleaver modify2(48bits input: Nbpsc > 1)
class Interleaverds2b[T <: Data](params: ModFFTParams[T]) extends Module {
   val io = IO(new Bundle {
      val in = Flipped(Decoupled(BitsBundle1bt(params)))
      val out = Decoupled(BitsBundle2t(params))
    //val in  = Flipped(Decoupled(Vec(params.Ncbps / params.Nbpsc,Bool())))
    //val out = Decoupled(Vec(params.Ncbps,Bool()))
    val cnt = Output(UInt(8.W))
    val sat = Output(UInt(2.W))
  })

  val s = floor( (params.Nbpsc+1)/2 )
  val pout = Reg(Vec(params.Ncbps,Bool()))
  val cnt = Reg(UInt(8.W))
  val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)
   val reg_pktstart = Reg(Bool())
   val reg_pktend = Reg(Bool())
  //io.out(0):= RegNext(io.in)
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  reg_pktstart :=io.in.bits.pktStart
          reg_pktend :=io.in.bits.pktEnd
	  for  (i <-0 until params.Ncbps / params.Nbpsc){
	        pout(i) := io.in.bits.bits(i)
	  }
	  
                    
	 
  }
  when (state === sWork && io.in.fire()) {
         val iterNext = iter + 1.U
         iter := iterNext

        
         for  (i <-0 until params.Ncbps / params.Nbpsc){
	        pout(i) := io.in.bits.bits(i)
	  }
         for  (j <-1 until  params.Nbpsc){

	      for  (i <-0 until params.Ncbps / params.Nbpsc){
	        pout( (params.Ncbps / params.Nbpsc)*j + i ) :=  pout( (params.Ncbps / params.Nbpsc) *(j-1) + i ) }
	      
	 }
        
	
         when (iterNext >= ( params.Nbpsc - 1).U) {
            state := sDone
          }
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }

  io.in.ready := state === sInit || state === sWork
  io.out.valid := state === sDone
  //io.out.bits.bits := pout
  io.out.bits.pktStart := reg_pktstart
  io.out.bits.pktEnd := reg_pktend
  io.cnt := iter
  io.sat := state
  val perm1 = Wire(Vec(params.Ncbps,Bool()))

   for (k <- 0 until params.Ncbps) {
    
     perm1(floor(params.Ncbps/16) * (k % 16) + floor(k/16)):= pout(k) }
   for (i <- 0 until params.Ncbps) {
       io.out.bits.bits( s*floor(i/s)+(i+params.Ncbps-floor(16* (i/params.Ncbps)) ) %s ):= perm1(i)  }

}



//-- Interleaver modify2(48bits input: Nbpsc > 1)
class Interleaverds2[T <: Data](params: ModFFTParams[T]) extends Module {
   val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(Vec(params.Ncbps / params.Nbpsc,Bool())))
    val out = Decoupled(Vec(params.Ncbps,Bool()))
    val cnt = Output(UInt(8.W))
    val sat = Output(UInt(2.W))
  })

  val s = floor( (params.Nbpsc+1)/2 )
  val pout = Reg(Vec(params.Ncbps,Bool()))
  val cnt = Reg(UInt(8.W))
  val iter = Reg(UInt(8.W))
   // Make states for state machine
  val sInit = 0.U(2.W)
  val sWork = 1.U(2.W)
  val sDone = 2.U(2.W)
  val state = RegInit(sInit)
  //io.out(0):= RegNext(io.in)
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  for  (i <-0 until params.Ncbps / params.Nbpsc){
	        pout(i) := io.in.bits(i)
	  }
	  
                    
	 
  }
  when (state === sWork && io.in.fire()) {
         val iterNext = iter + 1.U
         iter := iterNext
        
         for  (i <-0 until params.Ncbps / params.Nbpsc){
	        pout(i) := io.in.bits(i)
	  }
         for  (j <-1 until  params.Nbpsc){

	      for  (i <-0 until params.Ncbps / params.Nbpsc){
	        pout( (params.Ncbps / params.Nbpsc)*j + i ) :=  pout( (params.Ncbps / params.Nbpsc) *(j-1) + i ) }
	      
	 }
        
	
         when (iterNext >= ( params.Nbpsc - 1).U) {
            state := sDone
          }
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }

  io.in.ready := state === sInit || state === sWork
  io.out.valid := state === sDone
  //io.out.bits := pout
  io.cnt := iter
  io.sat := state
  val perm1 = Wire(Vec(params.Ncbps,Bool()))

   for (k <- 0 until params.Ncbps) {
    
     perm1(floor(params.Ncbps/16) * (k % 16) + floor(k/16)):= pout(k) }
   for (i <- 0 until params.Ncbps) {
       io.out.bits( s*floor(i/s)+(i+params.Ncbps-floor(16* (i/params.Ncbps)) ) %s ):= perm1(i)  }

}



// serilizer
class Serilizer[T <: Data](params: BPSKModParams[T]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Flipped(Decoupled(Vec(params.Ncbps,Bool())))
    val out = Decoupled(Bool())
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
  val ser = Reg(Bool())
 
   
  when (state === sInit && io.in.fire()) {
          state := sWork
          iter := 0.U
	  pout := io.in.bits
	  	                      
	 
  }
  when (state === sWork ) {
         val iterNext = iter + 1.U
         iter := iterNext
         ser := pout(iter)
        	
         when (iterNext >= (params.Ncbps).U) {
            state := sDone
                      }
  }
  when (state === sDone && io.out.fire()) {
          state := sInit	 
	  
  }
  io.in.ready := state === sInit 
  io.out.valid :=  (state === sWork && iter >= 1.U) || state === sDone
  io.out.bits := ser


}

trait ModFFTParams[T <: Data] extends FFTParams[T] {
  val Ncbps: Int
  val Nbpsc: Int
  //val length: Int
}
object ModFFTParams {
  def apply[T <: Data](old_params: ModFFTParams[T], new_num_points: Int, newDecimType: String): ModFFTParams[T] = new ModFFTParams[T] {
    val protoIQ = old_params.protoIQ
    val protoTwiddle = old_params.protoTwiddle
    val numPoints = new_num_points
    val pipeline = old_params.pipeline
    val fftType = old_params.fftType
    val Ncbps = old_params.Ncbps
    val Nbpsc = old_params.Nbpsc
    val decimType    = newDecimType
    //val length = old_params.length
  }
}

/**
 * FFT parameters object for fixed-point FFTs
 */
case class FixedModFFTParams(
  // width of Input and Output
  dataWidth: Int,
  //length: Int,
  // width of twiddle constants
  twiddleWidth: Int,
  numPoints: Int = 4,
  Ncbps: Int,
  Nbpsc: Int,
  binPoints: Int,
  decimType: String = "opt",
  fftType: String = "direct",
  pipeline: Boolean = false
) extends ModFFTParams[FixedPoint] {
  val protoIQ = DspComplex(FixedPoint(dataWidth.W, (binPoints).BP))
  val protoTwiddle = DspComplex(FixedPoint(twiddleWidth.W, (twiddleWidth-2).BP))
}



// Mapper
class BPSKCPMapper[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(Vec(params.Ncbps, Bool())))
        val out = Decoupled(Vec(params.Ncbps, params.protoIQ.cloneType))

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
   //io.par := rin.asUInt
    for (i <- 0 until params.Ncbps){
      when (rin(i)){
	io.out.bits(i).real := ConvertableTo[T].fromDouble(1.0)
        io.out.bits(i).imag := ConvertableTo[T].fromDouble(0.0)  
      } .otherwise{
        io.out.bits(i).real := ConvertableTo[T].fromDouble(-1.0)
        io.out.bits(i).imag := ConvertableTo[T].fromDouble(0.0) 
      }
    }
}
// Mapper modify 
class BPSKCPMapper1[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
   val io = IO(new Bundle {
         val in = Flipped(Decoupled(BitsBundle2t(params)))
	 val out = Decoupled(PacketBundle(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
        //val in  = Flipped(Decoupled(Vec(params.Ncbps, Bool())))
        //val out = Decoupled(Vec(params.Ncbps, params.protoIQ.cloneType))

     })

      val rin = Reg(Vec(params.Ncbps, Bool()))
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
   //io.par := rin.asUInt
    for (i <- 0 until params.Ncbps){
      when (rin(i)){
	io.out.bits.iq(i).real := ConvertableTo[T].fromDouble(1.0)
        io.out.bits.iq(i).imag := ConvertableTo[T].fromDouble(0.0)  
      } .otherwise{
        io.out.bits.iq(i).real := ConvertableTo[T].fromDouble(-1.0)
        io.out.bits.iq(i).imag := ConvertableTo[T].fromDouble(0.0) 
      }
    }
}

class BPSKCPModulator[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
      val io = IO(new Bundle {
       val in  = Flipped(Decoupled(Bool()))
       val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
  })
 val mapping = Module( new BPSKCPMapper(params) )
 val interleaving =  Module( new Interleaver(params) )
 interleaving.io.in.bits := io.in.bits
 interleaving.io.in.valid := io.in.valid
 interleaving.io.out.ready := mapping.io.in.ready
 mapping.io.in.valid := interleaving.io.out.valid
 mapping.io.in.bits := interleaving.io.out.bits
 mapping.io.out.ready := io.out.ready
 io.out.bits := mapping.io.out.bits
 io.out.valid := mapping.io.out.valid
 io.in.ready := interleaving.io.in.ready
}

class BPSKCPModulator1[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
      val io = IO(new Bundle {
        val in = Flipped(Decoupled(BitsBundle1bt(params)))
	val out = Decoupled(PacketBundle(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
       //val in  = Flipped(Decoupled(Bool()))
       //val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
  })
 val mapping = Module( new BPSKCPMapper1(params) )
 val interleaving =  Module( new Interleaverds1b(params) )
 interleaving.io.in.bits := io.in.bits
 interleaving.io.in.valid := io.in.valid
 interleaving.io.out.ready := mapping.io.in.ready
 mapping.io.in.valid := interleaving.io.out.valid
 mapping.io.in.bits := interleaving.io.out.bits
 mapping.io.out.ready := io.out.ready
 io.out.bits := mapping.io.out.bits
 io.out.valid := mapping.io.out.valid
 io.in.ready := interleaving.io.in.ready
}
class QPSKCPModulator1[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
      val io = IO(new Bundle {
        val in = Flipped(Decoupled(BitsBundle1bt(params)))
	val out = Decoupled(PacketBundle(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
       //val in  = Flipped(Decoupled(Bool()))
       //val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
  })
 val mapping = Module( new QPSKCPMapper1(params) )
 val interleaving =  Module( new Interleaverds2b(params) )
 interleaving.io.in.bits := io.in.bits
 interleaving.io.in.valid := io.in.valid
 interleaving.io.out.ready := mapping.io.in.ready
 mapping.io.in.valid := interleaving.io.out.valid
 mapping.io.in.bits := interleaving.io.out.bits
 mapping.io.out.ready := io.out.ready
 io.out.bits := mapping.io.out.bits
 io.out.valid := mapping.io.out.valid
 io.in.ready := interleaving.io.in.ready
}

class QAM16CPModulator1[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
      val io = IO(new Bundle {
        val in = Flipped(Decoupled(BitsBundle1bt(params)))
	val out = Decoupled(PacketBundle(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
       //val in  = Flipped(Decoupled(Bool()))
       //val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
  })
 val mapping = Module( new QAM16CPMapper1(params) )
 val interleaving =  Module( new Interleaverds2b(params) )
 interleaving.io.in.bits := io.in.bits
 interleaving.io.in.valid := io.in.valid
 interleaving.io.out.ready := mapping.io.in.ready
 mapping.io.in.valid := interleaving.io.out.valid
 mapping.io.in.bits := interleaving.io.out.bits
 mapping.io.out.ready := io.out.ready
 io.out.bits := mapping.io.out.bits
 io.out.valid := mapping.io.out.valid
 io.in.ready := interleaving.io.in.ready
}

class QPSKCPModulator[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
      val io = IO(new Bundle {
       val in  = Flipped(Decoupled(Bool()))
       val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
  })
 val mapping = Module( new QPSKCPMapper(params) )
 val interleaving =  Module( new Interleaver(params) )
 interleaving.io.in.bits := io.in.bits
 interleaving.io.in.valid := io.in.valid
 interleaving.io.out.ready := mapping.io.in.ready
 mapping.io.in.valid := interleaving.io.out.valid
 mapping.io.in.bits := interleaving.io.out.bits
 mapping.io.out.ready := io.out.ready
 io.out.bits := mapping.io.out.bits
 io.out.valid := mapping.io.out.valid
 io.in.ready := interleaving.io.in.ready
}
class QPSKModFFTIO[T <: Data : Ring](params: ModFFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(ModFFTBundle(params)))
  val out = Decoupled(SerialPacketBundle(params))

  override def cloneType: this.type = QPSKModFFTIO(params).asInstanceOf[this.type]
}
object QPSKModFFTIO {
  def apply[T <: Data : Ring](params: ModFFTParams[T]): QPSKModFFTIO[T] =
    new QPSKModFFTIO(params)
}

class QPSKModFFT[T <: Data :Real:BinaryRepresentation](val params: ModFFTParams[T]) extends Module {
    val io = IO(QPSKModFFTIO(params))
    val qpskmod =  Module( new QPSKCPModulator(params))
    val ifft_cal = Module( new IFFT(params))
    val z2d = ConvertableTo[T].fromDouble(0.633)
    val z0 = Ring[T].zero
   qpskmod.io.in.bits := io.in.bits.fec
   qpskmod.io.in.valid := io.in.valid
   qpskmod.io.out.ready := ifft_cal.io.in.ready
   ifft_cal.io.in.valid := qpskmod.io.out.valid
  
   ifft_cal.io.out.ready := io.out.ready
   io.out.bits := ifft_cal.io.out.bits
   io.out.valid := ifft_cal.io.out.valid
   io.in.ready := qpskmod.io.in.ready
   ifft_cal.io.in.bits.pktStart := io.in.bits.pktStart
   ifft_cal.io.in.bits.pktEnd := io.in.bits.pktEnd
 
   // ADD PILOT default:0
   ifft_cal.io.in.bits.iq(7).real := z0
   ifft_cal.io.in.bits.iq(7).imag := z0
   ifft_cal.io.in.bits.iq(21).real := z0
   ifft_cal.io.in.bits.iq(21).imag := z0
   ifft_cal.io.in.bits.iq(43).real := z0
   ifft_cal.io.in.bits.iq(43).imag := z0
   ifft_cal.io.in.bits.iq(57).real := z0
   ifft_cal.io.in.bits.iq(57).imag := z0
   // subcarrier allocate
   ifft_cal.io.in.bits.iq(0).real := z0
   ifft_cal.io.in.bits.iq(0).imag := z0
   for (i <- 1 until 7){
     ifft_cal.io.in.bits.iq(i) := qpskmod.io.out.bits(i+23)
   }
   for (i <- 8 until 21){
     ifft_cal.io.in.bits.iq(i) := qpskmod.io.out.bits(i+22)
   }
   for (i <- 22 until 27){
     ifft_cal.io.in.bits.iq(i) := qpskmod.io.out.bits(i+21)
   }
   for (i <- 27 until 38){
     ifft_cal.io.in.bits.iq(i).real := z0
     ifft_cal.io.in.bits.iq(i).imag := z0
   }
   for (i <- 38 until 43){
     ifft_cal.io.in.bits.iq(i) := qpskmod.io.out.bits(i-38)
   }
   for (i <- 44 until 57){
     ifft_cal.io.in.bits.iq(i) := qpskmod.io.out.bits(i-39)
   }
   for (i <- 58 until 64){
     ifft_cal.io.in.bits.iq(i) := qpskmod.io.out.bits(i-40)
   }
    
  
}

class QPSKCPMapper[T <: Data :Real:BinaryRepresentation](val params:  ModFFTParams[T]) extends Module {
   val io = IO(new Bundle {
        val in  = Flipped(Decoupled(Vec(params.Ncbps, Bool())))
        val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))

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
   //io.par := rin.asUInt
    for (i <- 0 until params.Ncbps/2){
      when(!rin(2*i+1) && !rin(2*i)){
           io.out.bits(i).real := ConvertableTo[T].fromDouble(-0.707)
           io.out.bits(i).imag := ConvertableTo[T].fromDouble(-0.707) 
        }
        // 01
        .elsewhen(!rin(2*i+1) && rin(2*i)){
           io.out.bits(i).real := ConvertableTo[T].fromDouble(0.707)
           io.out.bits(i).imag := ConvertableTo[T].fromDouble(-0.707) 
        }
        //10
       .elsewhen(rin(2*i+1) && !rin(2*i)){
           io.out.bits(i).real := ConvertableTo[T].fromDouble(-0.707)
           io.out.bits(i).imag := ConvertableTo[T].fromDouble(0.707) 
        } 
        //11
       .elsewhen(rin(2*i+1) && rin(2*i)){
           io.out.bits(i).real := ConvertableTo[T].fromDouble(0.707)
           io.out.bits(i).imag := ConvertableTo[T].fromDouble(0.707) 
        }.otherwise{
	   io.out.bits(i).real := ConvertableTo[T].fromDouble(0.707)
           io.out.bits(i).imag := ConvertableTo[T].fromDouble(0.707) 
	
	} 

     
     }
}
// QPSK mapper modify
class QPSKCPMapper1[T <: Data :Real:BinaryRepresentation](val params:  ModFFTParams[T]) extends Module {
   val io = IO(new Bundle {
         val in = Flipped(Decoupled(BitsBundle2t(params)))
	 val out = Decoupled(PacketBundle(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
        //val in  = Flipped(Decoupled(Vec(params.Ncbps, Bool())))
        //val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))

     })

      val rin = Reg(Vec(params.Ncbps, Bool()))
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
   //io.par := rin.asUInt
    for (i <- 0 until params.Ncbps/params.Nbpsc){
      when(!rin(2*i+1) && !rin(2*i)){
           io.out.bits.iq(i).real := ConvertableTo[T].fromDouble(-0.707)
           io.out.bits.iq(i).imag := ConvertableTo[T].fromDouble(-0.707) 
        }
        // 01
        .elsewhen(!rin(2*i+1) && rin(2*i)){
           io.out.bits.iq(i).real := ConvertableTo[T].fromDouble(0.707)
           io.out.bits.iq(i).imag := ConvertableTo[T].fromDouble(-0.707) 
        }
        //10
       .elsewhen(rin(2*i+1) && !rin(2*i)){
           io.out.bits.iq(i).real := ConvertableTo[T].fromDouble(-0.707)
           io.out.bits.iq(i).imag := ConvertableTo[T].fromDouble(0.707) 
        } 
        //11
       .elsewhen(rin(2*i+1) && rin(2*i)){
           io.out.bits.iq(i).real := ConvertableTo[T].fromDouble(0.707)
           io.out.bits.iq(i).imag := ConvertableTo[T].fromDouble(0.707) 
        }.otherwise{
	   io.out.bits.iq(i).real := ConvertableTo[T].fromDouble(0.707)
           io.out.bits.iq(i).imag := ConvertableTo[T].fromDouble(0.707) 
	
	} 

     
     }
}

// QPSK mapper modify
class QAM16CPMapper1[T <: Data :Real:BinaryRepresentation](val params:  ModFFTParams[T]) extends Module {
   val io = IO(new Bundle {
         val in = Flipped(Decoupled(BitsBundle2t(params)))
	 val out = Decoupled(PacketBundle(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))
        //val in  = Flipped(Decoupled(Vec(params.Ncbps, Bool())))
        //val out = Decoupled(Vec(params.Ncbps/params.Nbpsc, params.protoIQ.cloneType))

     })

      val rin = Reg(Vec(params.Ncbps, Bool()))
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
   val zd = ConvertableTo[T].fromDouble(0.316)
   val z3d = ConvertableTo[T].fromDouble(0.949)
   //io.par := rin.asUInt
    for (i <- 0 until params.Ncbps/params.Nbpsc){
        io.out.bits.iq(i).real := Mux(rin(4*i+1) && rin(4*i),zd,Mux(rin(4*i+1) && !rin(4*i),-zd,Mux(!rin(4*i+1) && rin(4*i),z3d,-z3d)))
        io.out.bits.iq(i).imag := Mux(rin(4*i+3) && rin(4*i+2),zd,Mux(rin(4*i+3) && !rin(4*i+2),-zd,Mux(!rin(4*i+3) && rin(4*i+2),z3d,-z3d)))     
     
     }
}

class BPSKMapper[T <: Data :Real:BinaryRepresentation](val params: IQBundleParams[T]) extends Module {
   val io = IO(new Bundle {
    
    val in  = Input(Bool())
    val out = Output(params.protoIQ.cloneType)
  })
      when (io.in === 0.U){
	io.out.real := ConvertableTo[T].fromDouble(-1.0)
        io.out.imag := ConvertableTo[T].fromDouble(0.0)  
      } .otherwise{
        io.out.real := ConvertableTo[T].fromDouble(1.0)
        io.out.imag := ConvertableTo[T].fromDouble(0.0) 
      }

}


class QPSKMapper[T <: Data :Real:BinaryRepresentation](val params: IQBundleParams[T]) extends Module {
   val io = IO(new Bundle {
    
    val in  = Input(UInt(2.W))
    val out = Output(params.protoIQ.cloneType)
  })
   when(io.in === 0.U){
           io.out.real := ConvertableTo[T].fromDouble(-0.707)
           io.out.imag := ConvertableTo[T].fromDouble(-0.707) 
        }
        // 01
        .elsewhen(io.in === 1.U){
           io.out.real := ConvertableTo[T].fromDouble(0.707)
           io.out.imag := ConvertableTo[T].fromDouble(-0.707) 
        }
        //10
       .elsewhen(io.in === 2.U){
           io.out.real := ConvertableTo[T].fromDouble(-0.707)
           io.out.imag := ConvertableTo[T].fromDouble(0.707) 
        } 
        //11
       .elsewhen(io.in === 3.U){
           io.out.real := ConvertableTo[T].fromDouble(0.707)
           io.out.imag := ConvertableTo[T].fromDouble(0.707) 
        }.otherwise{
	   io.out.real := ConvertableTo[T].fromDouble(0.707)
           io.out.imag := ConvertableTo[T].fromDouble(0.707) 
	
	} 
   
}



class Mapper[T <: Data :Real:BinaryRepresentation](val params: MapParams[T]) extends Module {
    val io = IO(MapperIO(params))
    val nbpsc = params.n.U
    // BPSK KMOD=1
     when(nbpsc === 1.U) {
      when (io.in === 0.U){
      io.out_i := ConvertableTo[T].fromDouble(-1.0)
      io.out_q := ConvertableTo[T].fromDouble(0.0)
      } .otherwise{
        io.out_i := ConvertableTo[T].fromDouble(1.0)
        io.out_q := ConvertableTo[T].fromDouble(0.0)  
      }
     }
    //QPSK KMOD=(1/2)^0.5
    .elsewhen (nbpsc === 2.U){
        // 00
        when(io.in === 0.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.707)
           io.out_q := ConvertableTo[T].fromDouble(-0.707) 
        }
        // 01
        .elsewhen(io.in === 1.U){
           io.out_i := ConvertableTo[T].fromDouble(0.707)
           io.out_q := ConvertableTo[T].fromDouble(-0.707) 
        }
        //10
       .elsewhen(io.in === 2.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.707)
           io.out_q := ConvertableTo[T].fromDouble(0.707) 
        } 
        //11
       .elsewhen(io.in === 3.U){
           io.out_i := ConvertableTo[T].fromDouble(0.707)
           io.out_q := ConvertableTo[T].fromDouble(0.707) 
        }.otherwise{
	   io.out_i := ConvertableTo[T].fromDouble(0.707)
           io.out_q := ConvertableTo[T].fromDouble(0.707) 
	
	} 
        
    }
    //16QAM KMOD=(1/10)^(0.5)
    .elsewhen (nbpsc === 4.U){
        // 0000
        when(io.in === 0.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.949)
           io.out_q := ConvertableTo[T].fromDouble(-0.949) 
        }
        // 0001
        .elsewhen(io.in === 1.U){
           io.out_i := ConvertableTo[T].fromDouble(0.949)
           io.out_q := ConvertableTo[T].fromDouble(-0.949) 
        }
        //0010
        .elsewhen(io.in === 2.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.316)
           io.out_q := ConvertableTo[T].fromDouble(-0.949) 
        }
        //0011
        .elsewhen(io.in === 3.U){
           io.out_i := ConvertableTo[T].fromDouble(0.316)
           io.out_q := ConvertableTo[T].fromDouble(-0.949) 
        }
        //0100
        .elsewhen(io.in === 4.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.949)
           io.out_q := ConvertableTo[T].fromDouble(0.949) 
        }
        //0101
        .elsewhen(io.in === 5.U){
           io.out_i := ConvertableTo[T].fromDouble(0.949)
           io.out_q := ConvertableTo[T].fromDouble(0.949) 
        }
        //0110
        .elsewhen(io.in === 6.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.316)
           io.out_q := ConvertableTo[T].fromDouble(0.949) 
        }
        //0111
        .elsewhen(io.in === 7.U){
           io.out_i := ConvertableTo[T].fromDouble(0.316)
           io.out_q := ConvertableTo[T].fromDouble(0.949) 
        }
        // 1000
        .elsewhen(io.in === 8.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.949)
           io.out_q := ConvertableTo[T].fromDouble(-0.316) 
        }
        // 1001
        .elsewhen(io.in === 9.U){
           io.out_i := ConvertableTo[T].fromDouble(0.949)
           io.out_q := ConvertableTo[T].fromDouble(-0.316) 
        }
        //1010
        .elsewhen(io.in === 10.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.316)
           io.out_q := ConvertableTo[T].fromDouble(-0.316) 
        }
        //1011
        .elsewhen(io.in === 11.U){
           io.out_i := ConvertableTo[T].fromDouble(0.316)
           io.out_q := ConvertableTo[T].fromDouble(-0.316) 
        }
        //1100
        .elsewhen(io.in === 12.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.949)
           io.out_q := ConvertableTo[T].fromDouble(0.316) 
        }
        //1101
        .elsewhen(io.in === 13.U){
           io.out_i := ConvertableTo[T].fromDouble(0.949)
           io.out_q := ConvertableTo[T].fromDouble(0.316) 
        }
        //1110
        .elsewhen(io.in === 14.U){
           io.out_i := ConvertableTo[T].fromDouble(-0.316)
           io.out_q := ConvertableTo[T].fromDouble(0.316) 
        }
        //1111
        .elsewhen(io.in === 15.U){
           io.out_i := ConvertableTo[T].fromDouble(0.316)
           io.out_q := ConvertableTo[T].fromDouble(0.316) 
        }.otherwise{
	   io.out_i := ConvertableTo[T].fromDouble(0.316)
           io.out_q := ConvertableTo[T].fromDouble(0.316)
	}
        
        
    }.otherwise{
      io.out_i := ConvertableTo[T].fromDouble(0)
      io.out_q := ConvertableTo[T].fromDouble(0.316)

 }
    
    
}












  
    
    


 


    

