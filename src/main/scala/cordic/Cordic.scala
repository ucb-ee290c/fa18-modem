package cordic

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



/**
 * Base class for CORDIC parameters
 *
 * These are type generic
 */
trait CordicParams[T <: Data] {
  val protoXY: T
  val protoZ: T
  val nStages: Int
  val correctGain: Boolean
  val stagesPerCycle: Int
}

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


trait MapParams[T <: Data]{
  val n: Int
  val protoIQ: T
}
trait FirParams[T <: Data] {
  val protoin: T
  val protoout: T
  val length: Int
  val constants: T

}
case class FixedFirParams(
    fiwidth: Int,
    fowidth: Int,
    cwidth: Int,
    length: Int
) extends FirParams[FixedPoint] {
    val protoin = FixedPoint(fiwidth.W, (fiwidth-3).BP)
    val protoout = FixedPoint(fowidth.W, (fowidth-3).BP)
    val constants = FixedPoint(cwidth.W, (cwidth-2).BP)
}

case class UIntFirParams(
   mwidth: Int,
   length: Int

) extends FirParams[UInt] {
  val protoin = UInt(mwidth.W)
  val protoout = UInt(mwidth.W)
  val constants = UInt(mwidth.W)
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


case class FixedCordicParams(
  // width of X and Y
  xyWidth: Int,
  // width of Z
  zWidth: Int,
  // scale output by correction factor?
  correctGain: Boolean = true,
  // number of CORDIC stages to perform per clock cycle
  stagesPerCycle: Int = 1,
  //nStages: Int 
) extends CordicParams[FixedPoint] {
  // prototype for x and y
  // binary point is (xyWidth-2) to represent 1.0 exactly
  val protoXY = FixedPoint(xyWidth.W, (xyWidth-2).BP)
  // prototype for z
  // binary point is (xyWidth-3) to represent Pi/2 exactly
  val protoZ = FixedPoint(zWidth.W, (zWidth-2).BP)
  val minNumber = math.pow(2.0, -(zWidth-2))
  // number of cordic stages
  private var n = 0
  while (breeze.numerics.tan(math.pow(2.0, -n)) >= minNumber) {
    n += 1
  }
  val nStages = n
}

case class DspRealCordicParams(

nStages: Int,

correctGain: Boolean = true,

stagesPerCycle: Int = 1,

) extends CordicParams[DspReal] {

val protoXY = DspReal()

val protoZ = DspReal()

}
/**
 * Bundle type that describes the input, state, and output of CORDIC
 */
class CordicBundle[T <: Data](params: CordicParams[T]) extends Bundle {
  val x: T = params.protoXY.cloneType
  val y: T = params.protoXY.cloneType
  val z: T = params.protoZ.cloneType

  override def cloneType: this.type = CordicBundle(params).asInstanceOf[this.type]
}
object CordicBundle {
  def apply[T <: Data](params: CordicParams[T]): CordicBundle[T] = new CordicBundle(params)
}

/**
 * Bundle type as IO for iterative CORDIC modules
 */
class IterativeCordicIO[T <: Data](params: CordicParams[T]) extends Bundle {
  val in = Flipped(Decoupled(CordicBundle(params)))
  val out = Decoupled(CordicBundle(params))
  val cnt = Output(UInt(5.W))
  //val out1 = Output(params.protoZ.cloneType)
  //val cycle_cnt = Output(UInt(log2Ceil(params.nStages/params.stagesPerCycle).toInt.W))
  val vectoring = Input(Bool())

  override def cloneType: this.type = IterativeCordicIO(params).asInstanceOf[this.type]
}
object IterativeCordicIO {
  def apply[T <: Data](params: CordicParams[T]): IterativeCordicIO[T] =
    new IterativeCordicIO(params)
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
    
    //val pout = Vec(params.n, Decoupled(params.gen.cloneType))
    override def cloneType: this.type = MapperIO(params).asInstanceOf[this.type]
}
object MapperIO {
  def apply[T <: Data](params: MapParams[T]): MapperIO[T] =
    new MapperIO(params)
}

//---Interleaver

class Interleav[T <: Data](params: InterleavParams[T]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Input(Vec(params.Ncbps,Bool()))
    val out = Output(Vec(params.Ncbps,Bool()))
  })
  val s = floor( (params.Nbpsc+1)/2 )
  val perm1 = Wire(Vec(params.Ncbps,Bool()))
  for (k <- 0 until params.Ncbps) {
    
    perm1(floor(params.Ncbps/16) * (k % 16) + floor(k/16)):= io.in(k)
  }
 for (i <- 0 until params.Ncbps) {
    
    io.out( s*floor(i/s)+(i+params.Ncbps-floor(16* (i/params.Ncbps)) ) %s ):= perm1(i)
  }

}
//---Deinterleaver
class Deinterleav[T <: Data](params: InterleavParams[T]) extends Module {
    val io = IO(new Bundle {
    
    val in  = Input(Vec(params.Ncbps,Bool()))
    val out = Output(Vec(params.Ncbps,Bool()))
  })
  val s = floor( (params.Nbpsc+1)/2 )
  val perm1 = Wire(Vec(params.Ncbps,Bool()))
  for (j <- 0 until params.Ncbps) {
    perm1 (s*floor(j/s)+(j+floor(16* (j/params.Ncbps))) %s ) := io.in(j)
    
  }
 for (i <- 0 until params.Ncbps) {
    
    io.out( 16*i -(params.Ncbps -1)* floor(16* (i/params.Ncbps)) ):= perm1(i)
  }

}


class FirIO[T <: Data](params: FirParams[T]) extends Bundle {
    val consts = Input(Vec(params.length, params.constants.cloneType))
    val valid = Input(Bool())
    val in = Input(params.protoin.cloneType)
    val out = Output(params.protoout.cloneType)
        //val pout = Vec(params.n, Decoupled(params.gen.cloneType))
    override def cloneType: this.type = FirIO(params).asInstanceOf[this.type]
}
object FirIO {
  def apply[T <: Data](params: FirParams[T]): FirIO[T] =
    new FirIO(params)
}


object AddSub {
  def apply[T <: Data : Ring](sel: Bool, a: T, b: T): T = {
    Mux(sel, a + b, a - b)
  }
}

// Mapper


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


class ScalaFirFilter(taps: Seq[Int]) {
  var pseudoRegisters = List.fill(taps.length)(0)

  def poke(value: Int): Int = {
    pseudoRegisters = value :: pseudoRegisters.take(taps.length - 1)
    var accumulator = 0
    for(i <- taps.indices) {
      accumulator += taps(i) * pseudoRegisters(i)
    }
    accumulator
  }
}

// Fir

class MFir[T <: Data :Real:BinaryRepresentation](val params: FirParams[T]) extends Module {
  val io = IO(FirIO(params))
 
  val taps = Seq(io.in) ++ Seq.fill(io.consts.length - 1)(RegInit(Ring[T].zero))
  taps.zip(taps.tail).foreach { case (a, b) => when (io.valid) { b := a } }

  io.out := taps.zip(io.consts).map { case (a, b) => a * b }.reduce(_ + _)
}
//Demapper

class Demapper[T <: Data :Real:BinaryRepresentation](val params: DemapParams[T]) extends Module {
    val io = IO(DemapperIO(params))
    val nbpsc = params.n.U
    // BPSK
    val z2d = ConvertableTo[T].fromDouble(0.633)
    val z0 = Ring[T].zero
     when(nbpsc === 1.U) {
        when(io.ini < z0){
	  io.out := 0.U
	}.otherwise {
	  io.out := 1.U
	  //QPSK
	}
     }.elsewhen(nbpsc === 2.U){
	  when (io.ini < z0 && io.inq < z0){
	  io.out :=0.U
	  }.elsewhen(io.ini < z0 && io.inq >= z0){
	   io.out :=2.U
	  }.elsewhen(io.ini >= z0 && io.inq < z0){
	   io.out := 1.U
	   }.otherwise{
	   io.out := 3.U
	   }
	
	}
    
     .elsewhen(nbpsc === 4.U){
       when(io.ini > z0&& io.ini <= z2d && io.inq > z0 && io.inq <=z2d){
           io.out := 15.U
       }.elsewhen(io.ini > z0&& io.ini <= z2d &&  io.inq > z2d) {
           io.out := 7.U
       }.elsewhen(io.ini > z2d && io.inq > z0 && io.inq <= z2d) {
           io.out := 13.U
       }.elsewhen(io.ini > z2d && io.inq > z2d) {
           io.out := 5.U
       }.elsewhen(io.ini > z0&& io.ini <= z2d && io.inq <= -z2d){
           io.out := 3.U
       }.elsewhen(io.ini > z0&& io.ini <= z2d && io.inq > -z2d && io.inq <= z0){
           io.out := 11.U
       }.elsewhen(io.ini >  z2d && io.inq > -z2d && io.inq <= z0){
           io.out := 9.U
       }.elsewhen(io.ini >  z2d && io.inq <= -z2d){
           io.out := 1.U
       
       }.elsewhen(io.ini > -z2d&& io.ini <= z0 && io.inq > z0 && io.inq <= z2d){
           io.out := 14.U
       }.elsewhen(io.ini > -z2d&& io.ini <= z0 && io.inq > z2d){
           io.out := 6.U
       }.elsewhen(io.ini <= -z2d && io.inq > z0 && io.inq <=z2d){
           io.out := 12.U
       }.elsewhen(io.ini <= -z2d && io.inq > z2d){
           io.out := 4.U
       }.elsewhen(io.ini <= -z2d && io.inq > -z2d && io.inq <= z0){
           io.out := 8.U
       }.elsewhen(io.ini > -z2d && io.ini <= z0 && io.inq > -z2d && io.inq <= z0){
           io.out :=10.U
       }.elsewhen(io.ini <= -z2d && io.inq  <= -z2d){
           io.out := 0.U
       }.otherwise{
           io.out := 2.U
       } 
     
     }.otherwise{

         io.out := 0.U
        }
}

// serial to parallel
class ShiftRegister(val params: P2sParams[UInt] ) extends Module {
    val io = IO(ShiftRegisterIO(params))
    //val out_vec = Reg(Vec(params.n ,io.sin.bits.cloneType))
    val in_eff = Reg(io.sin.bits.cloneType)
    

    val nbpsc = params.n.U
    //when (io.sin.fire()){
      io.pout(0).bits:= RegNext(io.sin.bits)
     //}
    
    for (i <- 1 until params.n){
      io.pout(i).bits := RegNext(io.pout(i-1).bits)
    }  
    val cntxyz = RegInit(UInt(5.W), 0.U)
    cntxyz := Mux(cntxyz >= (params.n-1).U, 0.U, cntxyz + 1.U)
    
    when(cntxyz === 0.U){
      io.sin.ready := true.B
      for  (i <- 0 until params.n) {
        io.pout(i).valid:= true.B
	//io.pout(i).bits := out_vec(i)
	}
      }.otherwise{
       io.sin.ready := false.B
      for  (i <- 0 until params.n) {
        io.pout(i).valid:= false.B
	//io.pout(i).bits :=  out_vec(i)
      }      
       }

    
        //io.pout.bits.foldLeft(io.sin.bits) { case (sin, pout) =>
       //pout := sin
        //RegNext(sin)
    //}
}


  
class IterativeCordic[T <: Data :Real:BinaryRepresentation](val params: CordicParams[T]) extends Module {
  val io = IO(IterativeCordicIO(params))
  
  
  //MAP TO (0,45)
  //val xmap = Wire (FixedPoint(io.in.bits.x.getWidth.W, io.in.bits.x.binaryPoint))
  val xmap = Wire (io.in.bits.x.cloneType)
  val ymap = Wire (io.in.bits.y.cloneType)
 
  val zmap = Wire (io.in.bits.z.cloneType)
  //(FixedPoint(io.in.bits.z.getWidth.W, io.in.bits.z.binaryPoint))
  
   //0 fixed point
  val x0 = Ring[T].zero
  //Wire(io.in.bits.x.cloneType)
  //ConvertableTo[T].fromDouble(0)
  
  
  //Wire (FixedPoint(io.in.bits.x.getWidth.W, io.in.bits.x.binaryPoint))
  val y0 = Ring[T].zero
  //Wire (FixedPoint(io.in.bits.y.getWidth.W, io.in.bits.y.binaryPoint))
  val z0 = Ring[T].zero
  
  
  val xycat = Wire (UInt(3.W))
  val xs = Wire (Bool())
  val ys = Wire (Bool())
  val xys = Wire (Bool())
  xs := io.in.bits.x >  x0
  ys := io.in.bits.y >  y0
  xys :=io.in.bits.x.abs() > io.in.bits.y.abs()
  xycat := Cat(xs,ys,xys)
  //xycat---reg
  val regxycat = Reg (UInt(3.W))

  //pi related
  val zpi4 = ConvertableTo[T].fromDouble(Pi/4)
  val zpi2 = ConvertableTo[T].fromDouble(Pi/2)
  val z3pi4 = ConvertableTo[T].fromDouble(3.0*Pi/4)
  val zpi = ConvertableTo[T].fromDouble(Pi)
  val znpi4 = z0 - zpi4 
  val znpi2 = z0 - zpi2

  val zn3pi4 = z0 - z3pi4
  val znpi = z0 - zpi


  val zmapr = Wire (io.in.bits.z.cloneType)
  
  // zcat
  val zcat = Wire (UInt(3.W))
  // Reg zcat
  val regzcat = Reg (UInt(3.W))
  when (z0 <= io.in.bits.z && io.in.bits.z <= zpi4) {
    zmapr := io.in.bits.z ; zcat := 0.U 
  } .elsewhen ( zpi4 < io.in.bits.z && io.in.bits.z <= zpi2) {
    zmapr := zpi2 - io.in.bits.z ; zcat := 1.U
  } .elsewhen ( zpi2 < io.in.bits.z && io.in.bits.z<= z3pi4) {
    zmapr := io.in.bits.z - zpi2 ; zcat := 2.U
  } .elsewhen ( z3pi4 < io.in.bits.z && io.in.bits.z <= zpi) {
    zmapr := zpi - io.in.bits.z; zcat := 3.U
  } .elsewhen ( znpi < io.in.bits.z && io.in.bits.z <= zn3pi4) {
    zmapr := zpi + io.in.bits.z; zcat := 4.U
  }.elsewhen ( zn3pi4 < io.in.bits.z && io.in.bits.z <= znpi2) {
    zmapr := z0 - io.in.bits.z -zpi2 ; zcat := 5.U
  } .elsewhen ( znpi2 < io.in.bits.z && io.in.bits.z <= znpi4) {
    zmapr := io.in.bits.z + zpi2; zcat := 6.U
  } .otherwise {
    zmapr := z0 - io.in.bits.z; zcat := 7.U
  }
/// xmap,ymap
   when(io.vectoring) {
     when(xycat===7.U) {
       xmap := io.in.bits.x;ymap := io.in.bits.y;zmap:= io.in.bits.z 
      } .elsewhen (xycat === 6.U) {
       xmap := io.in.bits.y;ymap := io.in.bits.x;zmap:= io.in.bits.z
      } .elsewhen (xycat === 2.U) {
       xmap :=  io.in.bits.y;ymap := x0-io.in.bits.x;zmap:= io.in.bits.z
      } .elsewhen (xycat === 3.U) {
       xmap := x0 - io.in.bits.x;ymap := io.in.bits.y;zmap:= io.in.bits.z
      } .elsewhen (xycat ===1.U) {
       xmap := x0 - io.in.bits.x;ymap := y0 - io.in.bits.y;zmap:= io.in.bits.z
      } .elsewhen (xycat === 0.U) {
       xmap := y0 - io.in.bits.y;ymap := x0 - io.in.bits.x;zmap:= io.in.bits.z
      } .elsewhen (xycat === 4.U) {
       xmap := y0 - io.in.bits.y;ymap := io.in.bits.x;zmap:= io.in.bits.z
      }.otherwise{
        xmap := io.in.bits.x;ymap := y0- io.in.bits.y;zmap:= io.in.bits.z
      }
  
   }.otherwise {
	    {xmap := io.in.bits.x;
	    ymap := io.in.bits.y;
	    zmap := zmapr}
	     }



	     
   val xvec = Wire(Vec(params.stagesPerCycle + 1 ,io.in.bits.x.cloneType))
   val yvec = Wire(Vec(params.stagesPerCycle + 1,io.in.bits.y.cloneType))
   val zvec = Wire(Vec(params.stagesPerCycle + 1,io.in.bits.z.cloneType))
   
   //val regxm = RegInit(FixedPoint(io.in.bits.x.getWidth.W, io.in.bits.x.binaryPoint),y0)	     
   //val regym = RegInit(FixedPoint(io.in.bits.y.getWidth.W, io.in.bits.y.binaryPoint),y0)
   //val regzm = RegInit(FixedPoint(io.in.bits.z.getWidth.W, io.in.bits.z.binaryPoint),z0)
   
   val regxm = Reg(io.in.bits.x.cloneType)
   //Reg(FixedPoint(io.in.bits.x.getWidth.W, io.in.bits.x.binaryPoint))	     
   val regym = Reg(io.in.bits.y.cloneType)
   //Reg(FixedPoint(io.in.bits.y.getWidth.W, io.in.bits.y.binaryPoint))
   val regzm = Reg(io.in.bits.z.cloneType)
   //Reg(FixedPoint(io.in.bits.z.getWidth.W, io.in.bits.z.binaryPoint))
   
   val m = VecInit(Constants.arctan(params.nStages).map(ConvertableTo[T].fromDouble(_)))
   //map(_.F(io.in.bits.z.getWidth.W, io.in.bits.z.binaryPoint)))
   val gainc = ConvertableTo[T].fromDouble(Constants.gain(params.nStages))
   val gainc_inv = ConvertableTo[T].fromDouble(1 / Constants.gain(params.nStages))  
   //val gainc_inv = 1.0.F(io.in.bits.x.getWidth.W, io.in.bits.x.binaryPoint) 
   val cntxyz = RegInit(UInt(5.W), 0.U)
   cntxyz := Mux(io.out.fire()||io.in.fire(),0.U, cntxyz + 1.U) 
   //Mux( cntxyz >= ((params.nStages / params.stagesPerCycle).toInt.U - 1.U), 0.U, cntxyz + 1.U)
   

   val iter = RegInit(0.U(log2Ceil(params.nStages + 1).W))
   
   // Make states for state machine
   val sInit = 0.U(2.W)
   val sWork = 1.U(2.W)
   val sDone = 2.U(2.W)
   val state = RegInit(sInit)
   
   //io.cnt := cntxyz
   //cntxyz := 1.U + cntxyz
   //val index = Wire(Vec(params.stagesPerCycle, UInt(5.W)))
   val regVectoring = Reg(Bool())
   when (state === sInit && io.in.fire()) {
    state := sWork
    iter := 0.U
    regVectoring := io.vectoring
    regxycat := xycat
    regzcat := zcat
    
  }
  when (state === sWork) {
    val iterNext = iter + params.stagesPerCycle.U
    iter := iterNext
    when (iterNext >= (params.nStages - 1).U) {
      state := sDone
    }

    
  }
  when (state === sDone && io.out.fire()) {
    state := sInit
  }

  io.cnt := iter
  
   when (regVectoring){
     when (state === sWork) {
        regxm := xvec(params.stagesPerCycle)
        regym := yvec(params.stagesPerCycle)
        regzm := zvec(params.stagesPerCycle)
      }
       when (state === sInit && io.in.fire()) {
        regxm := xmap
	regym := ymap
	regzm := zmap
      }
      //regxm := Mux(io.in.fire() && (cntxyz===0.U), xmap, xvec(params.stagesPerCycle))
      //regym := Mux(io.in.fire() && (cntxyz===0.U), ymap, yvec(params.stagesPerCycle))
      //regzm := Mux(io.in.fire() && (cntxyz===0.U), zmap, zvec(params.stagesPerCycle))

       xvec(0) := regxm
       yvec(0) := regym
       zvec(0) := regzm
 
       

    for (i <- 1 until params.stagesPerCycle + 1){
      
           
        xvec(i) := AddSub( (yvec(i-1) > y0), xvec(i-1), yvec(i-1)>>(  cntxyz*params.stagesPerCycle.U +(i-1).U) );
        yvec(i) := AddSub( yvec(i-1) > y0, yvec(i-1), -xvec(i-1)>>(  cntxyz*params.stagesPerCycle.U +(i-1).U) );
	zvec(i) := AddSub( yvec(i-1) > y0, zvec(i-1), m(cntxyz*params.stagesPerCycle.U + (i-1).U )  )
	

   }	
  } .otherwise {
      when (state === sWork) {
        regxm := xvec(params.stagesPerCycle)
        regym := yvec(params.stagesPerCycle)
        regzm := zvec(params.stagesPerCycle)
      }
      when (state === sInit && io.in.fire()) {
        regxm := xmap
	regym := ymap
	regzm := zmap
      }
      //regxm := Mux(io.in.fire() && (cntxyz===0.U), xmap, xvec(params.stagesPerCycle))
      //regym := Mux(io.in.fire() && (cntxyz===0.U), ymap, yvec(params.stagesPerCycle))
      //regzm := Mux(io.in.fire() && (cntxyz===0.U), zmap, zvec(params.stagesPerCycle))

       xvec(0) := regxm
       yvec(0) := regym
       zvec(0) := regzm
       //io.out1 := regzm
       //xvec(0) := Mux(cntxyz===0.U, xmap,regxm)
       //yvec(0) := Mux(cntxyz===0.U, ymap,regym)
       //zvec(0) := Mux(cntxyz===0.U, zmap,regzm)

      


     for (i <- 1 until params.stagesPerCycle + 1){
        
       //index(i-1) := cntxyz* params.stagesPerCycle.U +i.U
        xvec(i) := AddSub( zvec(i-1) < z0, xvec(i-1), yvec(i-1)>> (cntxyz*params.stagesPerCycle.U +(i-1).U  ));
        yvec(i) := AddSub( zvec(i-1) < z0, yvec(i-1), -xvec(i-1)>> (cntxyz*  params.stagesPerCycle.U +(i-1).U)  );
	zvec(i) := AddSub( zvec(i-1) < z0, zvec(i-1), m(cntxyz* params.stagesPerCycle.U + (i-1).U )  ) 
	//zvec(i) := zvec(i-1)-zpi4 
	}
   }
  //io.out.bits.x := xvec(params.stagesPerCycle)
  //io.out.valid 
  //===to modify
  //io.out.valid := (cntxyz === ((params.nStages / params.stagesPerCycle).U - 1.U))
  //val reg_out_fire = RegInit(Bool(), false.B)
  //reg_out_fire := io.out.fire() 
  //io.in.ready := reg_out_fire
  io.in.ready := state === sInit
  io.out.valid := state === sDone
  
  when (regVectoring) {
    
    when(regxycat === 7.U) 
      {io.out.bits.x := xvec(params.stagesPerCycle);io.out.bits.y := yvec(params.stagesPerCycle);io.out.bits.z := zvec(params.stagesPerCycle)}
      .elsewhen(regxycat === 6.U){io.out.bits.x := yvec(params.stagesPerCycle);io.out.bits.y := xvec(params.stagesPerCycle);io.out.bits.z := zpi2-zvec(params.stagesPerCycle)}
      .elsewhen(regxycat === 2.U){io.out.bits.x := y0-yvec(params.stagesPerCycle);io.out.bits.y := xvec(params.stagesPerCycle);io.out.bits.z := zpi2+zvec(params.stagesPerCycle)}
      .elsewhen(regxycat ===3.U){io.out.bits.x := x0-xvec(params.stagesPerCycle);io.out.bits.y := yvec(params.stagesPerCycle);io.out.bits.z := zpi-zvec(params.stagesPerCycle)}
      .elsewhen(regxycat === 1.U){io.out.bits.x := x0-xvec(params.stagesPerCycle);io.out.bits.y := y0 - yvec(params.stagesPerCycle);io.out.bits.z := znpi + zvec(params.stagesPerCycle)}
      .elsewhen(regxycat ===0.U){io.out.bits.x := y0-yvec(params.stagesPerCycle);io.out.bits.y := x0 - xvec(params.stagesPerCycle);io.out.bits.z := znpi2 - zvec(params.stagesPerCycle)}
      .elsewhen(regxycat ===4.U){io.out.bits.x := yvec(params.stagesPerCycle);io.out.bits.y := x0 - xvec(params.stagesPerCycle);io.out.bits.z := znpi2 + zvec(params.stagesPerCycle)}
      .otherwise{io.out.bits.x := xvec(params.stagesPerCycle);io.out.bits.y := y0 - yvec(params.stagesPerCycle);io.out.bits.z := z0- zvec(params.stagesPerCycle)}
   
  }.otherwise {
     when(regzcat===0.U) {
      io.out.bits.x := xvec(params.stagesPerCycle) * gainc_inv;
      io.out.bits.y := yvec(params.stagesPerCycle) * gainc_inv;
      io.out.bits.z := zvec(params.stagesPerCycle)
       
  
      } .elsewhen( regzcat ===1.U) {
       io.out.bits.x := yvec(params.stagesPerCycle) * gainc_inv;
       io.out.bits.y := xvec(params.stagesPerCycle) * gainc_inv;
       io.out.bits.z := zvec(params.stagesPerCycle)    
        
    
      } .elsewhen ( regzcat ===2.U) {
         io.out.bits.x := -yvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.y := xvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.z :=  zvec(params.stagesPerCycle) 
      } .elsewhen ( regzcat ===3.U) {
         io.out.bits.x := -xvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.y := yvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.z := zvec(params.stagesPerCycle) 
      } .elsewhen ( regzcat ===4.U) {
         io.out.bits.x := -xvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.y := -yvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.z := zvec(params.stagesPerCycle)
      } .elsewhen ( regzcat ===5.U) {
         io.out.bits.x := -yvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.y := -xvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.z := zvec(params.stagesPerCycle)
      } .elsewhen ( regzcat ===6.U) {
         io.out.bits.x := yvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.y := -xvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.z := zvec(params.stagesPerCycle)
      } .otherwise {
         io.out.bits.x := xvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.y := -yvec(params.stagesPerCycle) * gainc_inv;
         io.out.bits.z := zvec(params.stagesPerCycle)
      }
    }


  // new clock pin
   val clk_div = Reg(Clock())
   clk_div := Mux(io.out.fire()||io.in.fire(),false.B, true.B).asClock()
           
}
    
    

/**
  * Mixin for top-level rocket to add a PWM
  *
  */
trait HasPeripheryCordic extends BaseSubsystem {
  // instantiate cordic chain
  val cordicChain = LazyModule(new CordicThing(FixedCordicParams(10,16 )))
  // connect memory interfaces to pbus
  pbus.toVariableWidthSlave(Some("cordicWrite")) { cordicChain.writeQueue.mem.get }
  pbus.toVariableWidthSlave(Some("cordicRead")) { cordicChain.readQueue.mem.get }
}

 


    
