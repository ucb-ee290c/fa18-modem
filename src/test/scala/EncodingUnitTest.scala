import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

//  val g_matrix = List(7 ,5)
//  def genPolyInit(g_matrix: List[Int], bit_width: Int): Unit ={
//    val vec2D = VecInit(g_matrix.map(x => VecInit(x.U(bit_width.W))))
//    vec2D
//  }
//  val genPolyList: Unit = genPolyInit(g_matrix, 3)
//  printf(p"$v")

class RegNextModule extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(12.W))
    val out = Output(UInt(12.W))
  })

  // register bitwidth is inferred from io.out
  io.out := RegNext(io.in + 1.U)
}

class RegNextModuleTester(c: RegNextModule) extends PeekPokeTester(c) {
  for (i <- 0 until 100) {
    poke(c.io.in, i)
    step(1)
    expect(c.io.out, i+1)
  }
}
assert(chisel3.iotesters.Driver(() => new RegNextModule) { c => new RegNextModuleTester(c) })
println("SUCCESS!!")