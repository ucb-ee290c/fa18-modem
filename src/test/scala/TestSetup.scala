package modem

import chisel3.iotesters._
import dsptools._

object TestSetup {
  val testerOptionsFirrtl = TesterOptions(
    isVerbose = false,
    displayBase = 16,
    backendName = "firrtl"
  )
  val testerOptionsVerilog = TesterOptions(
    isVerbose = false,
    displayBase = 16,
    isGenVerilog = true,
    backendName = "verilator"
  )
  val dspTesterOptionsVerilog = new DspTesterOptionsManager {
    dspTesterOptions = dspTesterOptions.copy(
      isVerbose = false
    )
    testerOptions = testerOptionsVerilog
  }
  val dspTesterOptions = new DspTesterOptionsManager {
    dspTesterOptions = dspTesterOptions.copy(
      isVerbose = false
    )
    testerOptions = testerOptionsFirrtl
    //testerOptions = testerOptionsVerilog
  }
}
