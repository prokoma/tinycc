package tinycc.backend.t86

import tinycc.backend.t86.insel.T86InstructionSelection
import tinycc.backend.t86.regalloc.T86RegisterAllocator
import tinycc.common.ir.IrProgram
import tinycc.util.Logging
import tinycc.util.Profiler.profile

class T86Backend(program: IrProgram) extends Logging {
  def result(): T86Listing = {
    val t86Program = profile("instructionSelection", T86InstructionSelection(program).result())

    log("before register allocation:\n" + new T86AsmPrinter().printToString(t86Program.flatten))

    profile("registerAllocation", T86RegisterAllocator().transformProgram(t86Program))
    new T86FunProcessor().transformProgram(t86Program)

    new T86LabelProcessor(t86Program.flatten).result()
  }

  def resultAsString(): String = new T86AsmPrinter().printToString(result())
}
