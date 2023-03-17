package tinycc.common

import tinycc.common.ir.{IrPrinter, IrProgram}
import tinycc.common.transform.{AllocOrdering, BasicBlockScheduling, MemToReg, SingleFunExit}
import tinycc.util.Profiler.profile

class Optimizer(enableOptionalOptimizations: Boolean = false) extends ProgramTransform[IrProgram] {
  val allocOrdering = new AllocOrdering
  val basicBlockScheduling = new BasicBlockScheduling
  val memToReg = new MemToReg(removeLocals = true)
  val singleFunExit = new SingleFunExit

  val printer = new IrPrinter()

  override def transformProgram(program: IrProgram): Unit = {
    log("before\n" + printer.printToString(program))

    profile("basicBlockScheduling", basicBlockScheduling.transformProgram(program))
    profile("allocOrdering", allocOrdering.transformProgram(program))
    if (enableOptionalOptimizations) {
      profile("memToReg", memToReg.transformProgram(program))
      profile("singleFunExit", singleFunExit.transformProgram(program))
    }

    log("after\n" + printer.printToString(program))
  }
}
