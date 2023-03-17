package tinycc.common

import tinycc.common.ir.{IrPrinter, IrProgram}
import tinycc.common.transform.{AllocOrdering, BasicBlockScheduling, MemToReg, SingleFunExit}
import tinycc.util.Profiler.theProfiler

class Optimizer extends ProgramTransform[IrProgram] {
  val allocOrdering = new AllocOrdering
  val basicBlockScheduling = new BasicBlockScheduling
  val memToReg = new MemToReg(removeLocals = true)
  val singleFunExit = new SingleFunExit

  val printer = new IrPrinter()

  override def transformProgram(program: IrProgram): Unit = {
    log("before\n" + printer.printToString(program))

    theProfiler.profile("basicBlockScheduling", basicBlockScheduling.transformProgram(program))
    theProfiler.profile("allocOrdering", allocOrdering.transformProgram(program))
    theProfiler.profile("memToReg", memToReg.transformProgram(program))
    theProfiler.profile("singleFunExit", singleFunExit.transformProgram(program))

    log("after\n" + printer.printToString(program))
  }
}
