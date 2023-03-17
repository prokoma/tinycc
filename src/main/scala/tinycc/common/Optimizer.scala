package tinycc.common

import tinycc.common.ir.{IrPrinter, IrProgram}
import tinycc.common.transform.{AllocOrdering, BasicBlockScheduling, MemToReg, SingleFunExit}

class Optimizer extends ProgramTransform[IrProgram] {
  val allocOrdering = new AllocOrdering
  val basicBlockScheduling = new BasicBlockScheduling
  val memToReg = new MemToReg(removeLocals = true)
  val singleFunExit = new SingleFunExit

  val printer = new IrPrinter()

  override def transformProgram(program: IrProgram): Unit = {
    log("before\n" + printer.printToString(program))

    basicBlockScheduling.transformProgram(program)
    allocOrdering.transformProgram(program)
    memToReg.transformProgram(program)
    singleFunExit.transformProgram(program)

    log("after\n" + printer.printToString(program))
  }
}
