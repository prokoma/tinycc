package tinycc.common

import tinycc.common.ir.{IrPrinter, IrProgram}
import tinycc.common.transform.{AllocOrdering, BasicBlockInlining, BasicBlockScheduling, MemToReg, SingleFunExit, StrengthReduction}
import tinycc.util.Profiler.profile

class Optimizer(enableOptionalOptimizations: Boolean = false) extends ProgramTransform[IrProgram] {
  val allocOrdering = new AllocOrdering
  val basicBlockScheduling = new BasicBlockScheduling
  val memToReg = new MemToReg(removeLocals = true)
  val singleFunExit = new SingleFunExit
  val strengthReduction = new StrengthReduction
  val basicBlockInlining = new BasicBlockInlining
  val constantPropagation = new ConstantPropagation

  val printer = new IrPrinter()

  override def transformProgram(program: IrProgram): Unit = {
    log("before\n" + printer.printToString(program))

    runPass(program, basicBlockScheduling)
    runPass(program, allocOrdering)
    if (enableOptionalOptimizations) {
      runPass(program, singleFunExit)
      runPass(program, strengthReduction)
//      runPass(program, basicBlockInlining)
      runPass(program, basicBlockScheduling)
      runPass(program, memToReg)
      runPass(program, constantPropagation)
      runPass(program, strengthReduction)
      runPass(program, constantPropagation)
//      runPass(program, basicBlockInlining)
      runPass(program, basicBlockScheduling)
    }

    log("after\n" + printer.printToString(program))
  }

  private def runPass(program: IrProgram, opt: ProgramTransform[IrProgram]): Unit = {
    opt.transformProgram(program)
    program.validate()
  }
}
