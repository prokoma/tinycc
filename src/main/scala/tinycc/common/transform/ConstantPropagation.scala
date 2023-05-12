package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.analysis.dataflow.ConstantPropagationAnalysis
import tinycc.common.ir.IrManipulation.{insertInsnBefore, replaceInsnWith}
import tinycc.common.ir.{CondBrInsn, IImmInsn, InsnDfg, IrProgram}
import tinycc.util.Profiler.profile

class ConstantPropagation extends ProgramTransform[IrProgram] {

  import ConstantPropagationAnalysis._

  override def transformProgram(program: IrProgram): Unit = profile("constPropagation", {
    val constValues = new ConstantPropagationAnalysis(InsnDfg(program)).result()

    program.insns.foreach({
      case insn: CondBrInsn => constValues.getOrElse(insn.arg, Top) match {
        case NonZero =>
          log(s"replaced always non-zero condition ${insn.arg} of $insn in ${insn.basicBlock}")
          insn.argRef(insertInsnBefore(new IImmInsn(1, insn.basicBlock), insn))
        case _ =>
      }

      case _: IImmInsn =>

      case insn => constValues(insn) match {
        case Const(v) =>
          log(s"replaced $insn in ${insn.basicBlock} with constant $v")
          replaceInsnWith(insn, new IImmInsn(v, insn.basicBlock))

        case _ =>
      }
    })
  })
}
