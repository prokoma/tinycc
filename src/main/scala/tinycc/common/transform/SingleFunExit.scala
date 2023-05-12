package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.ir.IrManipulation.replaceInsnWith
import tinycc.common.ir.IrTy.VoidTy
import tinycc.common.ir._
import tinycc.util.Profiler.profile

/** If there are multiple blocks terminated by RetInsn or RetVoidInsn, create a new function exit block. */
class SingleFunExit extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("singleFunExit", {
    program.funs.foreach(transformFun)
  })

  def transformFun(fun: IrFun): Unit = {
    if (fun.returnTy == VoidTy) {
      val retVoidInsns = fun.insns.collect({ case insn: RetVoidInsn => insn })
      if (retVoidInsns.size > 1) {
        val exitBlock = fun.append(new BasicBlock("exit", fun))
        log(s"added new $exitBlock to $fun")
        exitBlock.append(new RetVoidInsn(exitBlock))
        retVoidInsns.foreach(insn => replaceInsnWith(insn, new BrInsn(exitBlock, insn.basicBlock)))
      }
    } else {
      val retInsns = fun.insns.collect({ case insn: RetInsn => insn })
      if (retInsns.size > 1) {
        val exitBlock = fun.append(new BasicBlock("exit", fun))
        log(s"added new $exitBlock to $fun")
        val phi = exitBlock.append(new PhiInsn(retInsns.map(insn => (Some(insn.arg), Some(insn.basicBlock))).toIndexedSeq, exitBlock))
        exitBlock.append(new RetInsn(phi, exitBlock))
        retInsns.foreach(insn => replaceInsnWith(insn, new BrInsn(exitBlock, insn.basicBlock)))
      }
    }
  }
}
