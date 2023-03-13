package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.ir._

/**
 * A pass used to aid instruction selection implementations working in linear program order. Ensures that it visits all variables before accessing them.
 *
 * 1. Move entryFun at the start of the program
 * 2. Move all [[AllocGInsn]] to entryBlock of entryFun
 * 3. Move all [[AllocLInsn]] and [[LoadArgInsn]] to the start of entryBlock of its function
 * 4. Reorder all [[AllocGInsn]], [[AllocLInsn]], [[LoadArgInsn]] in this order in the entryBlock
 */
class AllocOrdering extends ProgramTransform[IrProgram] {
  import IrManipulation._

  override def transformProgram(program: IrProgram): Unit = {
    val entryFun = program.entryFun
    if (entryFun != program.funs.head)
      program.funs = entryFun +: program.funs.filterNot(_ == entryFun) // 1.

    val entryFunBlock = entryFun.entryBlock
    program.globals.foreach(allocg => {
      if (allocg.basicBlock != entryFunBlock)
        appendInsnTo(allocg, entryFunBlock) // 2.
    })

    program.funs.foreach(transformFun)
  }

  def transformFun(fun: IrFun): Unit = {
    val entryBlock = fun.entryBlock
    fun.insns.foreach({
      case insn@(_: AllocLInsn | _: LoadArgInsn) if insn.basicBlock != entryBlock =>
        appendInsnTo(insn, entryBlock) // 3.

      case _ =>
    })

    val allocgList = IndexedSeq.newBuilder[AllocGInsn]
    val alloclList = IndexedSeq.newBuilder[AllocLInsn]
    val loadargList = IndexedSeq.newBuilder[LoadArgInsn]
    val restList = IndexedSeq.newBuilder[Insn]

    entryBlock.body.foreach({
      case insn: AllocGInsn => allocgList += insn
      case insn: AllocLInsn => alloclList += insn
      case insn: LoadArgInsn => loadargList += insn
      case insn => restList += insn
    })
    entryBlock.body = allocgList.result() ++ alloclList.result() ++ loadargList.result() ++ restList.result() // 4.
  }
}

