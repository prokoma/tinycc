package tinycc.common.ir.graph

import tinycc.common.Cfg
import tinycc.common.ir.{Insn, IrFun, IrProgram, TerminatorInsn}

trait InsnCfg extends Cfg[Insn] {
  override def getSucc(node: Insn): Seq[Insn] = node match {
    case insn: TerminatorInsn => insn.succBlocks.map(_.body.head)
    case insn => Seq.from(insn.succ)
  }

  override def getPred(node: Insn): Seq[Insn] = node.pred match {
    case Some(insn) => Seq(insn)
    case None => node.basicBlock.pred.map(_.terminator)
  }
}

object InsnCfg {
  def apply(irFun: IrFun): InsnCfg = new InsnCfg {
    override def nodes: Seq[Insn] = irFun.insns

    override def entryNodes: Seq[Insn] = Seq(irFun.entryPoint)

    // exitPoints are terminators, so there can't be multiple in a basic block
    override def exitNodes: Seq[Insn] = irFun.exitPoints
  }

  def apply(program: IrProgram): InsnCfg = new InsnCfg {
    override def nodes: Seq[Insn] = program.insns

    override def entryNodes: Seq[Insn] = program.funs.map(_.entryPoint)

    // exitPoints are terminators, so there can't be multiple in a basic block
    override def exitNodes: Seq[Insn] = program.funs.flatMap(_.exitPoints)
  }
}
