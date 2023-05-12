package tinycc.common.ir

import tinycc.common.Graph

object InsnDfg {
  def apply(insns: Seq[Insn]): Graph[Insn] = new Graph[Insn] {
    override def nodes: Seq[Insn] = insns

    override def getSucc(node: Insn): Seq[Insn] = node.uses.collect({ case OperandRef(owner, _) => owner }).toSeq

    override def getPred(node: Insn): Seq[Insn] = node.operands
  }

  def apply(program: IrProgram): Graph[Insn] = InsnDfg(program.insns.toSeq)
}
