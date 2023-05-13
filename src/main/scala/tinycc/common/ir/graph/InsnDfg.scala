package tinycc.common.ir.graph

import tinycc.common.Graph
import tinycc.common.ir.{Insn, IrProgram, OperandRef}

object InsnDfg {
  def apply(insns: Seq[Insn]): Graph[Insn] = new Graph[Insn] {
    override def nodes: Seq[Insn] = insns

    override def getSucc(node: Insn): Seq[Insn] = node.uses.collect({ case OperandRef(owner, _) => owner }).toSeq

    override def getPred(node: Insn): Seq[Insn] = node.operands
  }

  def apply(program: IrProgram): Graph[Insn] = InsnDfg(program.insns)
}
