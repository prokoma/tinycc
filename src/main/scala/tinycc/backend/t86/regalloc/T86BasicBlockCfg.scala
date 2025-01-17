package tinycc.backend.t86.regalloc

import tinycc.backend.t86.{T86BasicBlock, T86Fun}
import tinycc.common.Cfg

object T86BasicBlockCfg {
  /** Construct T86BasicBlockCfg from IR basic block control flow graph. This requires that all ASM basic blocks have irBasicBlock set. */
  def apply(fun: T86Fun): Cfg[T86BasicBlock] = {
    val Some(irFun) = fun.irFun
    val basicBlockMap = fun.basicBlocks.map(bb => (bb.irBasicBlock, bb)).collect({ case (Some(irBasicBlock), bb) => irBasicBlock -> bb }).toMap

    new Cfg[T86BasicBlock] {
      override def nodes: Seq[T86BasicBlock] = fun.basicBlocks

      override def entryNodes: Seq[T86BasicBlock] = Seq(basicBlockMap(irFun.entryBlock))

      override def exitNodes: Seq[T86BasicBlock] = irFun.exitPoints.map(insn => basicBlockMap(insn.basicBlock))

      override def getSucc(block: T86BasicBlock): Seq[T86BasicBlock] = block.irBasicBlock.get.succ.map(basicBlockMap)

      override def getPred(block: T86BasicBlock): Seq[T86BasicBlock] = block.irBasicBlock.get.pred.map(basicBlockMap)
    }
  }
}