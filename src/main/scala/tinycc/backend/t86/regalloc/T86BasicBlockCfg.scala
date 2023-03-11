package tinycc.backend.t86.regalloc

import tinycc.backend.t86.{T86BasicBlock, T86Fun}
import tinycc.common.Cfg

// allows us to get entry node, exit nodes, successors and predecessors
trait T86BasicBlockCfg extends Cfg[T86BasicBlock] {
  def isInLoop(block: T86BasicBlock): Boolean
}

object T86BasicBlockCfg {
  /** Construct T86BasicBlockCfg from IR basic block control flow graph. This requires that all ASM basic blocks have irBasicBlock set. */
  def apply(fun: T86Fun): T86BasicBlockCfg = {
    val Some(irFun) = fun.irFun
    val basicBlockMap = fun.basicBlocks.map(bb => (bb.irBasicBlock, bb)).collect({ case (Some(irBasicBlock), bb) => irBasicBlock -> bb }).toMap

    new T86BasicBlockCfg {
      override def nodes: Seq[T86BasicBlock] = fun.basicBlocks

      override def entry: T86BasicBlock = basicBlockMap(irFun.entryBlock)

      override def exit: Seq[T86BasicBlock] = irFun.exitPoints.map(insn => basicBlockMap(insn.basicBlock))

      override def getSucc(block: T86BasicBlock): Seq[T86BasicBlock] = block.irBasicBlock.get.succ.map(basicBlockMap)

      override def getPred(block: T86BasicBlock): Seq[T86BasicBlock] = block.irBasicBlock.get.pred.map(basicBlockMap)

      override def isInLoop(block: T86BasicBlock): Boolean = getPred(block).size > 1 // TODO: DFS, move to dedicated class and get rid of T86BasicBlockCfg
    }
  }
}