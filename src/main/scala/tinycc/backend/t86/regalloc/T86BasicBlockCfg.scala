package tinycc.backend.t86.regalloc

import tinycc.backend.t86.{T86BasicBlock, T86Fun}
import tinycc.common.Graph
import tinycc.common.ir.{BasicBlock, IrFun}

// allows us to get entry node, exit nodes, successors and predecessors
trait T86BasicBlockCfg extends Graph[T86BasicBlock] {
  def nodes: Seq[T86BasicBlock]

  def entry: T86BasicBlock

  def exit: Seq[T86BasicBlock]

  def getSucc(block: T86BasicBlock): Seq[T86BasicBlock]

  def getPred(block: T86BasicBlock): Seq[T86BasicBlock]
}

object T86BasicBlockCfg {
  def apply(fun: T86Fun, irFun: IrFun, basicBlockMap: Map[BasicBlock, T86BasicBlock]): T86BasicBlockCfg = {
    val invBasicBlockMap = basicBlockMap.map(_.swap)
    // TODO: maybe build the graph beforehand, so map lookups and converting to seq are not necessary
    new T86BasicBlockCfg {
      override def nodes: Seq[T86BasicBlock] = fun.basicBlocks

      override def entry: T86BasicBlock = basicBlockMap(irFun.entryBlock)

      override def exit: Seq[T86BasicBlock] = irFun.exitPoints.map(insn => basicBlockMap(insn.basicBlock))

      override def getSucc(block: T86BasicBlock): Seq[T86BasicBlock] = invBasicBlockMap(block).succ.map(basicBlockMap(_))

      override def getPred(block: T86BasicBlock): Seq[T86BasicBlock] = invBasicBlockMap(block).pred.map(basicBlockMap(_))
    }
  }
}
