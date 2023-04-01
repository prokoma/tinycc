package tinycc.common.ir

import tinycc.common.Cfg

object BasicBlockCfg {
  def apply(irFun: IrFun): Cfg[BasicBlock] = new Cfg[BasicBlock] {
    override def nodes: Seq[BasicBlock] = irFun.basicBlocks

    override def entry: BasicBlock = irFun.entryBlock

    // exitPoints are terminators, so there can't be multiple in a basic block
    override def exit: Seq[BasicBlock] = irFun.exitPoints.map(_.basicBlock)

    override def getSucc(node: BasicBlock): Seq[BasicBlock] = node.succ

    override def getPred(node: BasicBlock): Seq[BasicBlock] = node.pred
  }
}
