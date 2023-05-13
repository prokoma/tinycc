package tinycc.common.ir.graph

import tinycc.common.Cfg
import tinycc.common.ir.{BasicBlock, IrFun, IrProgram}

trait BasicBlockCfg extends Cfg[BasicBlock] {
  override def getSucc(node: BasicBlock): Seq[BasicBlock] = node.succ

  override def getPred(node: BasicBlock): Seq[BasicBlock] = node.pred
}

object BasicBlockCfg {
  def apply(irFun: IrFun): BasicBlockCfg = new BasicBlockCfg {
    override def nodes: Seq[BasicBlock] = irFun.basicBlocks

    override def entryNodes: Seq[BasicBlock] = Seq(irFun.entryBlock)

    // exitPoints are terminators, so there can't be multiple in a basic block
    override def exitNodes: Seq[BasicBlock] = irFun.exitPoints.map(_.basicBlock)
  }

  def apply(program: IrProgram): BasicBlockCfg = new BasicBlockCfg {
    override def nodes: Seq[BasicBlock] = program.basicBlocks

    override def entryNodes: Seq[BasicBlock] = program.funs.map(_.entryBlock)

    // exitPoints are terminators, so there can't be multiple in a basic block
    override def exitNodes: Seq[BasicBlock] = program.funs.flatMap(_.exitPoints).map(_.basicBlock)
  }
}
