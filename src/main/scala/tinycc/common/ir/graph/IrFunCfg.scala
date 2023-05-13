package tinycc.common.ir.graph

import tinycc.common.Cfg
import tinycc.common.ir.{EntryFunRef, IrFun, IrProgram, OperandFunRef}

object IrFunCfg {
//  def apply(program: IrProgram): Cfg[IrFun] = new Cfg[IrFun] {
//    override def entryNodes: Seq[IrFun] = Seq(program.entryFun)
//
//    override def exitNodes: Seq[IrFun] = ???
//
//    override def nodes: Seq[IrFun] = program.funs
//
//    override def getSucc(node: IrFun): Seq[IrFun] = node.insns.collect({
//      case
//    })
//
//    override def getPred(node: IrFun): Seq[IrFun] = node.uses.collect({
//      case ref: OperandFunRef => ref.owner.fun
//    }).toSeq
//  }
}
