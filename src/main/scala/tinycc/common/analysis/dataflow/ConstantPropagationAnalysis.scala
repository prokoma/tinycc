package tinycc.common.analysis.dataflow

import tinycc.common.Graph
import tinycc.common.ir.{IImmInsn, Insn}
import tinycc.common.ir.IrOpcode._

class ConstantPropagationAnalysis(dfg: Graph[Insn]) extends DataflowAnalysis.Builder[Insn](dfg, true)
  with FixpointComputation.Worklist {

  import ConstantPropagationAnalysis._

  override type NodeState = NodeVal

  override def nodeStateLattice: Lattice[NodeVal] = constLattice

  override def transfer(node: Insn, progState: ProgState): NodeVal = (node.op, node.operands.map(progState.apply)) match {
    case (IImm, Seq()) => Const(node.asInstanceOf[IImmInsn].value)

    case (IAdd, Seq(Const(x), Const(y))) => Const(x + y)
    case (ISub, Seq(Const(x), Const(y))) => Const(x - y)
    case (IAnd, Seq(Const(x), Const(y))) => Const(x & y)
    case (IOr, Seq(Const(x), Const(y))) => Const(x | y)
    case (IXor, Seq(Const(x), Const(y))) => Const(x ^ y)
    case (IShl, Seq(Const(x), Const(y))) => Const(x << y)
    case (IShr, Seq(Const(x), Const(y))) => Const(x >> y)
    case (UMul | SMul, Seq(Const(x), Const(y))) => Const(x * y)
    case (UDiv, Seq(Const(x), Const(y))) => Const(java.lang.Long.divideUnsigned(x, y))
    case (SDiv, Seq(Const(x), Const(y))) => Const(x / y)

    case (CmpIEq, Seq(Const(l), Const(r))) => Const(if (l == r) 1 else 0)
    case (CmpIEq, Seq(Const(0), NonZero) | Seq(NonZero, Const(0))) => Const(0)
    case (CmpINe, Seq(Const(l), Const(r))) => Const(if (l != r) 1 else 0)
    case (CmpINe, Seq(Const(0), NonZero) | Seq(NonZero, Const(0))) => Const(1)
    case (CmpULt, Seq(Const(l), Const(r))) => Const(if (java.lang.Long.compareUnsigned(l, r) < 0) 1 else 0)
    case (CmpULt, Seq(_, Const(0))) => Const(0)
    case (CmpULe, Seq(Const(l), Const(r))) => Const(if (java.lang.Long.compareUnsigned(l, r) <= 0) 1 else 0)
    case (CmpULt, Seq(Const(0), _)) => Const(1)
    case (CmpUGt, Seq(Const(l), Const(r))) => Const(if (java.lang.Long.compareUnsigned(l, r) > 0) 1 else 0)
    case (CmpUGt, Seq(Const(0), _)) => Const(0)
    case (CmpUGe, Seq(Const(l), Const(r))) => Const(if (java.lang.Long.compareUnsigned(l, r) >= 0) 1 else 0)
    case (CmpUGe, Seq(_, Const(0))) => Const(1)
    case (CmpSGe, Seq(Const(l), Const(r))) => Const(if (l >= r) 1 else 0)
    case (CmpSGt, Seq(Const(l), Const(r))) => Const(if (l > r) 1 else 0)
    case (CmpSLe, Seq(Const(l), Const(r))) => Const(if (l <= r) 1 else 0)
    case (CmpSLt, Seq(Const(l), Const(r))) => Const(if (l < r) 1 else 0)

    case (Phi, _) => join(node, progState)

    case _ => Top
  }

  def result(): Map[Insn, NodeVal] = fixpoint()
}

object ConstantPropagationAnalysis {
  sealed trait NodeVal extends Product with Serializable

  case object Top extends NodeVal

  case object Bot extends NodeVal

  case object NonZero extends NodeVal

  case class Const(v: Long) extends NodeVal

  lazy val constLattice = new Lattice[NodeVal] {
    override def top: NodeVal = Top

    override def bot: NodeVal = Bot

    override def lub(x: NodeVal, y: NodeVal): NodeVal = (x, y) match {
      case (x, y) if x == y => x
      case (Top, _) | (_, Top) => Top
      case (Bot, y) => y
      case (x, Bot) => x
      case (Const(0), Const(_) | NonZero) => Top
      case (Const(_) | NonZero, Const(0)) => Top
      case (Const(_) | NonZero, Const(_) | NonZero) => NonZero
    }

    override def glb(x: NodeVal, y: NodeVal): NodeVal = (x, y) match {
      case (x, y) if x == y => x
      case (Bot, _) | (_, Bot) => Bot
      case (Top, y) => y
      case (x, Top) => x
      case (Const(0), Const(_) | NonZero) => Bot
      case (Const(_) | NonZero, Const(0)) => Bot
      case (Const(_) | NonZero, Const(_) | NonZero) => Bot
    }
  }
}
