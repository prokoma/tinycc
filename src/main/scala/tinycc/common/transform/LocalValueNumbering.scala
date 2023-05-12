package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.ir.IrOpcode.{AllocG, AllocL, Load}
import tinycc.common.ir._
import tinycc.util.Profiler.profile

import scala.collection.mutable

class LocalValueNumbering extends ProgramTransform[IrProgram] {

  import LocalValueNumbering._

  override def transformProgram(program: IrProgram): Unit = profile("localValueNumbering", {
    program.basicBlocks.foreach(transformBlock)
  })

  private def transformBlock(block: BasicBlock): Unit = {
    val values = mutable.Map.empty[InsnSig, Insn]

    for (insn <- block.body if canOptimize(insn)) {
      val sig = InsnSig(insn)
      values.get(sig) match {
        case Some(prevInsn) =>
          log(s"replaced $insn with $prevInsn in ${insn.basicBlock}")
          insn.replaceUses(prevInsn)
          insn.remove()

        case None =>
          values(sig) = insn
      }
    }
  }
}

object LocalValueNumbering {
  sealed trait InsnSig extends Product with Serializable

  case class BasicInsnSig(op: IrOpcode, operands: Seq[Insn]) extends InsnSig

  case class IImmSig(value: Long) extends InsnSig

  case class FImmSig(value: Double) extends InsnSig

  case class LoadArgSig(index: Int) extends InsnSig

  case class GetElementPtrSig(ptr: Insn, index: Insn, elemTy: IrTy, fieldIndex: Int) extends InsnSig

  case class SizeOfSig(varTy: IrTy) extends InsnSig

  case class GetFunPtrSig(targetFun: IrFun) extends InsnSig

  case class PhiSig(args: Set[(Insn, BasicBlock)]) extends InsnSig

  object InsnSig {
    def apply(insn: Insn): InsnSig = insn match {
      case insn: IImmInsn => IImmSig(insn.value)
      case insn: FImmInsn => FImmSig(insn.value)
      case insn: GetElementPtrInsn => GetElementPtrSig(insn.ptr, insn.index, insn.elemTy, insn.fieldIndex)
      case insn: SizeOfInsn => SizeOfSig(insn.varTy)
      case insn: GetFunPtrInsn => GetFunPtrSig(insn.targetFun)
      case insn: LoadArgInsn => LoadArgSig(insn.index)
      case insn: PhiInsn => PhiSig(insn.args.toSet)

      case insn => BasicInsnSig(insn.op, insn.operands)
    }
  }

  private def canOptimize(insn: Insn): Boolean = {
    !insn.hasSideEffects && insn.op != AllocL && insn.op != AllocG && insn.op != Load
  }
}
