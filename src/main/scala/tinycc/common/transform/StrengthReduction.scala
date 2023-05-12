package tinycc.common.transform

import tinycc.common.PhiHelper.removeBlockPhiUsesIn
import tinycc.common.ProgramTransform
import tinycc.common.ir.IrManipulation.{insertInsnBefore, replaceInsnWith}
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir._
import tinycc.util.Profiler.profile

class StrengthReduction extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("strengthReduction", {
    program.insns.foreach(optimizeInsn)
  })

  private def prependIImm(value: Long, before: Insn): IImmInsn =
    insertInsnBefore(new IImmInsn(value, before.basicBlock), before)

  private def replaceInsnWithBinaryArith(insn: Insn, op: BinaryArithOp, left: Insn, right: Insn): BinaryArithInsn =
    replaceInsnWith(insn, new BinaryArithInsn(op, left, right, insn.basicBlock))

  private def replaceInsnWithIImm(insn: Insn, value: Long): IImmInsn =
    replaceInsnWith(insn, new IImmInsn(value, insn.basicBlock))

  private def optimizeInsn(insn: Insn): Unit = (insn.op, insn.operands) match {
    // x + 0 = x
    case (IAdd, Seq(x, IImmInsn(0))) =>
      log(s"replaced $insn x + 0 = x")
      insn.replaceUses(x)

    // 0 + x = x
    case (IAdd, Seq(IImmInsn(0), x)) =>
      log(s"replaced $insn 0 + x = x")
      insn.replaceUses(x)

    // x + x = x << 1
    case (IAdd, Seq(x, x2)) if x == x2 =>
      log(s"replaced $insn x + x = x << 1")
      replaceInsnWithBinaryArith(insn, IShl, x, prependIImm(1, insn))

    // x - 0 = x
    case (ISub, Seq(x, IImmInsn(0))) =>
      log(s"replaced $insn x - 0 = x")
      insn.replaceUses(x)

    // x - x = 0
    case (ISub, Seq(x, x2)) if x == x2 =>
      log(s"replaced $insn x - x = 0")
      replaceInsnWithIImm(insn, 0)

    // x * 1 = x
    case (UMul | SMul, Seq(x, IImmInsn(1))) =>
      log(s"replaced $insn x * 1 = x")
      insn.replaceUses(x)

    // 1 * x = x
    case (UMul | SMul, Seq(IImmInsn(1), x)) =>
      log(s"replaced $insn 1 * x = x")
      insn.replaceUses(x)

    // x * (-1) = 0 - x
    case (SMul, Seq(x, IImmInsn(-1))) =>
      log(s"replaced $insn x * (-1) = 0 - x")
      replaceInsnWithBinaryArith(insn, ISub, prependIImm(0, insn), x)

    // (-1) * x = 0 - x
    case (SMul, Seq(IImmInsn(-1), x)) =>
      log(s"replaced $insn (-1) * x = 0 - x")
      replaceInsnWithBinaryArith(insn, ISub, prependIImm(0, insn), x)

    // x / 1 = x
    case (UDiv | SDiv, Seq(x, IImmInsn(1))) =>
      log(s"replaced $insn x / 1 = x")
      insn.replaceUses(x)

    // x / (-1) = 0 - x
    case (SDiv, Seq(x, IImmInsn(-1))) =>
      log(s"replaced $insn x / (-1) = 0 - x")
      replaceInsnWithBinaryArith(insn, ISub, prependIImm(0, insn), x)

    // x >> 0 = x or x << 0 = x
    case (IShr | IShl, Seq(x, IImmInsn(0))) =>
      log(s"replaced $insn x >> 0 = x or x << 0 = x")
      insn.replaceUses(x)

    // x | x = x or x & x = x
    case (IOr | IAnd, Seq(x, y)) if x == y =>
      log(s"replaced $insn x | x = x or x & x = x")
      insn.replaceUses(x)

    // x ^ x = 0
    case (IXor, Seq(x, y)) if x == y =>
      log(s"replaced $insn x ^ x = 0")
      replaceInsnWith(insn, new IImmInsn(0, insn.basicBlock))

    // x * (2^y) = x << y
    case (SMul | UMul, Seq(x, IImmInsn(y))) if isPowerOfTwo(y) =>
      log(s"replaced $insn x * (2^y) = x << y")
      replaceInsnWithBinaryArith(insn, IShl, x, prependIImm(log2(y), insn))

    // (2^y) * x = x << y
    case (SMul | UMul, Seq(IImmInsn(y), x)) if isPowerOfTwo(y) =>
      log(s"replaced $insn (2^y) * x = x << y")
      replaceInsnWithBinaryArith(insn, IShl, x, prependIImm(log2(y), insn))

    // x / (2^y) = x >> y (unsigned only)
    case (UDiv, Seq(x, IImmInsn(y))) if isPowerOfTwo(y) =>
      log(s"replaced $insn x / (2^y) = x >> y")
      replaceInsnWithBinaryArith(insn, IShr, x, prependIImm(log2(y), insn))

    // x / x = 1
    case (SDiv | UDiv, Seq(x, x2)) if x == x2 =>
      log(s"replaced $insn x / x = 1")
      replaceInsnWithIImm(insn, 1)

    case _ => insn match {
      case insn: CondBrInsn => optimizeCondBr(insn)

      case _ =>
    }
  }

  private def optimizeCondBr(insn: CondBrInsn): Unit = insn.arg match {
    case BinaryInsn(CmpINe, x, IImmInsn(0)) =>
      log(s"replaced $insn x != 0 => x")
      insn.argRef(x)

    case BinaryInsn(CmpINe, IImmInsn(0), x) =>
      log(s"replaced $insn 0 != x => x")
      insn.argRef(x)

    case BinaryInsn(CmpIEq, x, IImmInsn(0)) =>
      log(s"replaced $insn x == 0 => !x")
      replaceInsnWith(insn, new CondBrInsn(x, insn.falseBlock, insn.trueBlock, insn.basicBlock))

    case BinaryInsn(CmpIEq, IImmInsn(0), x) =>
      log(s"replaced $insn 0 == x => !x")
      replaceInsnWith(insn, new CondBrInsn(x, insn.falseBlock, insn.trueBlock, insn.basicBlock))

    case IImmInsn(0) =>
      log(s"replaced $insn always 0")
      removeBlockPhiUsesIn(insn.basicBlock, insn.trueBlock)
      replaceInsnWith(insn, new BrInsn(insn.falseBlock, insn.basicBlock))

    case IImmInsn(_) =>
      log(s"replaced $insn always !0")
      removeBlockPhiUsesIn(insn.basicBlock, insn.trueBlock)
      replaceInsnWith(insn, new BrInsn(insn.trueBlock, insn.basicBlock))

    case _ =>
  }

  private def isPowerOfTwo(l: Long): Boolean =
    java.lang.Long.highestOneBit(l) == l

  private def log2(l: Long): Long =
    63 - java.lang.Long.numberOfLeadingZeros(l)
}
