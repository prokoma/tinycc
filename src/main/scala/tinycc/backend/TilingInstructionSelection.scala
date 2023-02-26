package tinycc.backend

import tinycc.common.ir.{Insn, IrOpcode}

import scala.language.implicitConversions

trait TilingInstructionSelection {
  type Context

  type AsmEmitter[+A] = Context => A

  /** A nonterminal (LHS of RewriteRule). */
  trait Var[T] {
    type ValueTy = T

    def resolveValue(insn: Insn): T
  }

  /** A pattern, which matches a tree of instructions. */
  trait Pat[+A] extends (Insn => Option[Pat.Match[A]]) {
    def ^^[B](f: A => B): Pat[B] = MapPat(this, f)
  }

  object Pat {
    case class Match[+A](value: A, coveredInsns: List[Insn], requiredInsns: List[(Var[_], Insn)])

    def apply[T <: Insn](op: IrOpcode): NullaryInsnPat[T] = NullaryInsnPat[T](op)

    def apply[T <: Insn, A](op: IrOpcode, operand: Pat[A]): UnaryInsnPat[T, A] = UnaryInsnPat[T, A](op, operand)

    def apply[T <: Insn, A, B](op: IrOpcode, left: Pat[A], right: Pat[B]): BinaryInsnPat[T, A, B] = BinaryInsnPat[T, A, B](op, left, right)
  }

  /** Matches a single instruction by its opcode and returns it. */
  case class NullaryInsnPat[T <: Insn](op: IrOpcode) extends Pat[T] {
    override def apply(insn: Insn): Option[Pat.Match[T]] =
      if (insn.op == op) Some(Pat.Match(insn.asInstanceOf[T], List(insn), List.empty)) else None
  }

  /** Matches an unary instruction by its opcode and operand, returns the instruction and value of the operand. */
  case class UnaryInsnPat[T <: Insn, A](op: IrOpcode, operand: Pat[A]) extends Pat[(T, A)] {
    override def apply(insn: Insn): Option[Pat.Match[(T, A)]] =
      if (insn.op == op && insn.operands.size == 1) {
        for (
          Pat.Match(value, coveredInsns, requiredInsns) <- operand(insn.operands(0))
        ) yield Pat.Match((insn.asInstanceOf[T], value), insn :: coveredInsns, requiredInsns)
      } else None
  }

  /** Matches a binary instruction by its opcode and operands, returns the instruction and value of the operands. */
  case class BinaryInsnPat[T <: Insn, A, B](op: IrOpcode, left: Pat[A], right: Pat[B]) extends Pat[(T, A, B)] {
    override def apply(insn: Insn): Option[Pat.Match[(T, A, B)]] =
      if (insn.op == op && insn.operands.size == 2)
        for (
          Pat.Match(leftValue, leftCoveredInsns, leftRequiredInsns) <- left(insn.operands(0));
          Pat.Match(rightValue, rightCoveredInsns, rightRequiredInsns) <- right(insn.operands(1))
        ) yield Pat.Match(
          (insn.asInstanceOf[T], leftValue, rightValue),
          insn :: (leftCoveredInsns ++ rightCoveredInsns),
          leftRequiredInsns ++ rightRequiredInsns)
      else None
  }

  /** Matches a nonterminal, the value for it must be available. */
  case class VarPat[T](v: Var[T]) extends Pat[T] {
    override def apply(insn: Insn): Option[Pat.Match[T]] =
      Some(Pat.Match(v.resolveValue(insn), Nil, List((v, insn))))
  }

  /** Transforms value of a pattern. */
  case class MapPat[T, U](node: Pat[T], f: T => U) extends Pat[U] {
    override def apply(insn: Insn): Option[Pat.Match[U]] = {
      for (
        Pat.Match(value, coveredInsns, requiredInsns) <- node(insn)
      ) yield Pat.Match(f(value), coveredInsns, requiredInsns)
    }
  }

  case class GenRule[T](v: Var[AsmEmitter[T]], rhs: Pat[AsmEmitter[T]]) extends (Insn => Option[GenRule.Match[T]]) {
    override def apply(insn: Insn): Option[GenRule.Match[T]] = rhs(insn).map(GenRule.Match(this, _))
  }

  object GenRule {
    case class Match[T](rule: GenRule[T], patMatch: Pat.Match[AsmEmitter[T]]) {
      def value: AsmEmitter[T] = patMatch.value

      def coveredInsns: List[Insn] = patMatch.coveredInsns

      def requiredInsns: List[(Var[_], Insn)] = patMatch.requiredInsns
    }
  }

  //  def ->[T](v: Var[AsmEmitter[T]], rhs: Pat[AsmEmitter[T]]): RewriteRule[T] = RewriteRule(v, rhs)

  implicit def var2pat[T](v: Var[T]): VarPat[T] = VarPat(v)
}
