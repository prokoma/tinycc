package tinycc.backend

import tinycc.common.ir.{CallInsn, CallPtrInsn, Insn, IrOpcode}

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
    /** Number of covered instructions by this pattern. */
    def size: Int

    def ^^[B](f: A => B): Pat[B] = MapPat(this, f)
  }

  object Pat {
    case class Match[+A](value: A, coveredInsns: List[Insn], requiredInsns: List[(Var[_], Insn)]) {
      def copyWithCoveredInsn(v: Insn): Match[A] = this.copy(coveredInsns = v :: coveredInsns)

      def copyWithRequiredInsn(v: (Var[_], Insn)): Match[A] = this.copy(requiredInsns = v :: requiredInsns)
    }

    def joinMatches[A, B, C](a: Match[A], b: Match[B], f: (A, B) => C): Match[C] =
      Match(f(a.value, b.value), a.coveredInsns ++ b.coveredInsns, a.requiredInsns ++ b.requiredInsns)

    def apply[T <: Insn](op: IrOpcode): NullaryInsnPat[T] = NullaryInsnPat[T](op)

    def apply[T <: Insn, A](op: IrOpcode, operand: Pat[A]): UnaryInsnPat[T, A] = UnaryInsnPat[T, A](op, operand)

    def apply[T <: Insn, A, B](op: IrOpcode, left: Pat[A], right: Pat[B]): BinaryInsnPat[T, A, B] = BinaryInsnPat[T, A, B](op, left, right)
  }

  /** Matches a single instruction by its opcode and returns it. */
  case class NullaryInsnPat[T <: Insn](op: IrOpcode) extends Pat[T] {
    override def size: Int = 1

    override def apply(insn: Insn): Option[Pat.Match[T]] =
      if (insn.op == op) Some(Pat.Match(insn.asInstanceOf[T], List(insn), List.empty)) else None

    override def toString(): String = s"Pat($op)"
  }

  /** Matches an unary instruction by its opcode and operand, returns the instruction and value of the operand. */
  case class UnaryInsnPat[T <: Insn, A](op: IrOpcode, operand: Pat[A]) extends Pat[(T, A)] {
    override def size: Int = operand.size + 1

    override def apply(insn: Insn): Option[Pat.Match[(T, A)]] =
      if (insn.op == op && insn.operands.size == 1) {
        for (
          Pat.Match(value, coveredInsns, requiredInsns) <- operand(insn.operands(0))
        ) yield Pat.Match((insn.asInstanceOf[T], value), insn :: coveredInsns, requiredInsns)
      } else None

    override def toString(): String = s"Pat($op, $operand)"
  }

  /** Matches a binary instruction by its opcode and operands, returns the instruction and value of the operands. */
  case class BinaryInsnPat[T <: Insn, A, B](op: IrOpcode, left: Pat[A], right: Pat[B]) extends Pat[(T, A, B)] {
    override def size: Int = left.size + right.size + 1

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

    override def toString(): String = s"Pat($op, $left, $right)"
  }

  /** Matches a nonterminal, the value for it must be available. */
  case class VarPat[T](v: Var[T]) extends Pat[T] {
    override def size: Int = 0

    override def apply(insn: Insn): Option[Pat.Match[T]] =
      Some(Pat.Match(v.resolveValue(insn), Nil, List((v, insn))))

    override def toString(): String = s"VarPat($v)"
  }

  /** Transforms value of a pattern. */
  case class MapPat[T, U](pat: Pat[T], f: T => U) extends Pat[U] {
    override def size: Int = pat.size

    override def apply(insn: Insn): Option[Pat.Match[U]] = {
      for (
        Pat.Match(value, coveredInsns, requiredInsns) <- pat(insn)
      ) yield Pat.Match(f(value), coveredInsns, requiredInsns)
    }

    override def toString(): String = s"($pat ^^ $f)"
  }

  protected def matchIndexedSeq[A](seq: IndexedSeq[Insn], pat: Pat[A]): Option[Pat.Match[IndexedSeq[A]]] = {
    if (seq.isEmpty)
      Some(Pat.Match(IndexedSeq.empty, Nil, Nil))
    else {
      for (
        Pat.Match(headValue, headCoveredInsns, headRequiredInsns) <- pat(seq.head);
        Pat.Match(tailValue, tailCoveredInsns, tailRequiredInsns) <- matchIndexedSeq(seq.tail, pat)
      ) yield Pat.Match(headValue +: tailValue, headCoveredInsns ++ tailCoveredInsns, headRequiredInsns ++ tailRequiredInsns)
    }
  }

  case class CallInsnPat[A](arg: Pat[A]) extends Pat[(CallInsn, IndexedSeq[A])] {
    override def size: Int = arg.size + 1

    override def apply(insn: Insn): Option[Pat.Match[(CallInsn, IndexedSeq[A])]] =
      if (insn.op == IrOpcode.Call) {
        val callInsn = insn.asInstanceOf[CallInsn]
        for (
          Pat.Match(args, coveredInsns, requiredInsns) <- matchIndexedSeq(callInsn.args.map(_.get), arg)
        ) yield Pat.Match((callInsn, args), insn :: coveredInsns, requiredInsns)
      } else None

    override def toString(): String = s"CallInsnPat($arg)"
  }

  case class CallPtrInsnPat[A, B](ptr: Pat[A], arg: Pat[B]) extends Pat[(CallPtrInsn, A, IndexedSeq[B])] {
    override def size: Int = ptr.size + arg.size + 1

    override def apply(insn: Insn): Option[Pat.Match[(CallPtrInsn, A, IndexedSeq[B])]] =
      if (insn.op == IrOpcode.CallPtr) {
        val callInsn = insn.asInstanceOf[CallPtrInsn]
        for (
          Pat.Match(ptr, ptrCoveredInsns, ptrRequiredInsns) <- ptr(callInsn.funPtr.get);
          Pat.Match(args, argsCoveredInsns, argsRequiredInsns) <- matchIndexedSeq(callInsn.args.map(_.get), arg)
        ) yield Pat.Match((callInsn, ptr, args), insn :: ptrCoveredInsns ++ argsCoveredInsns, ptrRequiredInsns ++ argsRequiredInsns)
      } else None

    override def toString(): String = s"CallPtrInsnPat($arg)"
  }

  case class GenRule[T](v: Var[AsmEmitter[T]], rhs: Pat[AsmEmitter[T]]) extends (Insn => Option[GenRule.Match[T]]) {
    override def apply(insn: Insn): Option[GenRule.Match[T]] = {
      try
        rhs(insn).map(GenRule.Match(this, _))
      catch {
        case e: Throwable => throw new BackendException(s"Failed to match $this", e)
      }
    }

    override def toString(): String = s"($v -> $rhs)"
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
