package tinycc.backend

import tinycc.common.ir.{CallInsn, CallPtrInsn, Insn, IrFun, IrOpcode, PhiInsn}

import scala.language.implicitConversions

trait TilingInstructionSelection {
  type Context

  type AsmEmitter[+A] = Context => A

  /** A nonterminal (LHS of RewriteRule). */
  trait Var[+T] {
    def resolveValue(insn: Insn): T
  }

  /** A pattern, which matches a tree of instructions. */
  trait Pat[+A] extends (Insn => Option[Pat.Match[A]]) {
    /** Number of covered instructions by this pattern. */
    def size: Int

    /** Cost is used to break ties between patterns with the same size (number of covered insns). Higher cost should mean longer execution time. */
    def cost: Int

    /** Returns a list of patterns without alternatives. */
    def flatten: Iterable[Pat[A]]

    def ^^[B](f: A => B): Pat[B] = MapPat(this, f)

    def |[B >: A](pat: Pat[B]): Pat[B] = OrPat(this, pat)

    def filter(f: A => Boolean): Pat[A] = FilterPat(this, (m: Pat.Match[A]) => f(m.value))

    def filterMatch(f: Pat.Match[A] => Boolean): Pat[A] = FilterPat(this, f)

    def cost(by: Int): Pat[A] = IncreaseCostPat(this, by)
  }

  object Pat {
    case class Match[+A](value: A, coveredInsns: List[Insn], requiredInsns: List[(Var[_], Insn)]) {
      def copyWithCoveredInsn(v: Insn): Match[A] = this.copy(coveredInsns = v :: coveredInsns)

      def copyWithRequiredInsn(v: (Var[_], Insn)): Match[A] = this.copy(requiredInsns = v :: requiredInsns)
    }

    def joinMatches[A, B, C](a: Match[A], b: Match[B], f: (A, B) => C): Match[C] =
      Match(f(a.value, b.value), a.coveredInsns ++ b.coveredInsns, a.requiredInsns ++ b.requiredInsns)

    def apply(op: IrOpcode): NullaryInsnPat[Insn] = NullaryInsnPat[Insn](op)

    def apply[A](op: IrOpcode, operand: Pat[A]): UnaryInsnPat[Insn, A] = UnaryInsnPat[Insn, A](op, operand)

    def apply[A, B](op: IrOpcode, left: Pat[A], right: Pat[B]): BinaryInsnPat[Insn, A, B] = BinaryInsnPat[Insn, A, B](op, left, right)
  }

  /** Matches a single instruction by its opcode and returns it. */
  case class NullaryInsnPat[T <: Insn](op: IrOpcode) extends Pat[T] {
    override def size: Int = 1

    override def cost: Int = 0

    override def apply(insn: Insn): Option[Pat.Match[T]] =
      if (insn.op == op) Some(Pat.Match(insn.asInstanceOf[T], List(insn), List.empty)) else None

    override def flatten: Iterable[NullaryInsnPat[T]] = Iterable(this)

    override def toString(): String = s"Pat($op)"
  }

  /** Matches an unary instruction by its opcode and operand, returns the instruction and value of the operand. */
  case class UnaryInsnPat[T <: Insn, A](op: IrOpcode, operand: Pat[A]) extends Pat[(T, A)] {
    override def size: Int = operand.size + 1

    override def cost: Int = operand.cost

    override def apply(insn: Insn): Option[Pat.Match[(T, A)]] =
      if (insn.op == op && insn.operands.size == 1) {
        for {
          Pat.Match(value, coveredInsns, requiredInsns) <- operand(insn.operands(0))
        } yield Pat.Match((insn.asInstanceOf[T], value), insn :: coveredInsns, requiredInsns)
      } else None

    override def flatten: Iterable[UnaryInsnPat[T, A]] =
      for {
        operand <- operand.flatten
      } yield UnaryInsnPat(op, operand)

    override def toString(): String = s"Pat($op, $operand)"
  }

  /** Matches a binary instruction by its opcode and operands, returns the instruction and value of the operands. */
  case class BinaryInsnPat[T <: Insn, A, B](op: IrOpcode, left: Pat[A], right: Pat[B]) extends Pat[(T, A, B)] {
    override def size: Int = left.size + right.size + 1

    override def cost: Int = left.cost + right.cost

    override def apply(insn: Insn): Option[Pat.Match[(T, A, B)]] =
      if (insn.op == op && insn.operands.size == 2)
        for {
          Pat.Match(leftValue, leftCoveredInsns, leftRequiredInsns) <- left(insn.operands(0))
          Pat.Match(rightValue, rightCoveredInsns, rightRequiredInsns) <- right(insn.operands(1))
        } yield Pat.Match(
          (insn.asInstanceOf[T], leftValue, rightValue),
          insn :: (leftCoveredInsns ++ rightCoveredInsns),
          leftRequiredInsns ++ rightRequiredInsns)
      else None

    override def flatten: Iterable[BinaryInsnPat[T, A, B]] =
      for {
        left <- left.flatten
        right <- right.flatten
      } yield BinaryInsnPat(op, left, right)

    override def toString(): String = s"Pat($op, $left, $right)"
  }

  /** Matches a nonterminal, the value for it must be available. */
  case class VarPat[T](v: Var[T]) extends Pat[T] {
    override def size: Int = 0

    override def cost: Int = 0

    override def apply(insn: Insn): Option[Pat.Match[T]] =
      Some(Pat.Match(v.resolveValue(insn), Nil, List((v, insn))))

    override def flatten: Iterable[VarPat[T]] = Iterable(this)

    override def toString(): String = s"VarPat($v)"
  }

  /** Transforms value of a pattern. */
  case class MapPat[T, U](pat: Pat[T], f: T => U) extends Pat[U] {
    override def size: Int = pat.size

    override def cost: Int = pat.cost

    override def apply(insn: Insn): Option[Pat.Match[U]] = {
      for {
        Pat.Match(value, coveredInsns, requiredInsns) <- pat(insn)
      } yield Pat.Match(f(value), coveredInsns, requiredInsns)
    }

    override def flatten: Iterable[MapPat[T, U]] =
      for {
        pat <- pat.flatten
      } yield MapPat(pat, f)

    override def toString(): String = s"($pat ^^ $f)"
  }

  /** Rejects matches, which don't pass through filter. */
  case class FilterPat[T](pat: Pat[T], f: Pat.Match[T] => Boolean) extends Pat[T] {
    override def size: Int = pat.size

    override def cost: Int = pat.cost

    override def apply(insn: Insn): Option[Pat.Match[T]] = {
      for {
        m <- pat(insn) if f(m)
      } yield m
    }

    override def flatten: Iterable[FilterPat[T]] =
      for {
        pat <- pat.flatten
      } yield FilterPat(pat, f)

    override def toString(): String = s"$pat.filter($f)"
  }

  /** Alternate. If [[pat]] matches, returns its result. Otherwise tries to match [[pat2]] . */
  case class OrPat[T](pat: Pat[T], pat2: Pat[T]) extends Pat[T] {
    override def size: Int = Math.min(pat.size, pat2.size)

    override def cost: Int = Math.max(pat.cost, pat2.cost)

    override def apply(insn: Insn): Option[Pat.Match[T]] =
      pat(insn).orElse(pat2(insn))

    override def flatten: Iterable[Pat[T]] = pat.flatten ++ pat2.flatten

    override def toString(): String = s"($pat | $pat2)"
  }

  /** Increases cost of [[pat]] by [[by]]. Useful to prioritize hand-optimized tiles over automatically generated ones. */
  case class IncreaseCostPat[T](pat: Pat[T], by: Int) extends Pat[T] {
    override def size: Int = pat.size

    override def cost: Int = pat.cost + by

    override def apply(insn: Insn): Option[Pat.Match[T]] = pat(insn)

    override def flatten: Iterable[Pat[T]] = pat.flatten

    override def toString(): String = s"($pat $$+$by)"
  }

  protected def matchIndexedSeq[A](seq: IndexedSeq[Insn], pat: Pat[A]): Option[Pat.Match[IndexedSeq[A]]] = {
    if (seq.isEmpty)
      Some(Pat.Match(IndexedSeq.empty, Nil, Nil))
    else {
      for {
        Pat.Match(headValue, headCoveredInsns, headRequiredInsns) <- pat(seq.head)
        Pat.Match(tailValue, tailCoveredInsns, tailRequiredInsns) <- matchIndexedSeq(seq.tail, pat)
      } yield Pat.Match(headValue +: tailValue, headCoveredInsns ++ tailCoveredInsns, headRequiredInsns ++ tailRequiredInsns)
    }
  }

  /** Matches CallInsn if [[arg]] matches all of its arguments. */
  case class CallInsnPat[A](arg: Pat[A]) extends Pat[(CallInsn, IndexedSeq[A])] {
    override def size: Int = arg.size + 1

    override def cost: Int = arg.cost

    override def apply(insn: Insn): Option[Pat.Match[(CallInsn, IndexedSeq[A])]] =
      if (insn.op == IrOpcode.Call) {
        val callInsn = insn.asInstanceOf[CallInsn]
        for {
          Pat.Match(args, coveredInsns, requiredInsns) <- matchIndexedSeq(callInsn.args, arg)
        } yield Pat.Match((callInsn, args), insn :: coveredInsns, requiredInsns)
      } else None

    override def flatten: Iterable[CallInsnPat[A]] =
      for {
        arg <- arg.flatten
      } yield CallInsnPat(arg)

    override def toString(): String = s"CallInsnPat($arg)"
  }

  case class CallPtrInsnPat[A, B](ptr: Pat[A], arg: Pat[B]) extends Pat[(CallPtrInsn, A, IndexedSeq[B])] {
    override def size: Int = ptr.size + arg.size + 1

    override def cost: Int = ptr.cost + arg.cost

    override def apply(insn: Insn): Option[Pat.Match[(CallPtrInsn, A, IndexedSeq[B])]] =
      if (insn.op == IrOpcode.CallPtr) {
        val callInsn = insn.asInstanceOf[CallPtrInsn]
        for (
          Pat.Match(ptr, ptrCoveredInsns, ptrRequiredInsns) <- ptr(callInsn.funPtr);
          Pat.Match(args, argsCoveredInsns, argsRequiredInsns) <- matchIndexedSeq(callInsn.args, arg)
        ) yield Pat.Match((callInsn, ptr, args), insn :: ptrCoveredInsns ++ argsCoveredInsns, ptrRequiredInsns ++ argsRequiredInsns)
      } else None

    override def flatten: Iterable[CallPtrInsnPat[A, B]] =
      for {
        ptr <- ptr.flatten
        arg <- arg.flatten
      } yield CallPtrInsnPat(ptr, arg)

    override def toString(): String = s"CallPtrInsnPat($arg)"
  }

  case class PhiInsnPat[A](arg: Pat[A]) extends Pat[(PhiInsn, IndexedSeq[A])] {
    override def size: Int = arg.size + 1

    override def cost: Int = arg.cost

    override def apply(insn: Insn): Option[Pat.Match[(PhiInsn, IndexedSeq[A])]] =
      if (insn.op == IrOpcode.Phi) {
        val phiInsn = insn.asInstanceOf[PhiInsn]
        for {
          Pat.Match(args, coveredInsns, requiredInsns) <- matchIndexedSeq(phiInsn.args.map(_._1), arg)
        } yield Pat.Match((phiInsn, args), insn :: coveredInsns, requiredInsns)
      } else None

    override def flatten: Iterable[PhiInsnPat[A]] =
      for {
        arg <- arg.flatten
      } yield PhiInsnPat(arg)

    override def toString(): String = s"PhiInsnPat($arg)"
  }

  type AsmPat[T] = Pat[AsmEmitter[T]]

  case class GenRule[T](variable: Var[AsmEmitter[T]], rhs: Pat[AsmEmitter[T]]) extends (Insn => Option[GenRule.Match[T]]) {
    override def apply(insn: Insn): Option[GenRule.Match[T]] = {
      try
        rhs(insn).map(GenRule.Match(this, _))
      catch {
        case e: Throwable => throw new BackendException(s"Failed to match $this", e)
      }
    }

    def flatten: Iterable[GenRule[T]] =
      for {
        rhs <- rhs.flatten
      } yield GenRule(variable, rhs)

    override def toString(): String = s"($variable -> $rhs)"
  }

  object GenRule {
    case class Match[T](rule: GenRule[T], patMatch: Pat.Match[AsmEmitter[T]]) {
      def variable: Var[AsmEmitter[T]] = rule.variable

      def value: AsmEmitter[T] = patMatch.value

      def coveredInsns: List[Insn] = patMatch.coveredInsns

      def requiredInsns: List[(Var[_], Insn)] = patMatch.requiredInsns
    }
  }

  implicit def var2pat[T](v: Var[T]): VarPat[T] = VarPat(v)

  /** A set of variables (nonterminals) used in the tree rewriting grammar */
  def variables: Seq[Var[AsmEmitter[_]]]

  /** A set of rewrite rules */
  def rules: Seq[GenRule[_]]

  /** Returns true, if the instruction can be covered by multiple tiles. */
  def canCoverByMultipleTiles(insn: Insn): Boolean

  /** Computes some covering of all instructions in the given [[fun]]. Every instruction can be at root of at most one tile.
   * Every instruction should be covered by at most one tile, except when allowed by [[canCoverByMultipleTiles]]. */
  def getTileMapForFun(fun: IrFun): Map[Insn, GenRule.Match[_]]
}
