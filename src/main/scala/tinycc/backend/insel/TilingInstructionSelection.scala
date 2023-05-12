package tinycc.backend.insel

import tinycc.common.ir._

import scala.language.implicitConversions

/** A main trait used by all tiling instruction selectors which defines the IR pattern matching system. */
trait TilingInstructionSelection {

  import Pat.Match

  type Context

  type AsmEmitter[+A] = Context => A

  /** A nonterminal (LHS of RewriteRule). */
  trait Var[+T] {
    def resolveValue(insn: Insn): T

    def getMatchCost(insn: Insn): Int = 0
  }

  type AsmVar[+T] = Var[AsmEmitter[T]]

  /** A pattern, which matches a tree of instructions. */
  trait Pat[+A] extends (Insn => Iterable[Match[A]]) {
    def rootOps: Iterable[IrOpcode]

    def flatten: Iterable[Pat[A]] = Iterable.single(this)

    /** Map values of each match using f */
    def ^^[B](f: A => B): Pat[B] = new MapPat(this, f)

    /** Match either this or pat, return list of all matches from both */
    def |[B >: A](pat: Pat[B]): Pat[B] = new OrPat(this, pat)

    def filter(f: A => Boolean): Pat[A] = new FilterMatchPat(this, (m: Match[A]) => f(m.value))

    def filterMatch(f: Match[A] => Boolean): Pat[A] = new FilterMatchPat(this, f)

    /** Increase cost of this pattern. */
    def cost(by: Int): Pat[A] = new IncCostPat(this, by)
  }

  object Pat {
    /** A pattern match.
     *
     * @param value The value returned by the pattern. This is usually an [[Insn]] or an [[AsmEmitter]].
     * @param cost Used to penalize patterns that for example do additional register moves, but cover the same amount of instructions
     *             as others. Lower cost should mean faster execution.
     * @param coveredInsns list of instructions covered by this pattern excluding matches of Vars (leaves)
     * @param requiredInsns list of matched variables (leaves) that will be accessed during code emission for this tile
     *
     * */
    case class Match[+A](value: A, cost: Int, coveredInsns: List[Insn], requiredInsns: List[(Var[_], Insn)])

    def apply(op: IrOpcode): NullaryInsnPat[Insn] = new NullaryInsnPat[Insn](op)

    def apply[A](op: IrOpcode, arg: Pat[A]): UnaryInsnPat[Insn, A] = new UnaryInsnPat[Insn, A](op, arg)

    def apply[A, B](op: IrOpcode, left: Pat[A], right: Pat[B]): BinaryInsnPat[Insn, A, B] = new BinaryInsnPat[Insn, A, B](op, left, right)
  }

  /** Matches a single instruction by its opcode and returns it. */
  class NullaryInsnPat[T <: Insn](op: IrOpcode) extends Pat[T] {
    override def rootOps: Iterable[IrOpcode] = Iterable.single(op)

    override def apply(insn: Insn): Iterable[Match[T]] = {
      if (insn.op != op)
        return Iterable.empty
      Iterable(Match(insn.asInstanceOf[T], 0, List(insn), List.empty))
    }

    override def toString(): String = s"Pat($op)"
  }

  /** Matches an unary instruction by its opcode and operand, returns the instruction and value of the operand. */
  class UnaryInsnPat[T <: Insn, A](op: IrOpcode, operandPat: Pat[A]) extends Pat[(T, A)] {
    override def rootOps: Iterable[IrOpcode] = Iterable.single(op)

    override def apply(insn: Insn): Iterable[Match[(T, A)]] = {
      if (insn.op != op || insn.operands.size != 1)
        return Iterable.empty
      for {
        Match(value, cost, coveredInsns, requiredInsns) <- operandPat(insn.operands(0))
      } yield Match((insn.asInstanceOf[T], value), cost, insn :: coveredInsns, requiredInsns)
    }

    override def toString(): String = s"Pat($op, $operandPat)"
  }

  /** Matches a binary instruction by its opcode and operands, returns the instruction and value of the operands. */
  class BinaryInsnPat[T <: Insn, A, B](op: IrOpcode, leftPat: Pat[A], rightPat: Pat[B]) extends Pat[(T, A, B)] {
    override def rootOps: Iterable[IrOpcode] = Iterable.single(op)

    override def apply(insn: Insn): Iterable[Match[(T, A, B)]] = {
      if (insn.op != op || insn.operands.size != 2)
        return Iterable.empty
      for {
        Match(leftValue, leftCost, leftCoveredInsns, leftRequiredInsns) <- leftPat(insn.operands(0))
        Match(rightValue, rightCost, rightCoveredInsns, rightRequiredInsns) <- rightPat(insn.operands(1))
      } yield Match(
        (insn.asInstanceOf[T], leftValue, rightValue),
        leftCost + rightCost,
        insn :: (leftCoveredInsns ++ rightCoveredInsns),
        leftRequiredInsns ++ rightRequiredInsns)
    }

    override def toString(): String = s"Pat($op, $leftPat, $rightPat)"
  }

  /** Matches a nonterminal, the value for it must be available. */
  class VarPat[T](v: Var[T]) extends Pat[T] {
    override def rootOps: Iterable[IrOpcode] = Iterable.empty

    override def apply(insn: Insn): Iterable[Match[T]] =
      Iterable.single(Match(v.resolveValue(insn), v.getMatchCost(insn), Nil, List((v, insn))))

    override def toString(): String = s"VarPat($v)"
  }

  /** Transforms value of a pattern. */
  class MapPat[T, U](pat: Pat[T], f: T => U) extends Pat[U] {
    override def rootOps: Iterable[IrOpcode] = pat.rootOps

    override def flatten: Iterable[Pat[U]] =
      for {
        pat <- pat.flatten
      } yield new MapPat(pat, f)

    override def apply(insn: Insn): Iterable[Match[U]] =
      for {
        Match(value, cost, coveredInsns, requiredInsns) <- pat(insn)
      } yield Match(f(value), cost, coveredInsns, requiredInsns)

    override def toString(): String = s"($pat ^^ $f)"
  }

  /** Rejects matches, which don't pass through filter. */
  class FilterMatchPat[T](pat: Pat[T], f: Match[T] => Boolean) extends Pat[T] {
    override def rootOps: Iterable[IrOpcode] = pat.rootOps

    override def flatten: Iterable[Pat[T]] =
      for {
        pat <- pat.flatten
      } yield new FilterMatchPat(pat, f)

    override def apply(insn: Insn): Iterable[Match[T]] =
      for {
        m <- pat(insn) if f(m)
      } yield m

    override def toString(): String = s"$pat.filter($f)"
  }

  /** Increases cost of [[pat]] by [[by]]. Useful to prioritize hand-optimized tiles over automatically generated ones. */
  class IncCostPat[T](pat: Pat[T], by: Int) extends Pat[T] {
    override def rootOps: Iterable[IrOpcode] = pat.rootOps

    override def flatten: Iterable[Pat[T]] =
      for {
        pat <- pat.flatten
      } yield new IncCostPat(pat, by)

    override def apply(insn: Insn): Iterable[Match[T]] =
      for {
        Match(value, cost, coveredInsns, requiredInsns) <- pat(insn)
      } yield Match(value, cost + by, coveredInsns, requiredInsns)

    override def toString(): String = s"($pat $$+$by)"
  }

  /** Fully backtracking alternate, returns results of matching both [[pat]] and [[pat2]]. */
  class OrPat[T](pat: Pat[T], pat2: Pat[T]) extends Pat[T] {
    override def rootOps: Iterable[IrOpcode] = pat.rootOps ++ pat2.rootOps

    override def flatten: Iterable[Pat[T]] = pat.flatten ++ pat2.flatten

    override def apply(insn: Insn): Iterable[Match[T]] =
      pat(insn) ++ pat2(insn)

    override def toString(): String = s"($pat | $pat2)"
  }

  protected def matchIndexedSeq[A](seq: IndexedSeq[Insn], pat: Pat[A]): Iterable[Match[IndexedSeq[A]]] = {
    if (seq.isEmpty)
      return Iterable.single(Match(IndexedSeq.empty, 0, Nil, Nil))
    for {
      Match(headValue, headCost, headCoveredInsns, headRequiredInsns) <- pat(seq.head)
      Match(tailValue, tailCost, tailCoveredInsns, tailRequiredInsns) <- matchIndexedSeq(seq.tail, pat)
    } yield Match(headValue +: tailValue, headCost + tailCost, headCoveredInsns ++ tailCoveredInsns, headRequiredInsns ++ tailRequiredInsns)
  }

  /** Matches [[CallInsn]] if [[argPat]] matches all of its operands. */
  case class CallInsnPat[A](argPat: Pat[A]) extends Pat[(CallInsn, IndexedSeq[A])] {
    override def rootOps: Iterable[IrOpcode] = Iterable.single(IrOpcode.Call)

    override def apply(insn: Insn): Iterable[Match[(CallInsn, IndexedSeq[A])]] = {
      if (insn.op != IrOpcode.Call)
        return Iterable.empty
      val callInsn = insn.asInstanceOf[CallInsn]
      for {
        Match(args, cost, coveredInsns, requiredInsns) <- matchIndexedSeq(callInsn.args, argPat)
      } yield Match((callInsn, args), cost, insn :: coveredInsns, requiredInsns)
    }

    override def toString(): String = s"CallInsnPat($argPat)"
  }

  /** Matches [[CallPtrInsn]] if [[ptrPat]] matches the function address and [[argPat]] matches all of its operands. */
  case class CallPtrInsnPat[A, B](ptrPat: Pat[A], argPat: Pat[B]) extends Pat[(CallPtrInsn, A, IndexedSeq[B])] {
    override def rootOps: Iterable[IrOpcode] = Iterable.single(IrOpcode.CallPtr)

    override def apply(insn: Insn): Iterable[Match[(CallPtrInsn, A, IndexedSeq[B])]] = {
      if (insn.op != IrOpcode.CallPtr)
        return Iterable.empty
      val callPtrInsn = insn.asInstanceOf[CallPtrInsn]
      for (
        Match(ptr, ptrCost, ptrCoveredInsns, ptrRequiredInsns) <- ptrPat(callPtrInsn.funPtr);
        Match(args, argsCost, argsCoveredInsns, argsRequiredInsns) <- matchIndexedSeq(callPtrInsn.args, argPat)
      ) yield Match((callPtrInsn, ptr, args), ptrCost + argsCost, insn :: ptrCoveredInsns ++ argsCoveredInsns, ptrRequiredInsns ++ argsRequiredInsns)
    }

    override def toString(): String = s"CallPtrInsnPat($argPat)"
  }

  /** Matches [[PhiInsn]] if [[argPat]] matches all of its operands. */
  case class PhiInsnPat[A](argPat: Pat[A]) extends Pat[(PhiInsn, IndexedSeq[A])] {
    override def rootOps: Iterable[IrOpcode] = Iterable.single(IrOpcode.Phi)

    override def apply(insn: Insn): Iterable[Match[(PhiInsn, IndexedSeq[A])]] = {
      if (insn.op != IrOpcode.Phi)
        return Iterable.empty
      val phiInsn = insn.asInstanceOf[PhiInsn]
      for {
        Match(args, cost, coveredInsns, requiredInsns) <- matchIndexedSeq(phiInsn.args.map(_._1), argPat)
      } yield Match((phiInsn, args), cost, insn :: coveredInsns, requiredInsns)
    }

    override def toString(): String = s"PhiInsnPat($argPat)"
  }

  type AsmPat[T] = Pat[AsmEmitter[T]]

  /** A class representing a code generation rewrite rule in form of variable -> rhs. */
  case class GenRule[T](variable: AsmVar[T], rhs: AsmPat[T]) extends (Insn => Iterable[GenRule.Match[T]]) {
    def flatten: Iterable[GenRule[T]] =
      for {
        rhs <- rhs.flatten
      } yield GenRule(variable, rhs)

    override def apply(insn: Insn): Iterable[GenRule.Match[T]] = {
      try
        rhs(insn).map(GenRule.Match(this, _))
      catch {
        case e: Throwable => throw new RuntimeException(s"Failed to match $this", e)
      }
    }

    override def toString(): String = s"($variable -> $rhs)"
  }

  object GenRule {
    case class Match[T](rule: GenRule[T], patMatch: Pat.Match[AsmEmitter[T]]) {
      def variable: AsmVar[T] = rule.variable

      lazy val size: Int = patMatch.coveredInsns.size

      def cost: Int = patMatch.cost

      def value: AsmEmitter[T] = patMatch.value

      def coveredInsns: List[Insn] = patMatch.coveredInsns

      def requiredInsns: List[(Var[_], Insn)] = patMatch.requiredInsns
    }
  }

  implicit def var2pat[T](v: Var[T]): VarPat[T] = new VarPat(v)

  /** A set of variables (nonterminals) used in the tree rewriting grammar */
  def variables: Seq[AsmVar[_]]

  /** A set of rewrite rules */
  def rules: Seq[GenRule[_]]

  /** Returns true if a value of nonterminal [[from]] can be converted to value of nonterminal [[to]]. */
  def canCastFromTo(from: AsmVar[_], to: AsmVar[_]): Boolean

  /** Returns the code to convert [[value]], which is a value of nonterminal [[from]] to a value of nonterminal [[to]]. */
  def emitCastFromTo[F, T](value: F, from: AsmVar[F], to: AsmVar[T]): AsmEmitter[T]

  /** Computes some covering of all instructions in the given [[fun]]. Every instruction can be at root of at most one tile.
   *  This can be implemented for example using dynamic programming or maximal munch. */
  def getTileMapForFun(fun: IrFun): Map[Insn, GenRule.Match[_]]
}
