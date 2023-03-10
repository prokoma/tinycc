package tinycc.util.parsing.combinator

import tinycc.util.parsing.SourceLocation

import scala.annotation.tailrec

trait Parsers {
  type Input <: Reader[_]

  sealed trait Result[+T] {
    def remainder: Input

    def map[R](f: T => R): Result[R]

    /** Runs the parser returned by f(value) on [[remainder]]. In case of rejection, returns the last reject. */
    def flatMap[R](f: T => Parser[R]): Result[R]

    def filter(f: T => Boolean): Result[T]

    def orElse[U >: T](other: => Result[U]): Result[U]
  }

  case class Accept[+T](value: T, remainder: Input, lastReject: Option[Reject] = None) extends Result[T] {
    override def map[R](f: T => R): Result[R] = Accept(f(value), remainder, lastReject)

    override def flatMap[R](f: T => Parser[R]): Result[R] = f(value)(remainder) match {
      case Accept(value, remainder, lastReject2) => Accept(value, remainder, getLastReject(lastReject, lastReject2))
      case error@Reject(_, _, true) => error
      case reject: Reject => getLastReject(lastReject, reject)
    }

    override def filter(f: T => Boolean): Result[T] = if (f(value)) this else Reject(remainder)

    override def orElse[U >: T](other: => Result[U]): Result[U] = this
  }

  case class Reject(expected: ExpTree, remainder: Input, fatal: Boolean = false) extends Result[Nothing] {
    override def map[R](f: Nothing => R): Result[R] = this

    override def flatMap[R](f: Nothing => Parser[R]): Result[R] = this

    override def filter(f: Nothing => Boolean): Result[Nothing] = this

    override def orElse[U >: Nothing](other: => Result[U]): Result[U] = {
      if (fatal)
        this
      else
        other match {
          case Accept(value, remainder, lastReject) => Accept(value, remainder, getLastReject(Some(this), lastReject))
          case error@Reject(_, _, true) => error
          case reject: Reject => getLastReject(this, reject)
        }
    }
  }

  object Reject {
    def apply(remainder: Input): Reject = Reject(ExpTree(), remainder)

    def apply(message: String, remainder: Input): Reject = Reject(ExpTree(message), remainder)

    def apply(expected: List[ExpTree], remainder: Input): Reject = Reject(ExpTree(expected), remainder)
  }

  case class ~[+A, +B](a: A, b: B) {
    override def toString: String = s"$a~$b"
  }

  case class ExpTree(message: String = "", children: List[ExpTree] = Nil) {
    def leaves: Seq[ExpTree] = if (children.isEmpty) Seq(this) else children.flatMap(_.leaves)
  }

  object ExpTree {
    def apply(children: List[ExpTree]): ExpTree = ExpTree("", children)
  }

  abstract class Parser[+T] extends (Input => Result[T]) {
    def map[R](f: T => R): Parser[R] = (in: Input) => this (in).map(f)

    def flatMap[R](f: T => Parser[R]): Parser[R] = (in: Input) => this (in).flatMap(f)

    def filter(f: T => Boolean): Parser[T] = (in: Input) => this (in).filter(f)

    def described(message: String): Parser[T] = (in: Input) => this (in) match {
      case Accept(value, remainder, lastReject) =>
        // Update information about lastReject with this message as a context.
        Accept(value, remainder, lastReject.map(lastReject => Reject(ExpTree(message, List(lastReject.expected)), lastReject.remainder)))

      case Reject(expected, remainder, fatal) =>
        // If the parser consumed some input, use its errors message as a context. Otherwise use just the provided message.
        Reject(ExpTree(message, if (remainder.loc > in.loc) List(expected) else Nil), remainder, fatal)
    }

    /** Map the result of this parser. */
    def ^^[R](f: T => R): Parser[R] = map(f)

    /** Sequence of two parsers. Returns lhs~rhs. */
    def ~[U](other: => Parser[U]): Parser[~[T, U]] = for (a <- this; b <- other) yield new~(a, b)

    /** Run the two parsers in sequence, but keep only result of rhs. */
    def ~>[U](other: => Parser[U]): Parser[U] = for (_ <- this; b <- other) yield b

    /** Run the two parsers in sequence, but keep only result of lhs. */
    def <~[U](other: => Parser[U]): Parser[T] = for (a <- this; _ <- other) yield a

    /** Alternative - if the lhs parser fails, backtrack and run the rhs parser on the same input. */
    def |[U >: T](other: => Parser[U]): Parser[U] = {
      lazy val _other = other
      (in: Input) => this (in).orElse(_other(in))
    }
  }

  protected def getLastReject(a: Reject, b: Reject): Reject = {
    if (a.remainder.loc == b.remainder.loc)
      Reject(List(a.expected, b.expected), a.remainder)
    else if (a.remainder.loc > b.remainder.loc) a
    else b
  }

  protected def getLastReject(a: Option[Reject], b: Reject): Reject = a match {
    case Some(a) => getLastReject(a, b)
    case None => b
  }

  protected def getLastReject(a: Option[Reject], b: Option[Reject]): Option[Reject] = (a, b) match {
    case (Some(a), Some(b)) => Some(getLastReject(a, b))
    case _ => a.orElse(b)
  }

  def Parser[T](f: Input => Result[T]): Parser[T] = (in: Input) => f(in)

  def success[T](value: T): Parser[T] = (in: Input) => Accept(value, in)

  def failure(message: String = ""): Parser[Nothing] = (in: Input) => Reject(message, in)

  def opt[T](parser: Parser[T]): Parser[Option[T]] = (parser ^^ Some.apply) | success(None)

  /** Accept if the parser rejects, otherwise reject. Does not consume any input. */
  def not[T](parser: Parser[T]): Parser[Unit] = (in: Input) => parser(in) match {
    case _: Accept[T] => Reject(in)
    case _: Reject => Accept((), in)
  }

  /** Run the parser in sequence while it matches and return the results as a list. Reject if the parser doesn't match at least once. */
  def rep1[T](parser: => Parser[T]): Parser[List[T]] = {
    lazy val _parser = parser
    (in: Input) => {
      @tailrec
      def iter(acc: Result[List[T]]): Result[List[T]] = acc match {
        case Accept(tail, remainder, lastReject) => _parser(remainder) match {
          case Accept(head, remainder, lastReject2) => iter(Accept(head :: tail, remainder, getLastReject(lastReject, lastReject2)))
          case error@Reject(_, _, true) => error

          // Current iteration rejected, but previous succeeded - return the result from previous iteration.
          case reject: Reject => Accept(tail, remainder, getLastReject(lastReject, Some(reject)))
        }

        case reject: Reject => reject
      }

      iter(_parser(in).map(List(_))).map(_.reverse)
    }
  }

  def rep[T](parser: => Parser[T]): Parser[List[T]] = rep1(parser) | success(Nil)

  def rep1sep[T](parser: => Parser[T], sep: => Parser[Any]): Parser[List[T]] = parser ~ rep(sep ~> parser) ^^ { case head ~ tail => head :: tail }

  def repsep[T](parser: => Parser[T], sep: => Parser[Any]): Parser[List[T]] = rep1sep(parser, sep) | success(Nil)

  /** Convert rejection from the inner parser into fatal rejection, which doesn't backtrack. Something like cut in Prolog. */
  def commit[T](parser: => Parser[T]): Parser[T] = (in: Input) => parser(in) match {
    case accept: Accept[T] => accept
    case Reject(expected, remainder, _) => Reject(expected, remainder, fatal = true)
  }

  def parse[T](parser: Parser[T], in: Input): Result[T] = parser(in)

  def remainderToString(in: Input): String = in.headOption.getOrElse("end of input").toString

  def formatErrorMessage(tree: ExpTree, remainder: Input): String = {
    def removeBlankNodes(tree: ExpTree): List[ExpTree] = tree match {
      case ExpTree("", children) => children.flatMap(removeBlankNodes)
      case ExpTree(message, children) => List(ExpTree(message, children.flatMap(removeBlankNodes)))
    }

    // Returns the first node, which is the ancestor of all leaves.
    @tailrec
    def findClosestContext(tree: ExpTree): ExpTree = tree match {
      case ExpTree(_, Nil) => tree // leaf -> return itself (shouldn't happen)
      case ExpTree(_, List(ExpTree(_, Nil))) => tree // has leaf as its only child -> return itself
      case ExpTree(_, List(child)) => findClosestContext(child) // has only child, which is not leaf -> descend
      case ExpTree(_, _) => tree // has multiple children (thus multiple leaves) -> return itself
    }

    val context = findClosestContext(ExpTree("", removeBlankNodes(tree)))
    val expected = context.leaves.map(_.message)

    var message = ""
    if (context.message != "")
      message += s"while parsing ${context.message}, "
    if (expected.nonEmpty) {
      message += "expected " + expected.tail.mkString(", ")
      if (expected.tail.nonEmpty)
        message += " or "
      message += expected.head
      message += ", but "
    }
    message += s"got unexpected ${remainderToString(remainder)}"
    message
  }

  lazy val loc: Parser[SourceLocation] = (in: Input) => Accept(in.loc, in)

  lazy val EOI: Parser[Unit] = (in: Input) =>
    if (in.isEmpty) Accept((), in)
    else Reject("end of input", in)
}
