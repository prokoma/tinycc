package tinycc.util.parsing.combinator

import tinycc.util.parsing.SourceLocation

case class CharReader(string: String, loc: SourceLocation = SourceLocation(1, 1, 0)) extends Reader[Char] {
  override def headOption: Option[Char] = if (isEmpty) None else Some(string(loc.offset))

  override def tail: CharReader = {
    if (isEmpty)
      throw new UnsupportedOperationException("tail of empty string")
    if (string(loc.offset) == '\r' && loc.offset + 1 < string.length && string(loc.offset + 1) == '\n')
      CharReader(string, SourceLocation(loc.line + 1, 1, loc.offset + 2))
    else if (string(loc.offset) == '\n')
      CharReader(string, SourceLocation(loc.line + 1, 1, loc.offset + 1))
    else
      CharReader(string, SourceLocation(loc.line, loc.col + 1, loc.offset + 1))
  }

  def take(n: Int): String = string.substring(loc.offset, Math.min(string.length, loc.offset + n))

  def drop(n: Int): CharReader = 0.until(n).foldLeft(this)((r, _) => r.tail)

  override def isEmpty: Boolean = loc.offset >= string.length
}

trait Lexical extends Parsers {
  override type Input = CharReader

  def unexpectedEndOfInput(in: Input): Reject = Reject("unexpected end of input", in)

  def elem[R](fn: PartialFunction[Char, R], msgFn: Char => String): Parser[R] =
    in => in.headOption match {
      case None => unexpectedEndOfInput(in)
      case Some(tok) => fn.andThen(Accept(_, in.tail)).applyOrElse(tok, (tok: Char) => Reject(msgFn(tok), in))
    }

  lazy val elem: Parser[Char] = in => in.headOption match {
    case None => unexpectedEndOfInput(in)
    case Some(tok) => Accept(tok, in.tail)
  }
  //
  //
  //
  //  lazy val EOF: Parser[Unit] = in => if (in.isEmpty) Accept((), in) else Reject("expected end of input", in)
  //
  //  lazy val loc: Parser[SourceLocation] = in => Accept(in.pos, in)

  def elem(compare: Char): Parser[Char] =
    elem({ case c if compare == c => c }, c => s"expected $compare, got $c")

  def elem(s: String): Parser[String] =
    in => in.take(s.length) match {
      case r if r == s => Accept(s, in.drop(s.length))
      case "" => unexpectedEndOfInput(in)
      case r => Reject(s"expected $s, got $r", in)
    }

  def elem(s: Symbol): Parser[Symbol] =
    in => in.take(s.name.length) match {
      case r if r == s.name => Accept(s, in.drop(s.name.length))
      case "" => unexpectedEndOfInput(in)
      case r => Reject(s"expected $s, got $r", in)
    }
  //
  //  def elem(allowed: Seq[String], msgFn: Char => String): Parser[String] = {
  //    val sorted = allowed.sortBy(- _.length)
  //    in => {
  //      for(s <- sorted) {
  //        if (s.nonEmpty && in.isEmpty)
  //          return unexpectedEndOfInput(in)
  //        if(in.take(s.length) == s)
  //          return Accept(s, in.drop(s.length))
  //      }
  //      return Reject(s"expected one of $allowed", in)
  //    }

  def oneOfChar(allowed: Seq[Char], msgFn: Char => String): Parser[Char] =
    elem({ case c if allowed.contains(c) => c }, msgFn)

  def oneOfSymbol(allowed: Seq[Symbol], msgFn: CharReader => String): Parser[Symbol] = {
    val sorted = allowed.sortBy(-_.name.length)

    def parser(in: Input): Result[Symbol] = {
      for (s <- sorted) {
        if (in.take(s.name.length) == s.name)
          return Accept(s, in.drop(s.name.length))
        if (in.isEmpty)
          return unexpectedEndOfInput(in)
      }
      Reject(msgFn(in), in)
    }

    parser
  }

  lazy val NL: Parser[String] = in =>
    if (in.isEmpty) Accept("", in)
    else if (in.take(1) == "\n") Accept("\n", in.tail)
    else if (in.take(2) == "\r\n") Accept("\r\n", in.drop(2))
    else Reject("expected end of line", in)
}
