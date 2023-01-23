package tinycc.frontend

import tinycc.frontend.ast.SourceLocation
import tinycc.util.parsing.combinator.Parsers

trait Reader[T] {
  def headOption: Option[T]

  def tail: Reader[T]

  def pos: SourceLocation

  def isEmpty: Boolean = headOption.isEmpty
}

trait MyParsers[T] extends Parsers {
  type Input = Reader[T]

  def unexpectedEndOfInput(in: Input): Reject = Reject("unexpected end of input", in)

  def elem[R](fn: PartialFunction[T, R], msgFn: T => String): Parser[R] =
    in => in.headOption match {
      case None => unexpectedEndOfInput(in)
      case Some(tok) => fn.andThen(Accept(_, in.tail)).applyOrElse(tok, tok => Reject(msgFn(tok), in))
    }

//  lazy val elem: Parser[T] = in => in.headOption match {
//    case None => unexpectedEndOfInput(in)
//    case Some(tok) => Accept(tok, in.tail)
//  }

//  def elem(allowed: T): Parser[T] =
//    elem({ case c if allowed == c => c }, c => s"expected $allowed, got $c")
//
//  def elem(allowed: Seq[T], msgFn: T => String): Parser[T] =
//    elem({ case c if allowed.contains(c) => c }, msgFn)

  lazy val EOF: Parser[Unit] = in => if (in.isEmpty) Accept((), in) else Reject("expected end of input", in)

  lazy val loc: Parser[SourceLocation] = in => Accept(in.pos, in)
}

case class StringReader(string: String, pos: SourceLocation) extends Reader[Char] {
  override def headOption: Option[Char] = if (isEmpty) None else Some(string(pos.offset))

  override def tail: StringReader = {
    if (isEmpty)
      throw new UnsupportedOperationException("tail of empty string")
    if (string(pos.offset) == '\r' && pos.offset + 1 < string.length && string(pos.offset + 1) == '\n')
      StringReader(string, SourceLocation(pos.line + 1, 0, pos.offset + 2))
    else if (string(pos.offset) == '\n')
      StringReader(string, SourceLocation(pos.line + 1, 0, pos.offset + 1))
    else
      StringReader(string, SourceLocation(pos.line, pos.col + 1, pos.offset + 1))
  }

  def take(n: Int): String = string.substring(pos.offset, pos.offset + n)

  def drop(n: Int): StringReader = 0.until(n).foldLeft(this)((r, _) => r.tail)

  override def isEmpty: Boolean = pos.offset >= string.length
}

trait Lexical extends MyParsers[Char] {
  override type Input = StringReader

//  def elem(s: String): Parser[String] =
//    in => in.take(s.length) match {
//      case r if r == s => Accept(s, in.drop(s.length))
//      case "" => unexpectedEndOfInput(in)
//      case r => Reject(s"expected $s, got $r", in)
//    }
//
//  def elem(s: Symbol): Parser[Symbol] =
//    in => in.take(s.name.length) match {
//      case r if r == s.name => Accept(s, in.drop(s.name.length))
//      case "" => unexpectedEndOfInput(in)
//      case r => Reject(s"expected $s, got $r", in)
//    }
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

  def elem(allowed: Seq[Symbol], msgFn: Input => String): Parser[Symbol] = {
    val sorted = allowed.sortBy(-_.name.length)
    in => {
//      for (s <- sorted) {
//        if (in.take(s.name.length) == s.name)
//          return Accept(s, in.drop(s.name.length))
//        if (in.isEmpty)
//          return unexpectedEndOfInput(in)
//      }
      Reject(msgFn(in), in)
    }
  }

  lazy val NL: Parser[String] = in =>
    if (in.isEmpty) Accept("", in)
    else if (in.take(1) == "\n") Accept("\n", in.tail)
    else if (in.take(2) == "\r\n") Accept("\r\n", in.drop(2))
    else Reject("expected end of line", in)
}