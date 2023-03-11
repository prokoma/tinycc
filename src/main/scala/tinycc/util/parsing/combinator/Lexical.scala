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

  def elem[R](message: String, fn: PartialFunction[Char, R]): Parser[R] = (in: Input) => in.headOption match {
    case Some(tok) => fn.andThen(Accept(_, in.tail)).applyOrElse(tok, (_: Char) => Reject(message, in))
    case None => Reject(in)
  }

  def elem(e: Char): Parser[Char] = elem(e.toString, { case c if e == c => c })

  def elem(s: String): Parser[String] = (in: Input) => in.take(s.length) match {
    case r if r == s => Accept(s, in.drop(s.length))
    case _ => Reject(s, in)
  }

  def elem(s: Symbol): Parser[Symbol] = elem(s.name) ^^ { _ => s }

  def oneOfChar(message: String, allowed: Seq[Char]): Parser[Char] =
    elem(message, { case c if allowed.contains(c) => c })

  def oneOfSymbol(message: String, allowed: Seq[Symbol]): Parser[Symbol] = {
    val sorted = allowed.sortBy(-_.name.length)

    def parser(in: Input): Result[Symbol] = {
      for (s <- sorted) {
        if (in.take(s.name.length) == s.name)
          return Accept(s, in.drop(s.name.length))
        if (in.isEmpty)
          return Reject(message, in)
      }
      Reject(message, in)
    }

    parser
  }

  lazy val letter: Parser[Char] = elem("letter", { case c if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') => c })

  lazy val digit: Parser[Char] = elem("digit", { case c if c >= '0' && c <= '9' => c })

  lazy val whitespace: Parser[Char] = oneOfChar("whitespace", Seq('\r', '\n', '\t', ' '))

  lazy val NL: Parser[String] = (in: Input) =>
    if (in.isEmpty) Accept("", in)
    else if (in.take(1) == "\n") Accept("\n", in.tail)
    else if (in.take(2) == "\r\n") Accept("\r\n", in.drop(2))
    else Reject("end of line", in)
}