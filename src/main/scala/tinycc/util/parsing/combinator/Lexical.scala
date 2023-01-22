package tinycc.util.parsing.combinator

abstract class Lexical extends Parsers with Debugging {
  type Input = CharInput

  type Token

  implicit def elem(c: Char): Parser[Char] = elem(s"'$c'", _ == c) label s"'$c'"

  lazy val elem: Parser[Char] = elem("a char", _ => true)

  def elem(msg: String, p: Char => Boolean): Parser[Char] = Parser { in =>
    if (p(in.charAt(0))) Accept(in.charAt(0), in.drop(1)) else Reject(s"expected $msg, got '${in.charAt(0)}'", in)
  } label msg

  implicit def lit(s: String): Parser[String] =
    Parser { in =>
      var i = 0
      while (i < in.length && i < s.length && in.charAt(i) == s.charAt(i)) {
        i += 1
      }
      if (i == s.length) {
        Accept(s, in.drop(i))
      } else {
        val found = if (i == in.length) {
          "end of input"
        } else {
          "'" + in.subSequence(0, math.min(in.length, s.length)).content.trim + "'"
        }
        Reject("expected '" + s + "', got " + found, in)
      }
    } label s"'$s'"

  def chrExcept(cs: Char*): Parser[Char] = elem("", ch => !cs.contains(ch))

  lazy val eof: Parser[Any] = Parser { in =>
    if (in.isEmpty) Accept((), in) else Reject(s"expected end of input, got '${in.charAt(0)}'", in)
  }

  def whitespace: Parser[Any]

  def token: Parser[Token]

  def errorToken(msg: String): Token

  //  def cursor: Parser[Position] = Parser { in => Accept(in.pos, in) } label "cursor"

  class TokenInput(in: CharInput) extends Iterable[Token] with FiniteInput {
    def this(in: String) = this(new CharInput(in))

    private val (afterWs, tok, afterTok) = parse(whitespace, in) match {
      case Accept(_, afterWs) =>
        parse(token, afterWs) match {
          case Accept(tok, afterTok) => (afterWs, tok, afterTok)
          case Reject(msg, _, _) => (afterWs, errorToken(msg), afterWs.drop(afterWs.length))
        }
      case Reject(msg, _, _) => (in, errorToken(msg), in.drop(in.length))
    }

    override def head: Token = tok

    override def tail = new TokenInput(afterTok)

    def pos: Position = afterWs.pos

    override def isEmpty: Boolean = in.isEmpty || afterWs.isEmpty

    override def iterator: Iterator[Token] = Iterator.unfold(this)(ti =>
      if (ti.isEmpty) None else Some((ti.head, ti.tail)))
  }
}
