package tinycc
package frontend

import tinycc.util.parsing.SourceLocation
import tinycc.util.parsing.combinator.{CharReader, Lexical, Reader}

import scala.language.implicitConversions

object Lexer extends Lexical {
  implicit def charParser(c: Char): Parser[Char] = elem(c)

  implicit def stringParser(s: String): Parser[String] = elem(s)

  //  def errorToken(msg: String): Token[_] = ErrorToken(msg)

  lazy val WHITESPACE: Parser[Any] = rep[Any](
    whitespaceChar
      | ("//" ~ rep(not(NL) ~> elem))
      | ("/*" ~ (blockCommentEnd | failure("unclosed comment")))
  )

  lazy val whitespaceChar: Parser[Char] =
    oneOfChar(Seq('\r', '\n', '\t', ' '), c => s"expected whitespace, got '$c'")

  def blockCommentEnd: Parser[Any] =
    rep(elem({ case c if c != '*' => () }, _ => "")) ~ '*' ~ (elem('/') | blockCommentEnd)

  // Token

  lazy val TOKEN: Parser[Token] =
    OPERATOR | KEYWORD | IDENTIFIER | NUMERIC_LITERAL | STRING_SINGLE_QUOTED | STRING_DOUBLE_QUOTED

  // Operator

  lazy val OPERATOR: Parser[Special] = oneOfSymbol(Seq(
    Symbols.inc,
    Symbols.dec,
    Symbols.add,
    Symbols.sub,
    Symbols.mul,
    Symbols.div,
    Symbols.mod,
    Symbols.shiftLeft,
    Symbols.shiftRight,
    Symbols.eq,
    Symbols.ne,
    Symbols.lt,
    Symbols.gt,
    Symbols.le,
    Symbols.ge,
    Symbols.bitAnd,
    Symbols.bitOr,
    Symbols.and,
    Symbols.or,
    Symbols.not,
    Symbols.neg,
    Symbols.bitXor,
    Symbols.dot,
    Symbols.semicolon,
    Symbols.colon,
    Symbols.arrowR,
    Symbols.comma,
    Symbols.parOpen,
    Symbols.parClose,
    Symbols.squareOpen,
    Symbols.squareClose,
    Symbols.curlyOpen,
    Symbols.curlyClose,
    Symbols.assign,
    Symbols.backtick,
  ), (_: CharReader) => "expected operator") ^^ Special

  lazy val KEYWORD: Parser[Special] = oneOfSymbol(Seq(
    Symbols.kwBreak,
    Symbols.kwCase,
    Symbols.kwCast,
    Symbols.kwChar,
    Symbols.kwContinue,
    Symbols.kwDefault,
    Symbols.kwDo,
    Symbols.kwDouble,
    Symbols.kwElse,
    Symbols.kwFor,
    Symbols.kwIf,
    Symbols.kwInt,
    Symbols.kwReturn,
    Symbols.kwStruct,
    Symbols.kwSwitch,
    Symbols.kwTypedef,
    Symbols.kwVoid,
    Symbols.kwWhile,
    Symbols.kwScan,
    Symbols.kwPrint,
  ), (_: CharReader) => "expected keyword") ^^ Special

  lazy val letter: Parser[Char] = elem({ case c if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') => c }, c => s"expected letter, got $c")

  lazy val digit: Parser[Char] = elem({ case c if c >= '0' && c <= '9' => c }, c => s"expected digit, got $c")

  // Identifier

  lazy val identifierStart: Parser[Char] =
    letter | '_'

  lazy val identifierMid: Parser[Char] =
    identifierStart | digit

  lazy val IDENTIFIER: Parser[Identifier] =
    (identifierStart ~ rep(identifierMid)) ^^ { case head ~ tail => Identifier(Symbol((head :: tail).mkString)) } |
      failure("expected identifier")


  // IntLiteral or DoubleLiteral

  lazy val NUMERIC_LITERAL: Parser[Token] =
    rep1(digit) ~ opt('.' ~ rep(digit) ^^ { case dot ~ digits => dot + digits.mkString }) ^^ {
      case intPart ~ None => IntLiteral(intPart.mkString.toLong)
      case intPart ~ Some(fracPart) => DoubleLiteral((intPart.mkString + fracPart).toDouble)
    }

  // StringLiteral

  private def charInString(quote: Char): Parser[Char] =
    ('\\' ~> (
      oneOfChar(Seq('\'', '\"', '\\'), (_: Char) => "")
        | ('r' ^^ { _ => '\r' })
        | ('n' ^^ { _ => '\n' })
        | ('r' ^^ { _ => '\r' })
        | ('t' ^^ { _ => '\t' })
        | failure("invalid escape sequence")
      )) | elem({ case c if c != quote => c }, _ => "unexpected quote")

  private def stringQuotedHelper(quote: Char): Parser[StringLiteral] =
    (quote ~> rep(charInString(quote)) <~ quote) ^^ { c => StringLiteral(c.mkString, quote) }

  lazy val STRING_SINGLE_QUOTED: Parser[StringLiteral] = stringQuotedHelper('\'')

  lazy val STRING_DOUBLE_QUOTED: Parser[StringLiteral] = stringQuotedHelper('\"')

  case class TokenReader(in: CharReader) extends Reader[Token] {
    def this(in: String) = this(CharReader(in))

    private val (afterWs, tokOption, afterTok) = parse(WHITESPACE, in) match {
      case Accept(_, afterWs) if afterWs.isEmpty => (afterWs, None, afterWs)
      case Accept(_, afterWs) =>
        parse(TOKEN, afterWs) match {
          case Accept(tok, afterTok) => (afterWs, Some(tok), afterTok)
          case Reject(msg, _, _) =>
            throw new RuntimeException(s"token lexing failed: $msg")
          //            (afterWs, errorToken(msg), afterWs.drop(afterWs.length))
        }
      case Reject(msg, _, _) =>
        throw new RuntimeException(s"whitespace lexing failed: $msg")
      //        (in, errorToken(msg), in.drop(in.length))
    }

    override def headOption: Option[Token] = tokOption

    override def tail: TokenReader = TokenReader(afterTok)

    override def loc: SourceLocation = afterWs.loc

    override def isEmpty: Boolean = in.isEmpty || afterWs.isEmpty
  }
}
