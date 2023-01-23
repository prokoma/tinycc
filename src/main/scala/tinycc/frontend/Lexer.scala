package tinycc
package frontend

import scala.language.implicitConversions

object Lexer extends Lexical {
  def errorToken(msg: String): Token[_] = ErrorToken(msg)

  // whitespace

  lazy val whitespace: Parser[Any] = rep[Any](
    whitespaceChar
 //     | ("//" ~ rep(not(nl) ~> elem))
 //     | ("/*" ~ (blockCommentEnd | failure("unclosed comment")))
  )

  lazy val whitespaceChar: Parser[Char] =
    elem(List('\r', '\n', '\t', ' '), c => s"expected whitespace, got '$c'")

  def blockCommentEnd: Parser[Any] =
    rep(chrExcept('*')) ~ '*' ~ (elem('/') | blockCommentEnd)

  // Token

  lazy val token: Parser[Token] = (
    operator | identifier | numericLiteral | stringSingleQuoted | stringDoubleQuoted
      | (eof ^^ { _ => EOF })
    )

  // Operator

  lazy val operator: Parser[Special] = elem(List(
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
    Symbols.nEq,
    Symbols.lt,
    Symbols.gt,
    Symbols.lte,
    Symbols.gte,
    Symbols.bitAnd,
    Symbols.bitOr,
    Symbols.and,
    Symbols.or,
    Symbols.not,
    Symbols.neg,
    Symbols.xor,
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
  ), _ => "expected operator") ^^ Special

  lazy val keyword: Parser[Special] = elem(List(
    Symbols.kwBreak,
    Symbols.kwCase,
    Symbols.kwCast,
    Symbols.kwChar,
    Symbols.kwContinue,
    Symbols.kwDefault,
    Symbols.kwDefine,
    Symbols.kwDefmacro,
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
  ), _ => "expected keyword") ^^ Special

  lazy val letter: Parser[Char] =
    elem("letter", ch => (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))

  lazy val digit: Parser[Char] =
    elem("digit", ch => ch >= '0' && ch <= '9')

  // Identifier

  lazy val identifierStart: Parser[Char] =
    letter | '_'

  lazy val identifierMid: Parser[Char] =
    identifierStart | digit

  lazy val identifier: Parser[Token] =
    (identifierStart ~ rep(identifierMid)) ^^ { case head ~ tail => Identifier((head :: tail).mkString) }

  // IntLiteral or DoubleLiteral

  lazy val numericLiteral: Parser[Token] =
    rep1(digit) ^^ { digits => IntLiteral(digits.mkString.toLong) }

  // StringLiteral

  lazy val charOrEscapeSequence: Parser[Char] =
    ('\\' ~> (
      elem('\'') | elem('\"') | elem('\\')
        | ('r' ^^ { _ => '\r' })
        | ('n' ^^ { _ => '\n' })
        | ('r' ^^ { _ => '\r' })
        | ('t' ^^ { _ => '\t' })
        | failure("invalid escape sequence")
      )) | elem

  private def stringQuotedHelper(quote: Char): Parser[StringLiteral] =
    (quote ~> rep(elem.filter(_ != quote)) <~ quote) ^^ { c => StringLiteral(c.mkString, quote) }

  lazy val stringSingleQuoted: Parser[StringLiteral] = stringQuotedHelper('\'')

  lazy val stringDoubleQuoted: Parser[StringLiteral] = stringQuotedHelper('\"')
}
