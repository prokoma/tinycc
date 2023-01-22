package tinycc
package frontend

import tinycc.util.parsing.combinator.Lexical

import scala.language.implicitConversions

object Lexer extends Lexical {
  type Token = tinycc.frontend.Token

  override def errorToken(msg: String): Token = ErrorToken(msg)

  // whitespace

  lazy val nl: Parser[Any] = opt('\r') ~ '\n'

  lazy val whitespace: Parser[Any] = rep[Any](
    whitespaceChar
 //     | ("//" ~ rep(not(nl) ~> elem))
 //     | ("/*" ~ (blockCommentEnd | failure("unclosed comment")))
  )

  lazy val whitespaceChar: Parser[Char] = elem("whitespace", ch => ch == '\r' || ch == '\n' || ch == '\t' || ch == ' ')

  def blockCommentEnd: Parser[Any] =
    rep(chrExcept('*')) ~ '*' ~ (elem('/') | blockCommentEnd)

  // Token

  lazy val token: Parser[Token] = (
    operator | identifier | numericLiteral | stringSingleQuoted | stringDoubleQuoted
      | (eof ^^ { _ => EOF })
    )

  // Operator

  lazy val operator: Parser[Token] = (
    "/=" | "/"
      | "++" | "+=" | "+"
      | "--" | "-=" | "->" | "-"
      | "*=" | "*"
      | "!=" | "!"
      | "==" | "="
      | "<<" | "<=" | "<"
      | ">>" | ">=" | ">"
      | "||" | "|"
      | "&&" | "&"
      | "%" | "." | "," | ";" | ":" | "?" | "[" | "]" | "(" | ")" | "{" | "}" | "~" | "`"
    ) ^^ { s => Operator(s) }

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
