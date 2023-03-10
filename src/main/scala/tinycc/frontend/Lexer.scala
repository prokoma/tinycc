package tinycc
package frontend

import tinycc.util.parsing.SourceLocation
import tinycc.util.parsing.combinator.{CharReader, Lexical, Reader}

import scala.language.implicitConversions

object Lexer extends Lexical {
  implicit def char2parser(c: Char): Parser[Char] = elem(c)

  implicit def string2parser(s: String): Parser[String] = elem(s)

  lazy val WHITESPACE: Parser[Any] = rep[Any](
    whitespaceChar
      | ("//" ~ rep(not(NL) ~> elem("", { case c => c })))
      | ("/*" ~ commit(blockCommentEnd.described("block comment end ('*/')")))
  )

  lazy val whitespaceChar: Parser[Char] = oneOfChar("whitespace", Seq('\r', '\n', '\t', ' '))

  def blockCommentEnd: Parser[Any] =
    rep(elem("", { case c if c != '*' => () })) ~ '*' ~ (elem('/') | blockCommentEnd)

  // Token

  lazy val TOKEN: Parser[Token] =
    OPERATOR | IDENTIFIER_OR_KEYWORD | NUMERIC_LITERAL | STRING_SINGLE_QUOTED | STRING_DOUBLE_QUOTED

  // Operator

  lazy val OPERATOR: Parser[Special] = oneOfSymbol("operator", Seq(
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
  )) ^^ Special

  private val keywords: Seq[Symbol] = Seq(
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
    Symbols.kwPrintnum,
  )

  lazy val IDENTIFIER_OR_KEYWORD: Parser[Token] = IDENTIFIER ^^ { ident =>
    if (keywords.contains(ident.value)) Special(ident.value) else ident
  }

  // Identifier

  lazy val identifierStart: Parser[Char] =
    letter | '_'

  lazy val identifierMid: Parser[Char] =
    identifierStart | digit

  lazy val IDENTIFIER: Parser[Identifier] =
    (identifierStart ~ rep(identifierMid)) ^^ { case head ~ tail => Identifier(Symbol((head :: tail).mkString)) } described "identifier"


  // IntLiteral or DoubleLiteral

  lazy val NUMERIC_LITERAL: Parser[Token] =
    rep1(digit) ~ opt('.' ~ rep(digit) ^^ { case dot ~ digits => dot + digits.mkString }) ^^ {
      case intPart ~ None => IntLiteral(intPart.mkString.toLong)
      case intPart ~ Some(fracPart) => DoubleLiteral((intPart.mkString + fracPart).toDouble)
    } described "numeric literal"

  // StringLiteral

  private def charInString(quote: Char): Parser[Char] =
    ('\\' ~> (
      oneOfChar("", Seq('\'', '\"', '\\'))
        | ('r' ^^ { _ => '\r' })
        | ('n' ^^ { _ => '\n' })
        | ('t' ^^ { _ => '\t' })
      )).described("escape sequence") | elem("", { case c if c != quote => c })

  private def stringQuotedHelper(quote: Char): Parser[StringLiteral] =
    (quote ~> rep(charInString(quote)) <~ quote) ^^ { c => StringLiteral(c.mkString, quote) }

  lazy val STRING_SINGLE_QUOTED: Parser[StringLiteral] = stringQuotedHelper('\'') described "single quoted string"

  lazy val STRING_DOUBLE_QUOTED: Parser[StringLiteral] = stringQuotedHelper('\"') described "double quoted string"

  case class TokenReader(in: CharReader) extends Reader[Token] {
    def this(in: String) = this(CharReader(in))

    private val (afterWs, tokOption, afterTok) = parse(WHITESPACE, in) match {
      case Accept(_, afterWs, _) if afterWs.isEmpty => (afterWs, None, afterWs)
      case Accept(_, afterWs, _) =>
        parse(TOKEN, afterWs) match {
          case Accept(tok, afterTok, _) => (afterWs, Some(tok), afterTok)
          case Reject(msg, remainder, _) =>
            throw new TinyCParserException(formatErrorMessage(msg, remainder), remainder.loc)
        }
      case Reject(msg, remainder, _) =>
        throw new TinyCParserException(formatErrorMessage(msg, remainder), remainder.loc)
    }

    override def headOption: Option[Token] = tokOption

    override def tail: TokenReader = TokenReader(afterTok)

    override def loc: SourceLocation = afterWs.loc

    override def isEmpty: Boolean = in.isEmpty || afterWs.isEmpty
  }
}
