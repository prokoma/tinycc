package tinycc.frontend.parser

import tinycc.util.parsing.combinator.{Lexical, Scanners}

import scala.language.implicitConversions

object Lexer extends Lexical with Scanners {
  sealed trait Token extends Product with Serializable

  object Token {
    /** An operator, keyword or some other special character. */
    case class Special(value: Symbol) extends Token {
      override def toString: String = s"Special('${value.name}')"
    }

    /** A valid TinyC identifier (variable, type or function name). */
    case class Identifier(value: Symbol) extends Token {
      override def toString: String = s"Identifier('${value.name}')"
    }

    /** An integer literal. */
    case class IntLiteral(value: Long) extends Token

    case class DoubleLiteral(value: Double) extends Token

    case class StringLiteral(value: String, quote: Char) extends Token {
      override def toString: String = s"StringLiteral($quote$value$quote)"
    }
  }

  import Token._

  implicit def char2parser(c: Char): Parser[Char] = elem(c)

  implicit def string2parser(s: String): Parser[String] = elem(s)

  override lazy val WHITESPACE: Parser[Any] = rep[Any](
    whitespace
      | ("//" ~ rep(not(NL) ~> elem("", { case c => c })))
      | ("/*" ~ commit(blockCommentEnd described "block comment end ('*/')"))
  )

  def blockCommentEnd: Parser[Any] =
    rep(elem("", { case c if c != '*' => () })) ~ '*' ~ (elem('/') | blockCommentEnd)

  // Token

  override lazy val TOKEN: Parser[Token] =
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

  lazy val intPart: Parser[String] = rep1(digit) ^^ (digits => digits.mkString)

  lazy val fracPart: Parser[String] = '.' ~> commit(rep(digit)) ^^ (digits => s".${digits.mkString}")

  lazy val exponentPart: Parser[String] = (elem('e') | 'E') ~> commit(opt(elem('-') | '+') ~ rep1(digit)) ^^ { case sign ~ digits => s"e${sign.getOrElse("")}${digits.mkString}" }

  lazy val NUMERIC_LITERAL: Parser[Token] =
    intPart ~ opt(fracPart) ~ opt(exponentPart) ^^ {
      case intPart ~ None ~ None => IntLiteral(intPart.toLong)
      case intPart ~ fracPart ~ exponentPart => DoubleLiteral((intPart + fracPart.getOrElse("") + exponentPart.getOrElse("")).toDouble)
    } described "numeric literal"

  // StringLiteral

  private def charInString(quote: Char): Parser[Char] =
    (('\\' ~> commit(
      oneOfChar("", Seq('\'', '\"', '\\'))
        | ('r' ^^ { _ => '\r' })
        | ('n' ^^ { _ => '\n' })
        | ('t' ^^ { _ => '\t' })
        | ('0' ^^ { _ => 0.toChar })
      )) described "escape sequence") | elem("", { case c if c != quote => c })

  private def stringQuotedHelper(quote: Char): Parser[StringLiteral] =
    (quote ~> rep(charInString(quote)) <~ quote) ^^ { c => StringLiteral(c.mkString, quote) }

  lazy val STRING_SINGLE_QUOTED: Parser[StringLiteral] = stringQuotedHelper('\'') described "single quoted string"

  lazy val STRING_DOUBLE_QUOTED: Parser[StringLiteral] = stringQuotedHelper('\"') described "double quoted string"
}
