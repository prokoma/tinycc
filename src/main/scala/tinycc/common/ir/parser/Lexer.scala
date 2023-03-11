package tinycc.common.ir.parser

import tinycc.util.parsing.combinator.{Lexical, Scanners}

object Lexer extends Lexical with Scanners {
  sealed trait Token extends Product with Serializable

  object Token {
    /** An operator, keyword or some other special character. */
    case class Special(value: Symbol) extends Token {
      override def toString: String = s"Special('${value.name}')"
    }

    case class Register(value: Symbol) extends Token {
      override def toString: String = s"Register('%${value.name}')"
    }

    case class Identifier(value: Symbol) extends Token {
      override def toString: String = s"Identifier('${value.name}')"
    }

    /** An integer literal. */
    case class IntLiteral(value: Long) extends Token

    case class DoubleLiteral(value: Double) extends Token
  }

  import Token._

  implicit def char2parser(c: Char): Parser[Char] = elem(c)

  implicit def string2parser(s: String): Parser[String] = elem(s)

  override lazy val WHITESPACE: Parser[Any] = rep[Any](
    whitespace
      | ("//" ~ rep(not(NL) ~> elem("", { case c => c })))
      | ("/*" ~ commit(blockCommentEnd.described("block comment end ('*/')")))
  )

  def blockCommentEnd: Parser[Any] =
    rep(elem("", { case c if c != '*' => () })) ~ '*' ~ (elem('/') | blockCommentEnd)

  // Token

  lazy val TOKEN: Parser[Token] =
    REGISTER | OPERATOR | IDENTIFIER_OR_KEYWORD | NUMERIC_LITERAL

  // Operator

  lazy val OPERATOR: Parser[Special] = oneOfSymbol("operator", Seq(
    Symbols.colon,
    Symbols.comma,
    Symbols.parOpen,
    Symbols.parClose,
    Symbols.squareOpen,
    Symbols.squareClose,
    Symbols.curlyOpen,
    Symbols.curlyClose,
    Symbols.assign,
  )) ^^ Special

  private val keywords: Seq[Symbol] = Seq(
    Symbols.kwFn,
    Symbols.kwLabel,
    Symbols.kwI64,
    Symbols.kwDouble,
    Symbols.kwVoid,
    Symbols.kwPtr,
    Symbols.kwStruct,
    Symbols.kwNull,
  )

  lazy val IDENTIFIER_OR_KEYWORD: Parser[Token] = IDENTIFIER ^^ { ident =>
    if (keywords.contains(ident.value)) Special(ident.value) else ident
  }

  // Identifier

  lazy val identifierStart: Parser[Char] =
    letter | '_' | '$'

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

  // Register

  lazy val REGISTER: Parser[Register] = '%' ~> rep1(identifierMid) ^^ { name => Register(Symbol(name.mkString)) }
}