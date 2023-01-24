package tinycc
package frontend

sealed trait Token

/** An operator, keyword or some other special character. */
case class Special(value: Symbol) extends Token {
  override def toString: String = s"Special('${value.name}')"
}

/** A valid TinyC identifier (variable, type or function name). */
case class Identifier(value: Symbol) extends Token{
  override def toString: String = s"Identifier('${value.name}')"
}

/** An integer literal. */
case class IntLiteral(value: Long) extends Token

case class DoubleLiteral(value: Double) extends Token

case class StringLiteral(value: String, quote: Char) extends Token {
  override def toString: String = s"StringLiteral($quote$value$quote)"
}

case object EOF extends Token

case class ErrorToken(msg: String) extends Token