package tinycc
package frontend

sealed trait Token[T] {
  def value: T
}

/** An operator, keyword or some other special character. */
case class Special(value: Symbol) extends Token[Symbol]

/** A valid TinyC identifier (variable, type or function name). */
case class Identifier(value: Symbol) extends Token[Symbol]

/** An integer literal. */
case class IntLiteral(value: Long) extends Token[Long]

case class DoubleLiteral(value: Double) extends Token[Double]

case class StringLiteral(value: String, quote: Char) extends Token[String]

case object EOF extends Token[Unit] {
  def value: Unit = ()
}

case class ErrorToken(msg: String) extends Token[Unit] {
  def value: Unit = ()
}