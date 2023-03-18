package tinycc.util.parsing.combinator

import tinycc.util.parsing.SourceLocation

import scala.language.implicitConversions

case class SeqReader[T](seq: Seq[T], offset: Int = 0) extends Reader[T] {
  override def headOption: Option[T] = seq.headOption

  override def isEmpty: Boolean = seq.isEmpty

  override def tail: SeqReader[T] = SeqReader(seq.tail, offset + 1)

  override def loc: SourceLocation = SourceLocation(0, offset, offset)

  override def iterator: Iterator[T] = seq.iterator
}

trait SeqParsers[T] extends Parsers {
  type Input = SeqReader[T]

  lazy val elem: Parser[T] = (in: Input) => in.headOption match {
    case Some(tok) => Accept(tok, in.tail)
    case None => Reject(in)
  }

  def elem[R](message: String, fn: PartialFunction[T, R]): Parser[R] = (in: Input) => in.headOption match {
    case Some(tok) => fn.andThen(Accept(_, in.tail)).applyOrElse(tok, (_: T) => Reject(message, in))
    case None => Reject(message, in)
  }

  def elemToString(e: T): String = e.toString

  implicit def elem(e: T): Parser[T] = elem(elemToString(e), { case c if e == c => c })
}