package tinycc.util.parsing.combinator

import tinycc.util.parsing.SourceLocation

trait Reader[T] {
  def headOption: Option[T]

  def tail: Reader[T]

  def isEmpty: Boolean = headOption.isEmpty

  def loc: SourceLocation

  def iterator: Iterator[T] = Iterator.unfold(this)(r => r.headOption.map(h => (h, r.tail)))
}
