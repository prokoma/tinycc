package tinycc.frontend.ast

case class SourceLocation(line: Int, col: Int) extends Ordered[SourceLocation] {
  override def toString: String = s"$line:$col"

  override def compare(that: SourceLocation): Int = {
    val d = this.line - that.line
    if (d == 0) this.col - that.col else d
  }
}
