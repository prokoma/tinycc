package tinycc.util.parsing

/** line and col start at 1 */
case class SourceLocation(line: Int, col: Int, offset: Int) extends Ordered[SourceLocation] {
  override def toString: String = s"$line:$col"

  override def compare(that: SourceLocation): Int = {
    val d = this.line - that.line
    if (d == 0) this.col - that.col else d
  }
}
