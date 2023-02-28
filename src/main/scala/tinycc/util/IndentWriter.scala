package tinycc.util

import java.io.Writer

class IndentWriter(inner: Writer, indent: String = "  ") extends Writer {
  private var level: Int = 0

  private var isStartOfLine: Boolean = false

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    for (i <- 0.until(len)) {
      val ch = cbuf(i + off)
      if (ch != '\r' && ch != '\n' && isStartOfLine) {
        inner.write(indent * level)
        isStartOfLine = false
      }
      inner.write(ch)
      if (cbuf(i) == '\n') {
        isStartOfLine = true
      }
    }
  }

  override def flush(): Unit = inner.flush()

  override def close(): Unit = inner.close()

  def nl(): Unit = write("\n")

  def indent(): Unit = {
    level += 1
    if (!isStartOfLine)
      nl()
  }

  def dedent(): Unit = {
    level -= 1
    if (!isStartOfLine)
      nl()
  }

  def withIndent[R](thunk: => R): R = {
    indent()
    try
      thunk
    finally
      dedent()
  }
}
