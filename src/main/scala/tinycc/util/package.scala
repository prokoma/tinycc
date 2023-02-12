package tinycc

package object util {
  implicit class IterableForeachSep[T](that: Iterable[T]) {
    def foreachSep[U](f: T => U, sep: => Any): Unit = {
      val it = that.iterator
      while (it.hasNext) {
        f(it.next())
        if (it.hasNext) sep
      }
    }
  }
}
