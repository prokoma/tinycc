package tinycc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

package object t86 {
  type T86Program = Seq[T86ProgramElement]

  type T86ProgramBuilder = mutable.ListBuffer[T86ProgramElement]
}
