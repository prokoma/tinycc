package tinycc.common

abstract class ProgramTransform[T] {
  def name: String = this.getClass.getSimpleName

  def transformProgram(program: T): Unit
}
