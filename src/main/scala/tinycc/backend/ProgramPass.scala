package tinycc.backend

abstract class ProgramPass[T] {
  def transformProgram(program: T): Unit
}
