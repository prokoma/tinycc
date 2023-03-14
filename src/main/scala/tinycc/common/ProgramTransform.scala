package tinycc.common

import tinycc.util.Logging

abstract class ProgramTransform[T] extends Logging {
  def transformProgram(program: T): Unit
}
