package tinycc

import tinycc.cli.Reporter

class ProgramException(val message: String) extends RuntimeException(message) {
  def format(reporter: Reporter): String = reporter.formatError(ErrorLevel.Error, message)
}

sealed trait ErrorLevel extends Product with Serializable

object ErrorLevel {
  case object Error extends ErrorLevel

  case object Warning extends ErrorLevel

  case object Note extends ErrorLevel
}
