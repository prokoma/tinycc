package tinycc

import tinycc.cli.Reporter

class ProgramException(val message: String, cause: Throwable = null) extends RuntimeException(message, cause) {
  def cause: Throwable = getCause

  def format(reporter: Reporter): String = reporter.formatError(ErrorLevel.Error, message)
}

sealed trait ErrorLevel extends Product with Serializable

object ErrorLevel {
  case object Error extends ErrorLevel

  case object Warning extends ErrorLevel

  case object Note extends ErrorLevel
}
