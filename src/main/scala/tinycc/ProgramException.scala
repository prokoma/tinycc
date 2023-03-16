package tinycc

import tinycc.util.{ErrorLevel, Reporter}

class ProgramException(val message: String, cause: Throwable = null) extends RuntimeException(message, cause) {
  def cause: Throwable = getCause

  def format(reporter: Reporter): String = reporter.formatError(ErrorLevel.Error, message)
}
