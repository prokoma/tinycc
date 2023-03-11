package tinycc.util.parsing

import tinycc.util.Reporter
import tinycc.{ErrorLevel, ProgramException}

class ParserException(message: String, val loc: SourceLocation) extends ProgramException(message) {
  override def format(reporter: Reporter): String = reporter.formatError(ErrorLevel.Error, message, loc)
}
