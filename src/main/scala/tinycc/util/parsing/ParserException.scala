package tinycc.util.parsing

import tinycc.util.{ErrorLevel, Reporter}
import tinycc.ProgramException

class ParserException(message: String, val loc: SourceLocation) extends ProgramException(message) {
  override def format(reporter: Reporter): String = reporter.formatError(ErrorLevel.Error, message, loc)
}
