package tinycc.cli

import tinycc.ErrorLevel
import tinycc.util.parsing.SourceLocation

import java.nio.file.{Files, Path}

class Reporter(source: String, fileName: Option[String] = None) {
  protected val lines: IndexedSeq[String] = source.linesIterator.toIndexedSeq

  import scala.io.AnsiColor._

  protected def formatFileName(): String =
    fileName.map(name => s"${BOLD}${name}:${RESET}").getOrElse("")

  protected def formatLocation(loc: SourceLocation): String =
    formatFileName() + loc.line + ":" + loc.col + ":"

  protected def formatLevel(level: ErrorLevel): String = level match {
    case ErrorLevel.Error => s"${RED}error:${RESET}"
    case ErrorLevel.Warning => s"${YELLOW}warning:${RESET}"
    case ErrorLevel.Note => s"${BLUE}note:${RESET}"
  }

  protected def formatContext(loc: SourceLocation): String = {
    f"${loc.line}%5d" + " | " + lines(loc.line - 1) + "\n" +
    "     " + " | " + (" " * (loc.col - 1)) + s"${BOLD}${MAGENTA}^${RESET}"
  }

  def formatError(level: ErrorLevel, message: String): String =
    formatFileName() + " " + formatLevel(level) + " " + message

  def formatError(level: ErrorLevel, message: String, loc: SourceLocation): String =
    formatLocation(loc) + " " + formatLevel(level) + " " + message + "\n" + formatContext(loc)
}

object Reporter {
  def apply(file: Path): Reporter =
    new Reporter(Files.readString(file), Some(file.getFileName.toString))
}