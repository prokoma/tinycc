package tinycc

class ProgramException(val level: ErrorLevel, message: String) extends RuntimeException(message) {

}

sealed trait ErrorLevel extends Product with Serializable

object ErrorLevel {
  case object Error extends ErrorLevel

  case object Warning extends ErrorLevel

  case object Note extends ErrorLevel
}
