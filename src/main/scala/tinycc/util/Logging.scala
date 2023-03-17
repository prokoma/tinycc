package tinycc.util

trait Logging {
  import Logging._

  def name: String = getClass.getSimpleName

  def log(message: => String): Unit = {
    if (enableLogging)
      Console.err.println(s"[$name] $message")
  }
}

object Logging {
  var enableLogging: Boolean = false
}