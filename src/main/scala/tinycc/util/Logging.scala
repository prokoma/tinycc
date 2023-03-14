package tinycc.util

trait Logging {
  def name: String = getClass.getSimpleName

  def log(message: => String): Unit = {
    Console.err.println(s"[$name] $message")
  }
}
