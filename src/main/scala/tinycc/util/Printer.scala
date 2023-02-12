package tinycc.util

import java.io.{StringWriter, Writer}

trait Printer[T] {
  def print(obj: T, writer: Writer): Unit

  def printToString(obj: T): String = {
    val writer = new StringWriter()
    print(obj, writer)
    writer.toString
  }
}

trait IndentPrinter[T] extends Printer[T] {
  def defaultIndent: String = "  "

  def print(obj: T, writer: Writer): Unit = {
    val out = new IndentWriter(writer, defaultIndent)
    print(obj, out)
  }

  def print(obj: T, out: IndentWriter): Unit
}

//class ToStringPrinter extends Printer[Any] {
//  override def printToString(obj: Any): String = obj.toString
//
//  override def print(obj: Any, writer: Writer): Unit = writer.write(obj.toString)
//}