package tinycc.util

import tinycc.util.Profiler.Task

import java.io.{PrintStream, PrintWriter}
import scala.util.Using

class Profiler {
  val rootTask = new Task("")
  var curTask: Task = rootTask

  def startTask(name: String): Task = {
    val task = new Task(name)
    curTask.children :+= task
    task.start()
    task
  }

  def profile[T](name: String, thunk: => T): T = {
    val oldCurTask = curTask
    curTask = startTask(name)
    try
      thunk
    finally {
      curTask.end()
      curTask = oldCurTask
    }
  }

  def printReport(out: IndentWriter): Unit = {
    rootTask.children.foreach(_.printReport(out))
  }

  def printReport(out: PrintStream): Unit = {
    val writer = new IndentWriter(new PrintWriter(out))
    printReport(writer)
    writer.flush()
  }
}

object Profiler {
  val theProfiler: Profiler = new Profiler

  class Task(val name: String, var children: Seq[Task] = Seq.empty) {
    var startTime: Long = 0
    var endTime: Long = 0

    def time: Long = endTime - startTime

    def selfTime: Long = time - children.map(_.time).sum

    def start(): Unit = {
      assert(startTime == 0)
      startTime = System.currentTimeMillis()
    }

    def end(): Unit = {
      assert(startTime != 0 && endTime == 0)
      endTime = System.currentTimeMillis()
    }

    def printReport(out: IndentWriter): Unit = {
      out.write(s"$name (total $time ms, self $selfTime ms)" + (if (children.isEmpty) "." else ":"))
      out.withIndent({
        children.foreach(_.printReport(out))
      })
    }
  }
}