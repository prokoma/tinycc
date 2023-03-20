package tinycc.util

import tinycc.util.Profiler.Task

import java.io.{PrintStream, PrintWriter}

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
    val task = startTask(name)
    curTask = task
    try
      thunk
    finally {
      task.end()
      curTask = oldCurTask
    }
  }

  def printReport(out: IndentWriter): Unit = {
    out.write("======= PROFILER REPORT =======\n")
    rootTask.children.foreach(_.printReport(out))
  }

  def printReport(out: PrintStream): Unit = {
    val writer = new IndentWriter(new PrintWriter(out))
    printReport(writer)
    writer.flush()
  }
}

object Profiler {
  val instance: Profiler = new Profiler

  def profile[T](name: String, thunk: => T): T = instance.profile(name, thunk)

  class Task(val name: String, var children: Seq[Task] = Seq.empty) {
    var startTime: Long = -1
    var endTime: Long = -1

    def time: Long = endTime - startTime

    def selfTime: Long = time - children.map(_.time).sum

    def start(): Unit = {
      assert(startTime == -1, "task has already started")
      startTime = System.currentTimeMillis()
    }

    def end(): Unit = {
      assert(startTime != -1, "task hasn't started")
      assert(endTime == -1, "task has already ended")
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