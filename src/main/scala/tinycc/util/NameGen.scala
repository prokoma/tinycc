package tinycc.util

import java.util.concurrent.atomic.AtomicInteger

class NameGen(val globalPrefix: String = "") {
  private val ctr: AtomicInteger = new AtomicInteger(0)

  def apply(prefix: String = ""): String =
    globalPrefix + prefix + ctr.getAndIncrement()

  def reset(): Unit = ctr.set(0)
}