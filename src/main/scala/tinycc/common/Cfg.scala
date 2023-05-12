package tinycc.common

trait Cfg[T] extends Graph[T] {
  def entry: T

  def exit: Seq[T]
}