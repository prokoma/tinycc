package tinycc.common

trait Cfg[T] {
  def nodes: Seq[T]

  def entry: T

  def exit: Seq[T]

  def getSucc(node: T): Seq[T]

  def getPred(node: T): Seq[T]
}