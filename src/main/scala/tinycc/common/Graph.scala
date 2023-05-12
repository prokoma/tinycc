package tinycc.common

trait Graph[T] {
  def nodes: Seq[T]

  def getSucc(node: T): Seq[T]

  def getPred(node: T): Seq[T]
}
