package tinycc.common

trait Graph[T] {
  def nodes: Seq[T]

  def getSucc(node: T): Seq[T]

  def getPred(node: T): Seq[T]

  def getOutDegree(node: T): Int = getSucc(node).size

  def getInDegree(node: T): Int = getPred(node).size

  def getDegree(node: T): Int = getOutDegree(node) + getInDegree(node)
}
