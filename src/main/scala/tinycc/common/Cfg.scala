package tinycc.common

trait Cfg[T] extends Graph[T] {
  def entryNodes: Seq[T]

  def exitNodes: Seq[T]
}

object Cfg {
  def union[T](a: Cfg[T], b: Cfg[T]): Cfg[T] = new Cfg[T] {
    override def entryNodes: Seq[T] = a.entryNodes ++ b.entryNodes

    override def exitNodes: Seq[T] = a.exitNodes ++ b.exitNodes

    override def nodes: Seq[T] = a.nodes ++ b.nodes

    override def getSucc(node: T): Seq[T] = a.getSucc(node) match {
      case Seq() => b.getSucc(node)
      case succ => succ
    }

    override def getPred(node: T): Seq[T] = a.getPred(node) match {
      case Seq() => b.getPred(node)
      case succ => succ
    }
  }
}