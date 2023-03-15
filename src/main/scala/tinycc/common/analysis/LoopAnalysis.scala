package tinycc.common.analysis

import tinycc.common.Cfg

import scala.collection.mutable

/** Analyses which CFG nodes are part of a loop. */
class LoopAnalysis[T](cfg: Cfg[T]) {
  private def topsort(): Seq[T] = {
    var stack = List.empty[T]
    var visited = Set.empty[T]
    def dfs(u: T): Unit = {
      if(visited.contains(u))
        return
      visited += u
      cfg.getSucc(u).foreach(dfs)
      stack ::= u
    }
    cfg.nodes.foreach(dfs)
    stack
  }

  def result(): Set[T] = {
    // an implementation of Kosaraju's algorithm to find strongly connected components
    val componentSizes = mutable.Map.empty[T, Int].withDefaultValue(0)
    val nodeComponents = mutable.Map.empty[T, T]
    def assign(u: T, comp: T): Unit = {
      if(nodeComponents.contains(u))
        return
      nodeComponents(u) = comp
      componentSizes(comp) += 1
      cfg.getPred(u).foreach(v => assign(v, comp))
    }
    topsort().foreach(u => assign(u, u))

    // node is in loop if it is not alone in its strongly connected component or its is self loop
    cfg.nodes.filter(u => componentSizes(nodeComponents(u)) > 1 || cfg.getSucc(u).contains(u)).toSet
  }
}