package tinycc.util

import scala.collection.mutable

/** A disjoint-set union data structure. */
class DSU[T] {
  private val parents: mutable.Map[T, T] = mutable.Map.empty.withDefault(p => p)

  def find(a: T): T = {
    val par = parents(a)
    if (par != a) {
      val par2 = find(par)
      parents(a) = par2
      par2
    } else par
  }

  /** Marks child as an alias for parent. */
  def union(parent: T, child: T): T = {
    val newParent = find(parent)
    parents(find(child)) = newParent
    newParent
  }
}