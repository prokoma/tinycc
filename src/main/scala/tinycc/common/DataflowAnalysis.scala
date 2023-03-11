package tinycc.common

import scala.collection.mutable

trait Lattice[T] {
  type Inst = T

  def top: T

  def bot: T

  // ⊔
  def lub(x: T, y: T): T

  // ⊓
  def glb(x: T, y: T): T
}

object Lattice {
  def powersetLat[T](domain: => Set[T]): Lattice[Set[T]] = new Lattice[Set[T]] {
    override lazy val top: Set[T] = domain

    override val bot: Set[T] = Set.empty

    override def lub(x: Set[T], y: Set[T]): Set[T] = x.union(y)

    override def glb(x: Set[T], y: Set[T]): Set[T] = x.intersect(y)
  }
}

case class MapLattice[K, V](domain: Iterable[K], sub: Lattice[V]) extends Lattice[Map[K, V]] {
  override def top: Inst = domain.map(key => key -> sub.top).toMap

  override def bot: Inst = domain.map(key => key -> sub.bot).toMap

  override def lub(x: Inst, y: Inst): Inst = x.transform((key, xVal) => sub.lub(xVal, y(key)))

  override def glb(x: Inst, y: Inst): Inst = x.transform((key, xVal) => sub.glb(xVal, y(key)))
}

//case class PowersetLattice[T](lazyDomain: () => Set[T]) extends Lattice[Set[T]] {
//  lazy val top: Inst = lazyDomain()
//
//  override def bot: Inst = Set.empty
//
//  override def lub(x: Set[T], y: Set[T]): Set[T] = x.union(y)
//
//  override def glb(x: Set[T], y: Set[T]): Set[T] = x.intersect(y)
//}

case class InvLattice[T](subLattice: Lattice[T]) extends Lattice[T] {
  override def top: Inst = subLattice.bot

  override def bot: Inst = subLattice.top

  override def lub(x: T, y: T): T = subLattice.glb(x, y)

  override def glb(x: T, y: T): T = subLattice.lub(x, y)
}

trait DataflowAnalysis {
  type NodeState
  type CfgNode
  type CfgState = Map[CfgNode, NodeState]

  def nodeStateLattice: Lattice[NodeState]

  def cfgStateLattice: MapLattice[CfgNode, NodeState]

  def transfer(node: CfgNode, joinedState: NodeState): NodeState

  def forward: Boolean

  def join(node: CfgNode, cfgState: CfgState): NodeState

  def fixpoint(): CfgState
}

object DataflowAnalysis {
  // TODO: maybe remove this class
  abstract class Builder[T](cfg: Cfg[T], val forward: Boolean) extends DataflowAnalysis {
    type CfgNode = T

    lazy val cfgStateLattice = new MapLattice[CfgNode, NodeState](cfg.nodes, nodeStateLattice)

    protected def cfgNodes: Seq[CfgNode] = cfg.nodes

    /** Returns nodes over which we perform the join. */
    protected def getNodeDependencies(node: CfgNode): Iterable[CfgNode] =
      if (forward) cfg.getPred(node) else cfg.getSucc(node)

    /** Returns nodes which are affected by a change in the given node. Inverse of getNodeDependencies. */
    protected def getNodeDependents(node: CfgNode): Iterable[CfgNode] =
      if (forward) cfg.getSucc(node) else cfg.getPred(node)

    def join(node: CfgNode, cfgState: CfgState): NodeState = {
      val prevStates = getNodeDependencies(node).map(w => cfgState(w)) // state of all preds (forward) or succ (reverse)
      prevStates.foldLeft(nodeStateLattice.bot)(nodeStateLattice.lub)
    }
  }
}

object FixpointComputation {
  trait Naive extends DataflowAnalysis {
    def transferCfgState(state: CfgState): CfgState =
      state.transform((node, _) => transfer(node, join(node, state)))

    override def fixpoint(): CfgState = {
      var cur = cfgStateLattice.bot
      var prev = cur
      do {
        prev = cur
        cur = transferCfgState(prev)
      } while (cur != prev)
      cur
    }
  }

  trait Worklist extends DataflowAnalysis {
    protected def getNodeDependents(node: CfgNode): Iterable[CfgNode]

    protected def cfgNodes: Seq[CfgNode]

    override def fixpoint(): CfgState = {
      val workList = mutable.Queue.from(if (forward) cfgNodes else cfgNodes.reverseIterator)
      var cfgState = cfgStateLattice.bot

      while (workList.nonEmpty) {
        val node = workList.dequeue()
        val newNodeState = transfer(node, join(node, cfgState))
        if (cfgState(node) != newNodeState) {
          cfgState += (node -> newNodeState)
          workList.enqueueAll(getNodeDependents(node))
        }
      }
      cfgState
    }
  }
}