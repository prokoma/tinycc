package tinycc.common

import tinycc.common.Lattice.MapLattice

import scala.collection.mutable

trait Lattice[T] {
  type Elem = T

  def top: T

  def bot: T

  // ⊔
  def lub(x: T, y: T): T

  // ⊓
  def glb(x: T, y: T): T
}

object Lattice {
  type PowersetLattice[T] = Lattice[Set[T]]

  type MapLattice[K, V] = Lattice[Map[K, V]]

  def lazyPowersetLattice[T](domain: => Set[T]): Lattice[Set[T]] = new Lattice[Set[T]] {
    override lazy val top: Elem = domain

    override def bot: Elem = Set.empty

    override def lub(x: Elem, y: Elem): Elem = x.union(y)

    override def glb(x: Elem, y: Elem): Elem = x.intersect(y)
  }

  def lazyMapLattice[K, V](_domain: => Iterable[K], sub: Lattice[V]): Lattice[Map[K, V]] = new Lattice[Map[K, V]] {
    private lazy val domain = _domain

    override lazy val top: Elem = domain.map(key => key -> sub.top).toMap

    override lazy val bot: Elem = domain.map(key => key -> sub.bot).toMap

    override def lub(x: Elem, y: Elem): Elem = {
      require(x.keys.size == y.keys.size)
      x.transform((key, xVal) => sub.lub(xVal, y(key)))
    }

    override def glb(x: Elem, y: Elem): Elem = {
      require(x.keys.size == y.keys.size)
      x.transform((key, xVal) => sub.glb(xVal, y(key)))
    }
  }

  def invLattice[T](sub: Lattice[T]): Lattice[T] = new Lattice[T] {
    override def top: Elem = sub.bot

    override def bot: Elem = sub.top

    override def lub(x: Elem, y: Elem): Elem = sub.glb(x, y)

    override def glb(x: Elem, y: Elem): Elem = sub.lub(x, y)
  }
}

trait DataflowAnalysis {
  type NodeState
  type Node
  type CfgState = Map[Node, NodeState]

  def nodeStateLattice: Lattice[NodeState]

  def cfgStateLattice: MapLattice[Node, NodeState]

  def transfer(node: Node, joinedState: NodeState): NodeState

  def forward: Boolean

  def join(node: Node, cfgState: CfgState): NodeState

  def fixpoint(): CfgState
}

object DataflowAnalysis {
  abstract class Builder[T](cfg: Cfg[T], val forward: Boolean) extends DataflowAnalysis {
    type Node = T

    lazy val cfgStateLattice: MapLattice[Node, NodeState] = Lattice.lazyMapLattice(cfg.nodes, nodeStateLattice)

    protected def cfgNodes: Seq[Node] = cfg.nodes

    /** Returns nodes over which we perform the join. */
    protected def getNodeDependencies(node: Node): Iterable[Node] =
      if (forward) cfg.getPred(node) else cfg.getSucc(node)

    /** Returns nodes which are affected by a change in the given node. Inverse of getNodeDependencies. */
    protected def getNodeDependents(node: Node): Iterable[Node] =
      if (forward) cfg.getSucc(node) else cfg.getPred(node)

    def join(node: Node, cfgState: CfgState): NodeState = {
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
    protected def getNodeDependents(node: Node): Iterable[Node]

    protected def cfgNodes: Seq[Node]

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