package tinycc.common

trait Lattice[T] {
  type Inst = T

  def top: T

  def bot: T

  // ⊔
  def lub(x: T, y: T): T

  // ⊓
  def glb(x: T, y: T): T
}

case class MapLattice[K, V](domain: Iterable[K], sub: Lattice[V]) extends Lattice[Map[K, V]] {
  override def top: Inst = domain.map(key => key -> sub.top).toMap

  override def bot: Inst = domain.map(key => key -> sub.bot).toMap

  override def lub(x: Inst, y: Inst): Inst = x.transform((key, xVal) => sub.lub(xVal, y(key)))

  override def glb(x: Inst, y: Inst): Inst = x.transform((key, xVal) => sub.glb(xVal, y(key)))
}

case class PowersetLattice[T](lazyDomain: () => Set[T]) extends Lattice[Set[T]] {
  lazy val top: Inst = lazyDomain()

  override def bot: Inst = Set.empty

  override def lub(x: Set[T], y: Set[T]): Set[T] = x.union(y)

  override def glb(x: Set[T], y: Set[T]): Set[T] = x.intersect(y)
}

case class InvLattice[T](subLattice: Lattice[T]) extends Lattice[T] {
  override def top: Inst = subLattice.bot

  override def bot: Inst = subLattice.top

  override def lub(x: T, y: T): T = subLattice.glb(x, y)

  override def glb(x: T, y: T): T = subLattice.lub(x, y)
}

trait DataflowAnalysis {
  type NodeState
  type CfgNode
  type Cfg <: Graph[CfgNode]
  type CfgState = Map[CfgNode, NodeState]

  def nodeStateLattice: Lattice[NodeState]

  def cfgStateLattice: MapLattice[CfgNode, NodeState]

  def transfer(node: CfgNode, joinedState: NodeState): NodeState

  def forward: Boolean

  /** We join over those dependencies. */
  def getNodeDependencies(node: CfgNode): Iterable[CfgNode]

  def join(node: CfgNode, cfgState: CfgState): NodeState = {
    val prevStates = getNodeDependencies(node).map(w => cfgState(w)) // state of all preds (forward) or succ (reverse)
    prevStates.reduce(nodeStateLattice.lub)
  }

  def cfgNodes: Seq[CfgNode]

  def fixpoint(): CfgState
}

object DataflowAnalysis {
  // TODO: maybe remove this class
  abstract class Builder[T](cfg: Graph[T], val forward: Boolean) extends DataflowAnalysis {
    type CfgNode = T

    lazy val cfgStateLattice = new MapLattice[CfgNode, NodeState](cfg.nodes, nodeStateLattice)

    override protected def getNodeDependencies(node: CfgNode): Iterable[CfgNode] =
      if (forward) cfg.getPred(node) else cfg.getSucc(node)
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

  //    trait Worklist extends DataflowAnalysis {
  //
  //      /** Get nodes that need to be reprocessed after a node changes. Inverse of getNodeDependencies. */
  //      protected def getNodeDependents(node: CfgNode): Iterable[CfgNode]
  //
  //      override protected def findFixPoint(): CfgState = {
  //        val workList = mutable.Queue.from(cfgNodes)
  //        var progState = progLattice.bot
  //
  //        while (workList.nonEmpty) {
  //          val node = workList.dequeue()
  //          val newNodeState = transferFun(node, join(node, progState))
  //          if (progState(node) != newNodeState) {
  //            progState += (node -> newNodeState)
  //            workList.enqueueAll(getNodeDependents(node))
  //          }
  //        }
  //        progState
  //      }
}