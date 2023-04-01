package tinycc.common.analysis.dataflow

import tinycc.common.Cfg
import tinycc.common.analysis.dataflow.Lattice.MapLattice

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

