package tinycc.common.analysis.dataflow

import tinycc.common.{Cfg, Graph}
import tinycc.common.analysis.dataflow.Lattice.MapLattice

/** A monotone framework dataflow analysis. */
trait DataflowAnalysis {
  type NodeState
  type Node
  type ProgState = Map[Node, NodeState]

  def nodeStateLattice: Lattice[NodeState]

  def progStateLattice: MapLattice[Node, NodeState]

  /** The node transfer function, which can query state of its predecessors or successors (depending on analysis direction). */
  def transfer(node: Node, progState: ProgState): NodeState

  /** The direction of the analysis. */
  def forward: Boolean

  /** Computes the fixed point for the CFG. */
  def fixpoint(): ProgState
}

object DataflowAnalysis {
  abstract class Builder[T](graph: Graph[T], val forward: Boolean) extends DataflowAnalysis {
    type Node = T

    lazy val progStateLattice: MapLattice[Node, NodeState] = Lattice.lazyMapLattice(graph.nodes, nodeStateLattice)

    protected def progNodes: Seq[Node] = graph.nodes

    /** Returns nodes over which we perform the join. */
    protected def getNodeDependencies(node: Node): Iterable[Node] =
      if (forward) graph.getPred(node) else graph.getSucc(node)

    /** Returns nodes which are affected by a change in the given node. Inverse of getNodeDependencies. */
    protected def getNodeDependents(node: Node): Iterable[Node] =
      if (forward) graph.getSucc(node) else graph.getPred(node)

    /** Returns a joined state of the successors or predecessors of the node (depending on the analysis direction). */
    def join(node: Node, cfgState: ProgState): NodeState = {
      val prevStates = getNodeDependencies(node).map(w => cfgState(w)) // state of all preds (forward) or succ (reverse)
      prevStates.foldLeft(nodeStateLattice.bot)(nodeStateLattice.lub)
    }
  }
}

