package tinycc.common.analysis.dataflow

import scala.collection.mutable

object FixpointComputation {
  /** Defines the transfer function for the whole CFG and uses it to naively find the fixed point. */
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

  /** Uses a worklist to only call node transfer functions on nodes that may return a new result. */
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
