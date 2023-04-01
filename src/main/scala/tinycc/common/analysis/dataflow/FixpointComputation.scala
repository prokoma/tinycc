package tinycc.common.analysis.dataflow

import scala.collection.mutable

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
