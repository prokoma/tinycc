package tinycc.common.analysis.dataflow

import scala.collection.mutable

object FixpointComputation {
  /** Defines the transfer function for the whole CFG and uses it to naively find the fixed point. */
  trait Naive extends DataflowAnalysis {
    def transferProgState(state: ProgState): ProgState =
      state.transform((node, _) => transfer(node, state))

    override def fixpoint(): ProgState = {
      var cur = progStateLattice.bot
      var prev = cur
      do {
        prev = cur
        cur = transferProgState(prev)
      } while (cur != prev)
      cur
    }
  }

  /** Uses a worklist to only call node transfer functions on nodes that may return a new result. */
  trait Worklist extends DataflowAnalysis {
    protected def getNodeDependents(node: Node): Iterable[Node]

    protected def progNodes: Seq[Node]

    override def fixpoint(): ProgState = {
      val workList = mutable.Queue.from(if (forward) progNodes else progNodes.reverseIterator)
      var progState = progStateLattice.bot

      while (workList.nonEmpty) {
        val node = workList.dequeue()
        val newNodeState = transfer(node, progState)
        if (progState(node) != newNodeState) {
          progState += (node -> newNodeState)
          workList.enqueueAll(getNodeDependents(node))
        }
      }
      progState
    }
  }
}
