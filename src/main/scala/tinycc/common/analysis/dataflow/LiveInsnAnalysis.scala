package tinycc.common.analysis.dataflow

import tinycc.common.Graph
import tinycc.common.ir.Insn

class LiveInsnAnalysis(dfg: Graph[Insn]) extends DataflowAnalysis.Builder[Insn](dfg, false)
  with FixpointComputation.Worklist {

  import LiveInsnAnalysis._

  override type NodeState = Liveness

  override def nodeStateLattice: Lattice[Liveness] = livenessLattice

  override def transfer(node: Insn, progState: ProgState): Liveness = {
    if(node.hasSideEffects || join(node, progState) == Live) Live
    else Dead
  }

  def result(): Set[Insn] = fixpoint().collect({ case (insn, Live) => insn }).toSet
}

object LiveInsnAnalysis {
  sealed trait Liveness extends Product with Serializable

  case object Live extends Liveness

  case object Dead extends Liveness

  lazy val livenessLattice: Lattice[Liveness] = new Lattice[Liveness] {
    override def top: Liveness = Live

    override def bot: Liveness = Dead

    override def lub(x: Liveness, y: Liveness): Liveness =
      if(x == Live || y == Live) Live
      else Dead

    override def glb(x: Liveness, y: Liveness): Liveness =
      if(x == Dead || y == Dead) Dead
      else Live
  }
}