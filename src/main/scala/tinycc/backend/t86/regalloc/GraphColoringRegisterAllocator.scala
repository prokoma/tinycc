package tinycc.backend.t86.regalloc

import tinycc.backend.RegisterAllocator
import tinycc.backend.t86.regalloc.GraphColoringRegisterAllocator.{InterferenceGraph, T86BasicBlockLivenessAnalysis}
import tinycc.backend.t86.regalloc.T86RegisterAllocator.{DefUse, getInsnDefUse, isRegRegMove}
import tinycc.backend.t86.{T86BasicBlock, T86Fun, T86Program}
import tinycc.common._

import scala.collection.mutable

class GraphColoringRegisterAllocator(program: T86Program) extends T86RegisterAllocator(program: T86Program) {

  import T86RegisterAllocator._

  override def result(): T86Program = {
    program.funs.foreach(processFun)
    program
  }

  def processFun(fun: T86Fun): Unit = {
    // 1. construct CFG
    val cfg = T86BasicBlockCfg(fun)

    // 2. run liveness analysis on basic blocks
    val liveTemps = new T86BasicBlockLivenessAnalysis(cfg).result()

    // 3. compute live temporaries for each instruction in basic block and build interference graph
    val interfBuilder = InterferenceGraph.newBuilder
    fun.basicBlocks.foreach(bb => {
      var live = liveTemps.getLiveOut(bb)

      for (insn <- bb.insns.reverse) {
        val DefUse(defines, uses) = getInsnDefUse(insn)

        if (isRegRegMove(insn)) {
          live --= uses
          // TODO: add to move worklist
        }

        live ++= defines
        for (d <- defines if !isSpecialTemp(d); l <- live if !isSpecialTemp(l))
          interfBuilder.addEdge(d, l)
        live = (live -- defines) ++ uses
      }
    })
    val interf = interfBuilder.result()

    Console.out.println(interf.edges)
  }
}

object GraphColoringRegisterAllocator {

  import T86RegisterAllocator._

  // backwards must dataflow analysis
  class T86BasicBlockLivenessAnalysis(cfg: T86BasicBlockCfg)
    extends DataflowAnalysis.Builder[T86BasicBlock](cfg, forward = false)
      with FixpointComputation.Naive {

    import T86BasicBlockLivenessAnalysis._

    type NodeState = Set[Temp] // set of live-in variables for a basic block

    override def nodeStateLattice: Lattice[NodeState] = Lattice.powersetLat(???)

    override def cfgNodes: Seq[T86BasicBlock] = cfg.nodes

    private val bbDefUse: Map[CfgNode, DefUse] = cfg.nodes.map(bb => bb -> getBasicBlockDefUse(bb)).toMap

    /** Returns live-in variables for this basic block. */
    override def transfer(bb: T86BasicBlock, liveOutVars: Set[Temp]): Set[Temp] = {
      val DefUse(defines, uses) = bbDefUse(bb)
      (liveOutVars -- defines) ++ uses
    }

    def result(): Result = Result(fixpoint(), join)
  }

  object T86BasicBlockLivenessAnalysis {
    type NodeState = Set[Temp]
    type CfgState = Map[T86BasicBlock, NodeState]

    case class Result(cfgState: CfgState, join: (T86BasicBlock, CfgState) => NodeState) {
      def getLiveOut(bb: T86BasicBlock): Set[Temp] = join(bb, cfgState)

      def getLiveIn(bb: T86BasicBlock): Set[Temp] = cfgState(bb)
    }
  }

  trait InterferenceGraph extends Graph[Temp] {
    def edges: Seq[(Temp, Temp)]
  }

  object InterferenceGraph {
    class Builder {
      val adjMap: mutable.Map[Temp, mutable.Set[Temp]] = mutable.Map.empty

      val edgeSet: mutable.Set[(Temp, Temp)] = mutable.Set.empty

      def addEdge(u: Temp, v: Temp): Unit = {
        if (u != v) {
          adjMap.getOrElseUpdate(u, mutable.Set.empty).addOne(v)
          adjMap.getOrElseUpdate(v, mutable.Set.empty).addOne(u)
          edgeSet.addOne((u, v))
          edgeSet.addOne((v, u))
        }
      }

      def result(): InterferenceGraph = new InterferenceGraph {

        override val edges: Seq[(Temp, Temp)] = edgeSet.toSeq

        override val nodes: Seq[Temp] = adjMap.keySet.toSeq

        override def getSucc(node: Temp): Seq[Temp] = adjMap(node).toSeq

        override def getPred(node: Temp): Seq[Temp] = adjMap(node).toSeq

        override def getDegree(node: Temp): Int = adjMap(node).size

        override def getInDegree(node: Temp): Int = throw new UnsupportedOperationException()

        override def getOutDegree(node: Temp): Int = throw new UnsupportedOperationException()
      }
    }

    def newBuilder: Builder = new Builder
  }
}