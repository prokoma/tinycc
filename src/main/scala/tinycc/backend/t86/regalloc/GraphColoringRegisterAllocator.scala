package tinycc.backend.t86.regalloc

import tinycc.backend.t86.T86Opcode.MOV
import tinycc.backend.t86._
import tinycc.common._
import tinycc.common.analysis.LoopAnalysis
import tinycc.common.analysis.dataflow.{DataflowAnalysis, FixpointComputation, Lattice}
import tinycc.util.{DSU, Logging}

import scala.annotation.tailrec
import scala.collection.mutable

class GraphColoringRegisterAllocator(_machineRegCount: Int, _machineFRegCount: Int) extends T86RegisterAllocator {

  private val regRegisterAllocator = new GenericGraphColoringRegisterAllocator[Operand.Reg] with T86RegRegisterAllocator {
    override def machineRegCount: Int = _machineRegCount

    override def rewriteFunWithSpilledNodes(fun: T86Fun, spilledNodes: Set[Operand.Reg]): Set[Operand.Reg] = {
      val allocMap = spilledNodes.map(node => (node -> fun.freshLocal(regSize))).toMap // maps spilled temporaries to memory locations
      var newRegs = Set.empty[Operand.Reg]

      fun.basicBlocks.foreach(bb => {
        val newBody = IndexedSeq.newBuilder[T86ListingElement]
        bb.body.foreach({
          case insn: T86Insn =>
            val DefUse(defines, uses) = getInsnDefUse(insn)
            var regMap = Map.empty[Operand.Reg, Operand.Reg]

            // load used spilled registers from memory
            uses.intersect(spilledNodes).foreach(reg => {
              val newReg = fun.freshReg()
              newRegs += newReg
              log(s"spilling $reg into $newReg")
              regMap += (reg -> newReg)
              newBody += T86Insn(MOV, newReg, allocMap(reg))
            })

            newBody += remapRegistersInInsn(insn, regMap.withDefault(reg => reg))

            // store defined spilled registers into memory
            defines.intersect(spilledNodes).foreach(reg => {
              newBody += T86Insn(MOV, allocMap(reg), regMap.getOrElse(reg, reg))
            })

          case elem => newBody += elem
        })
        bb.body = newBody.result()
      })
      newRegs
    }
  }

  private val fregRegisterAllocator = new GenericGraphColoringRegisterAllocator[Operand.FReg] with T86FRegRegisterAllocator {
    override def machineRegCount: Int = _machineFRegCount

    override def rewriteFunWithSpilledNodes(fun: T86Fun, spilledNodes: Set[Operand.FReg]): Set[Operand.FReg] = {
      val allocMap = spilledNodes.map(node => (node -> fun.freshLocal(regSize))).toMap // maps spilled temporaries to memory locations
      var newRegs = Set.empty[Operand.FReg]

      fun.basicBlocks.foreach(bb => {
        val newBody = IndexedSeq.newBuilder[T86ListingElement]
        bb.body.foreach({
          case insn: T86Insn =>
            val DefUse(defines, uses) = getInsnDefUse(insn)
            var regMap = Map.empty[Operand.FReg, Operand.FReg]

            // load used spilled registers from memory
            uses.intersect(spilledNodes).foreach(freg => {
              // cannot load to freg directly from MemRegImm, need to go through normal reg
              val tmpReg = fun.freshReg()
              newBody += T86Insn(MOV, tmpReg, allocMap(freg))
              val newFreg = fun.freshFReg()
              newRegs += newFreg
              log(s"spilling $freg into $newFreg")
              regMap += (freg -> newFreg)
              newBody += T86Insn(MOV, newFreg, tmpReg)
            })

            newBody += remapRegistersInInsn(insn, regMap.withDefault(reg => reg))

            defines.intersect(spilledNodes).foreach(freg => {
              // store defined spilled registers into memory
              newBody += T86Insn(MOV, allocMap(freg), regMap.getOrElse(freg, freg))
            })

          case elem => newBody += elem
        })
        bb.body = newBody.result()
      })
      newRegs
    }
  }

  override def transformProgram(program: T86Program): Unit = {
    program.funs.foreach(transformFun)
  }

  def transformFun(fun: T86Fun): Unit = {
    // handle float regs first, because to spill them we need regular regs because of MOV operand limitations
    log(s"allocating FRegs for ${fun.name}")
    fregRegisterAllocator.transformFun(fun)
    log(s"allocating Regs for ${fun.name}")
    regRegisterAllocator.transformFun(fun)
  }
}

trait GenericGraphColoringRegisterAllocator[T <: Operand] extends T86GenericRegisterAllocator[T] with Logging {

  // backwards must dataflow analysis
  class LivenessAnalysis(cfg: Cfg[T86BasicBlock])
    extends DataflowAnalysis.Builder[T86BasicBlock](cfg, forward = false)
      with FixpointComputation.Naive {

    import LivenessAnalysis._

    type NodeState = Set[T] // set of live-in variables for a basic block

    override def nodeStateLattice: Lattice[NodeState] = Lattice.lazyPowersetLattice(throw new UnsupportedOperationException())

    override def cfgNodes: Seq[T86BasicBlock] = cfg.nodes

    private val bbDefUse: Map[Node, DefUse] = cfg.nodes.map(bb => bb -> getBasicBlockDefUse(bb)).toMap

    /** Returns live-in variables for this basic block. */
    override def transfer(bb: T86BasicBlock, liveOutVars: Set[T]): Set[T] = {
      val DefUse(defines, uses) = bbDefUse(bb)
      (liveOutVars -- defines) ++ uses
    }

    def result(): Result = Result(fixpoint(), join)
  }

  object LivenessAnalysis {
    type NodeState = Set[T]
    type CfgState = Map[T86BasicBlock, NodeState]

    case class Result(cfgState: CfgState, join: (T86BasicBlock, CfgState) => NodeState) {
      def getLiveOut(bb: T86BasicBlock): Set[T] = join(bb, cfgState)

      def getLiveIn(bb: T86BasicBlock): Set[T] = cfgState(bb)
    }
  }

  trait InterferenceGraph {
    def nodes: Set[T]

    def adjList: Map[T, Set[T]]

    def moveInsns: Set[T86InsnRef]

    def relatedMoveList: Map[T, Set[T86InsnRef]]

    def getDefUseCountInLoop(node: T): Int

    def getDefUseCountOutsideLoop(node: T): Int

    def getLiveRangeSize(node: T): Int
  }

  object InterferenceGraph {
    def apply(cfg: Cfg[T86BasicBlock], blocksInLoop: Set[T86BasicBlock]): InterferenceGraph =
      apply(cfg, new LivenessAnalysis(cfg).result(), blocksInLoop)

    def apply(cfg: Cfg[T86BasicBlock], livenessResult: LivenessAnalysis.Result, blocksInLoop: Set[T86BasicBlock]): InterferenceGraph = {
      val _relatedMoveList = mutable.Map.empty[T, Set[T86InsnRef]].withDefaultValue(Set.empty)
      var _moveInsns = Set.empty[T86InsnRef]
      val _adjList = mutable.Map.empty[T, Set[T]].withDefaultValue(Set.empty)
      var _nodes = machineRegs

      def addEdge(u: T, v: T): Unit = {
        if (u != v) {
          if (!machineRegs.contains(u)) _adjList(u) += v
          if (!machineRegs.contains(v)) _adjList(v) += u
        }
      }

      // stats used for spill heuristic
      val _defUseCountsInLoop = mutable.Map.empty[T, Int].withDefaultValue(0)
      val _defUseCountsOutsideLoop = mutable.Map.empty[T, Int].withDefaultValue(0)
      val _liveRangeSizes = mutable.Map.empty[T, Int].withDefaultValue(0)

      cfg.nodes.foreach(bb => {
        var live = livenessResult.getLiveOut(bb)
        val inLoop = blocksInLoop.contains(bb)

        for ((insn, insnRef) <- bb.insnsWithRefs.reverse) {
          val DefUse(defines, uses) = getInsnDefUse(insn)
          _nodes ++= defines ++ uses

          // if the same register is defined and also used, count it as 2 distinct registers
          (defines.toSeq ++ uses.toSeq).foreach(node => {
            if (inLoop)
              _defUseCountsInLoop(node) += 1
            else
              _defUseCountsOutsideLoop(node) += 1
          })

          if (isRegRegMove(insn)) {
            // We just copy one register to another of the same type
            // The live ranges don't overlap, because if we color the temps with the same color, the move is redundant
            for (temp <- defines ++ uses) {
              _relatedMoveList(temp) += insnRef
            }
            _moveInsns += insnRef

            for (l <- live -- uses; d <- defines)
              addEdge(d, l)
          } else {
            for (l <- live; d <- defines)
              addEdge(d, l)
          }

          live = (live -- defines) ++ uses

          live.foreach(l => _liveRangeSizes(l) = _liveRangeSizes(l) + 1)
        }
      })

      // make sure _adjList and _relatedMoves is defined for all nodes
      for (temp <- _nodes) {
        if (!machineRegs.contains(temp) && !_adjList.contains(temp))
          _adjList(temp) = Set.empty
        if (!_relatedMoveList.contains(temp))
          _relatedMoveList(temp) = Set.empty
      }

      new InterferenceGraph {
        override def nodes: Set[T] = _nodes

        override val adjList: Map[T, Set[T]] = _adjList.toMap

        override def moveInsns: Set[T86InsnRef] = _moveInsns

        override val relatedMoveList: Map[T, Set[T86InsnRef]] = _relatedMoveList.toMap

        override def getDefUseCountInLoop(node: T): Int = _defUseCountsInLoop(node)

        override def getDefUseCountOutsideLoop(node: T): Int = _defUseCountsOutsideLoop(node)

        override def getLiveRangeSize(node: T): Int = _liveRangeSizes(node)
      }
    }
  }

  trait InterferenceGraphColoring {
    def colorMap: Map[T, T]

    def spilledNodes: Set[T]
  }

  object InterferenceGraphColoring {
    /** A set of distinct worklists. Ensures that every element is in exactly one of the worklists.
     * Moving an element into a list prepends it at the beginning, so every worklist behaves like a stack. */
    abstract class WorklistSet[A, B] {
      protected def curWorklist: mutable.Map[A, B]

      def apply(worklist: B): List[A]

      protected def update(worklist: B, nodes: List[A]): Unit

      @inline
      def isNonEmpty(worklist: B): Boolean = this (worklist).nonEmpty

      @inline
      def isInList(node: A, worklist: B): Boolean = curWorklist(node) == worklist

      @inline
      def isInListUnchecked(node: A, worklist: B): Boolean = curWorklist.getOrElse(node, null) == worklist

      def moveToList(node: A, newWorklist: B): Unit = {
        val oldWorklist = curWorklist(node)
        if (oldWorklist == newWorklist)
          return

        /** Returns a copy of [[list]] with the first occurrence of [[el]] removed. */
        @tailrec
        def removeFirstOcc(list: List[A], el: A, acc: List[A] = Nil): List[A] = list match {
          case Nil => acc
          case head :: tail if head == el => acc ++ tail
          case head :: tail => removeFirstOcc(tail, el, head :: acc)
        }

        this (oldWorklist) = removeFirstOcc(this (oldWorklist), node)
        this (newWorklist) ::= node
        curWorklist(node) = newWorklist
      }
    }

    sealed trait NodeWorklist

    object NodeWorklist {
      case object Initial extends NodeWorklist

      case object Simplify extends NodeWorklist

      case object Freeze extends NodeWorklist

      case object Spill extends NodeWorklist

      case object SelectStack extends NodeWorklist // removed from graph

      case object SpilledNodes extends NodeWorklist // removed from graph

      case object CoalescedNodes extends NodeWorklist // removed from graph
    }

    class NodeWorklistSet(_initial: Iterable[T]) extends WorklistSet[T, NodeWorklist] {

      import NodeWorklist._

      require(_initial.toSet.size == _initial.size, s"NodeWorklistSet can't contain duplicates")

      private var initial: List[T] = _initial.toList
      private var simplify = List.empty[T]
      private var freeze = List.empty[T]
      private var spill = List.empty[T]
      private var selectStack = List.empty[T]
      private var spilledNodes = List.empty[T]
      private var coalescedNodes = List.empty[T]

      override protected val curWorklist: mutable.Map[T, NodeWorklist] = mutable.Map.from(initial.map(node => node -> Initial))

      override def apply(worklist: NodeWorklist): List[T] = worklist match {
        case Initial => initial
        case Simplify => simplify
        case Freeze => freeze
        case Spill => spill
        case SelectStack => selectStack
        case SpilledNodes => spilledNodes
        case CoalescedNodes => coalescedNodes
      }

      override protected def update(worklist: NodeWorklist, nodes: List[T]): Unit = worklist match {
        case Initial => initial = nodes
        case Simplify => simplify = nodes
        case Freeze => freeze = nodes
        case Spill => spill = nodes
        case SelectStack => selectStack = nodes
        case SpilledNodes => spilledNodes = nodes
        case CoalescedNodes => coalescedNodes = nodes
      }
    }

    sealed trait MoveWorklist

    object MoveWorklist {
      case object WorklistMoves extends MoveWorklist

      case object DisabledMoves extends MoveWorklist

      case object CoalescedMoves extends MoveWorklist // removed from graph

      case object ConstrainedMoves extends MoveWorklist // removed from graph

      case object FrozenMoves extends MoveWorklist // removed from graph
    }

    class MoveWorklistSet(_worklistMoves: Iterable[T86InsnRef]) extends WorklistSet[T86InsnRef, MoveWorklist] {

      import MoveWorklist._

      require(_worklistMoves.toSet.size == _worklistMoves.size, s"NodeWorklistSet can't contain duplicates")

      private var worklistMoves: List[T86InsnRef] = _worklistMoves.toList
      private var disabledMoves = List.empty[T86InsnRef]
      private var coalescedMoves = List.empty[T86InsnRef]
      private var constrainedMoves = List.empty[T86InsnRef]
      private var frozenMoves = List.empty[T86InsnRef]

      override protected val curWorklist: mutable.Map[T86InsnRef, MoveWorklist] = mutable.Map.from(worklistMoves.map(node => node -> WorklistMoves))

      override def apply(worklist: MoveWorklist): List[T86InsnRef] = worklist match {
        case WorklistMoves => worklistMoves
        case DisabledMoves => disabledMoves
        case CoalescedMoves => coalescedMoves
        case ConstrainedMoves => constrainedMoves
        case FrozenMoves => frozenMoves
      }

      override protected def update(worklist: MoveWorklist, moves: List[T86InsnRef]): Unit = worklist match {
        case WorklistMoves => worklistMoves = moves
        case DisabledMoves => disabledMoves = moves
        case CoalescedMoves => coalescedMoves = moves
        case ConstrainedMoves => constrainedMoves = moves
        case FrozenMoves => frozenMoves = moves
      }
    }

    def apply(interf: InterferenceGraph, getSpillCost: T => Double): InterferenceGraphColoring = {
      import MoveWorklist._
      import NodeWorklist._

      // === VARIABLES ===

      // number of colors
      val K = machineRegs.size

      // mutually exclusive sets of nodes
      val nodeLists = new NodeWorklistSet(interf.nodes -- machineRegs)
      // mutually exclusive sets of MOV Rx, Ry
      val moveLists = new MoveWorklistSet(interf.moveInsns)

      // aliases for coalesced nodes
      val _aliases = new DSU[T]

      /** Adjacency list containing edges currently in the graph. Defined for nodes in [[Initial]], [[Simplify]], [[Freeze]] and [[Spill]]. */
      val _adjList = mutable.Map.from(interf.adjList)
      /** Adjacency list containing edges that are or sometime have been in the graph. */
      val _adjListWithRemovedEdges = mutable.Map.from(interf.adjList)

      // current list of moves related to a node (updated after coalescing)
      val _relatedMoveList = mutable.Map.from(interf.relatedMoveList)
      // current spill cost for a node (updated after coalescing)
      val _spillCosts = mutable.Map.empty[T, Double].withDefault(getSpillCost)

      // === HELPER FUNCTIONS ===

      def isMoveRelated(node: T): Boolean = _relatedMoveList(node).nonEmpty

      def degree(node: T): Int = _adjList(node).size

      /** Moves [[DisabledMoves]] moves related to [[nodes]] to [[WorklistMoves]] */
      def enableRelatedMoves(nodes: Set[T]): Unit =
        moveLists(DisabledMoves).foreach(insn => {
          val (dest, src) = getMoveDestSrc(insn())
          if (nodes.contains(dest) || nodes.contains(src))
            moveLists.moveToList(insn, WorklistMoves)
        })

      /** Adds edge between [[u]] and [[v]] to adjacency lists */
      def addEdge(u: T, v: T): Unit = {
        if (u != v) {
          if (!machineRegs.contains(u)) {
            _adjList(u) += v
            _adjListWithRemovedEdges(u) += v
          }
          if (!machineRegs.contains(v)) {
            _adjList(v) += u
            _adjListWithRemovedEdges(v) += u
          }
        }
      }

      /** Removes edge node -> adj from the graph, called before removing [[node]] from the graph. Decrements degree of [[adj]]. */
      def removeEdge(node: T, adj: T): Unit = {
        if (machineRegs.contains(adj))
          return

        _adjList(adj) -= node // decrement degree for nodes still in graph
        // we are removing node from the graph, so we don't need to remove the opposite direction

        if (degree(adj) == K - 1) { // node has no longer a significant degree
          enableRelatedMoves(_adjList(adj) + adj)
          if (isMoveRelated(adj))
            nodeLists.moveToList(adj, Freeze)
          else
            nodeLists.moveToList(adj, Simplify)
        }
      }

      /** Freeze moves related to [[node]]. */
      def freezeMoves(node: T): Unit = {
        val u = _aliases.find(node)
        _relatedMoveList(node).foreach(insn => {
          val v = {
            val (dest, src) = getMoveDestSrc(insn())
            val rDest = _aliases.find(dest)
            if (rDest == u)
              _aliases.find(src)
            else
              rDest
          }

          _relatedMoveList(u) -= insn
          _relatedMoveList(v) -= insn
          moveLists.moveToList(insn, FrozenMoves)

          // if a node was move related with insignificant degree and now isn't, move it to [[Simplify]]
          if (nodeLists.isInListUnchecked(v, Freeze) && !isMoveRelated(v))
            nodeLists.moveToList(v, Simplify)
        })
      }

      def tryMoveToSimplify(node: T): Unit = {
        if (!machineRegs.contains(node) && !isMoveRelated(node) && degree(node) < K) {
          nodeLists.moveToList(node, Simplify)
        }
      }

      /** Check if we don't harm colorability of the graph by combining [[u]] and [[v]] into single node. */
      def shouldCoalesce(u: T, v: T): Boolean = {
        if (machineRegs.contains(u)) { // u is machine reg, but v is not machine reg
          _adjList(v).forall(adj => { // George
            machineRegs.contains(adj) || {
              val tmp = _adjList(adj)
              tmp.size < K || tmp.contains(u)
            }
          })
        } else { // both are not machine regs
          // Briggs
          (_adjList(u) ++ _adjList(v)).count(adj => machineRegs.contains(adj) || degree(adj) >= K) < K // after coalesce, number of nodes with significant degree is < K
          // machineRegs.contains(adj) implies that degree(adj) >= K (K-1 remaining machineRegs + u + v)
        }
      }

      /** Merges [[u]] and [[v]] into single node [[u]]. The nodes can't interfere. */
      def combine(u: T, v: T): Unit = {
        // if we combine precolored and non-precolored nodes, we need to keep the precolored one (u) in the graph
        // remove v from the graph
        nodeLists.moveToList(v, CoalescedNodes) // mark node as coalesced
        _aliases.union(u, v)

        // copy edges
        _spillCosts(u) += _spillCosts(v)
        _relatedMoveList(u) ++= _relatedMoveList(v)
        enableRelatedMoves(Set(v))
        _adjList(v).foreach(adj => {
          addEdge(u, adj)
          removeEdge(v, adj)
        })
        if (nodeLists.isInListUnchecked(u, Freeze) && degree(u) >= K)
          nodeLists.moveToList(u, Spill)
      }

      // === MAIN LOOP ===

      // distribute nodes from Initial into Spill, Freeze and Simplify
      nodeLists(Initial).foreach(node => {
        if (degree(node) >= K)
          nodeLists.moveToList(node, Spill)
        else if (isMoveRelated(node))
          nodeLists.moveToList(node, Freeze)
        else
          nodeLists.moveToList(node, Simplify)
      })

      while (nodeLists.isNonEmpty(Simplify) || moveLists.isNonEmpty(WorklistMoves) || nodeLists.isNonEmpty(Spill) || nodeLists.isNonEmpty(Freeze)) {
        if (nodeLists.isNonEmpty(Simplify)) {
          val node = nodeLists(Simplify).head

          log(s"simplify $node")
          nodeLists.moveToList(node, SelectStack) // mark node for coloring
          for (adj <- _adjList(node))
            removeEdge(node, adj)

        } else if (moveLists.isNonEmpty(WorklistMoves)) {
          val insn = moveLists(WorklistMoves).head

          val (u, v) = {
            val (dest, src) = getMoveDestSrc(insn())
            val rDest = _aliases.find(dest)
            val rSrc = _aliases.find(src)
            if (machineRegs.contains(rDest))
              (rDest, rSrc)
            else (rSrc, rDest)
          }

          if (u == v) {
            moveLists.moveToList(insn, CoalescedMoves)
            _relatedMoveList(u) -= insn
            _relatedMoveList(v) -= insn
            tryMoveToSimplify(u)
          } else if (machineRegs.contains(v) || _adjListWithRemovedEdges(v).contains(u)) {
            // registers are interfering (we don't store edges between machineRegs in _adjListWithRemoveEdges, so check that specifically)
            log(s"move $insn ($u, $v) constrained")
            moveLists.moveToList(insn, ConstrainedMoves)
            _relatedMoveList(u) -= insn
            _relatedMoveList(v) -= insn
            tryMoveToSimplify(u)
            tryMoveToSimplify(v)
          } else {
            log(s"move $insn ($u, $v) candidate for coalescing")
            if (shouldCoalesce(u, v)) {
              log(s"combine $insn ($u, $v)")
              moveLists.moveToList(insn, CoalescedMoves)
              _relatedMoveList(u) -= insn
              _relatedMoveList(v) -= insn
              combine(u, v) // combines u and v to uv
              tryMoveToSimplify(u)
            } else {
              log(s"disable $insn ($u, $v)")
              moveLists.moveToList(insn, DisabledMoves) // mark move as pending, can be enabled later by enableRelatedMoves
            }
          }
          assert(!moveLists.isInList(insn, WorklistMoves))

        } else if (nodeLists.isNonEmpty(Freeze)) {
          val node = nodeLists(Freeze).head
          nodeLists.moveToList(node, Simplify)
          freezeMoves(node)

        } else if (nodeLists.isNonEmpty(Spill)) {
          val node = nodeLists(Spill).minBy(_spillCosts)
          log(s"candidate for spill: $node")
          nodeLists.moveToList(node, Simplify)
          freezeMoves(node)

        }
      }

      // initialize _colorMap with precolored registers
      val _colorMap = mutable.Map.from(machineRegs.map(r => (r -> r)))

      for (node <- nodeLists(SelectStack)) {
        var okColors = machineRegs
        for (adj <- _adjListWithRemovedEdges(node)) {
          _colorMap.get(_aliases.find(adj)).foreach(color => okColors -= color)
        }
        if (okColors.isEmpty) {
          nodeLists.moveToList(node, SpilledNodes)
        } else {
          val newColor = okColors.head
          _colorMap(node) = newColor
        }
      }
      for (node <- nodeLists(CoalescedNodes))
        _colorMap(node) = _colorMap(_aliases.find(node))

      new InterferenceGraphColoring {
        override def colorMap: Map[T, T] = _colorMap.toMap

        override def spilledNodes: Set[T] = nodeLists(SpilledNodes).toSet
      }
    }
  }

  override def name: String = "GenericGraphColoringRegisterAllocator"

  def remapCalleeSaveRegs(fun: T86Fun): Unit = {
    val _calleeSaveRegs = calleeSaveRegs.toSeq
    val _regMap = mutable.Map.empty[T, T]

    val backupCode = _calleeSaveRegs.map(reg => {
      val newReg = freshReg(fun)
      _regMap(reg) = newReg
      T86Insn(MOV, newReg, reg)
    })
    val restoreCode = _calleeSaveRegs.map(reg => {
      T86Insn(MOV, reg, _regMap(reg))
    })

    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.flatMap({
        case T86SpecialLabel.FunPrologueMarker => T86SpecialLabel.FunPrologueMarker +: backupCode
        case T86SpecialLabel.FunEpilogueMarker => restoreCode :+ T86SpecialLabel.FunEpilogueMarker

        case elem => Seq(elem)
      })
    })
  }

  /** Removes redundant MOV Rx, Rx instructions */
  def removeRedundantMoves(fun: T86Fun): Unit = {
    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.filter({
        case insn: T86Insn if isRegRegMove(insn) =>
          val (dest, src) = getMoveDestSrc(insn)
          if (dest == src)
            log(s"removed redundant $insn")
          dest != src
        case _ => true
      })
    })
  }

  /**
   * @return newly created registers
   */
  def rewriteFunWithSpilledNodes(fun: T86Fun, spilledNodes: Set[T]): Set[T]

  def transformFun(fun: T86Fun): Unit = {
    // insert code to backup and restore all callee save registers
    remapCalleeSaveRegs(fun)

    val cfg = T86BasicBlockCfg(fun)
    val blocksInLoop = new LoopAnalysis(cfg).result()

    var noSpillNodes = Set.empty[T]

    var didSpill = false
    do {
      didSpill = false

      val interferenceGraph = InterferenceGraph(cfg, blocksInLoop)
      log("interfering nodes: \n" + interferenceGraph.adjList.map({ case (u, set) => s"$u -> $set" }).mkString("\n"))

      val coloring = InterferenceGraphColoring(interferenceGraph, node => {
        if (machineRegs.contains(node) || noSpillNodes.contains(node))
          Double.PositiveInfinity
        else
          (interferenceGraph.getDefUseCountInLoop(node) * 10 + interferenceGraph.getDefUseCountOutsideLoop(node)).toDouble / interferenceGraph.adjList(node).size
      })
      log("registers to spill: " + coloring.spilledNodes)

      if (coloring.spilledNodes.nonEmpty) {
        didSpill = true
        // don't spill already spilled registers in the next iteration
        noSpillNodes ++= rewriteFunWithSpilledNodes(fun, coloring.spilledNodes) ++ coloring.spilledNodes
      } else
        remapRegistersInFun(fun, coloring.colorMap.withDefault(reg => reg))
    } while (didSpill)

    removeRedundantMoves(fun)

    log(new T86AsmPrinter().printToString(fun.flatten))
  }
}