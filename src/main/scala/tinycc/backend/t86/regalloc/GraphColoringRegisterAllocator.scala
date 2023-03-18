package tinycc.backend.t86.regalloc

import tinycc.backend.t86.T86Opcode.MOV
import tinycc.backend.t86._
import tinycc.common._
import tinycc.common.analysis.LoopAnalysis
import tinycc.util.{DSU, Logging}

import scala.collection.mutable

class GraphColoringRegisterAllocator extends T86RegisterAllocator {

  private val regRegisterAllocator = new GenericGraphColoringRegisterAllocator[Operand.Reg] with T86RegRegisterAllocator {
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

    def regRegMoveInsns: Set[T86InsnRef]

    def regRegMoveList: Map[T, Set[T86InsnRef]]

    def getDefUseCountInLoop(node: T): Int

    def getDefUseCountOutsideLoop(node: T): Int

    def getLiveRangeSize(node: T): Int
  }

  object InterferenceGraph {
    def apply(cfg: Cfg[T86BasicBlock], blocksInLoop: Set[T86BasicBlock]): InterferenceGraph =
      apply(cfg, new LivenessAnalysis(cfg).result(), blocksInLoop)

    def apply(cfg: Cfg[T86BasicBlock], livenessResult: LivenessAnalysis.Result, blocksInLoop: Set[T86BasicBlock]): InterferenceGraph = {
      val _regRegMoveList = mutable.Map.empty[T, Set[T86InsnRef]].withDefaultValue(Set.empty)
      var _regRegMoveInsns = Set.empty[T86InsnRef]
      val _adjList = mutable.Map.empty[T, Set[T]].withDefaultValue(Set.empty)
      var _nodes = machineRegs

      def addEdge(u: T, v: T): Unit = {
        if (u != v) {
          if (!machineRegs.contains(u)) _adjList(u) += v
          if (!machineRegs.contains(v)) _adjList(v) += u
        }
      }

      val _defUseCountsInLoop = mutable.Map.empty[T, Int].withDefaultValue(0)
      val _defUseCountsOutsideLoop = mutable.Map.empty[T, Int].withDefaultValue(0)

      val _liveRangeSizes = mutable.Map.empty[T, Int].withDefaultValue(0)

      cfg.nodes.foreach(bb => {
        var live = livenessResult.getLiveOut(bb)
        val inLoop = blocksInLoop.contains(bb)

        for ((insn, insnRef) <- bb.insnsWithRefs.reverse) {
          val DefUse(defines, uses) = getInsnDefUse(insn)
          _nodes ++= defines ++ uses

          (defines ++ uses).foreach(node => {
            if(inLoop)
              _defUseCountsInLoop(node) += 1
            else
              _defUseCountsOutsideLoop(node) += 1
          })

          if (isRegRegMove(insn)) {
            // We just copy one register to another of the same type
            // The live ranges don't overlap, because if we color the temps with the same color, the move is redundant
            for (temp <- defines ++ uses) {
              _regRegMoveList(temp) += insnRef
            }
            _regRegMoveInsns += insnRef

            for (l <- live -- uses; d <- defines)
              addEdge(d, l)
          } else {
            for (l <- live; d <- defines)
              addEdge(d, l)
          }

          live = (live -- defines) ++ uses

//          for (l <- live)
//            _liveRangeSizes(l) = _liveRangeSizes(l) + 1
        }
      })

      for (temp <- _nodes) {
        if (!machineRegs.contains(temp) && !_adjList.contains(temp))
          _adjList(temp) = Set.empty
        if (!_regRegMoveList.contains(temp))
          _regRegMoveList(temp) = Set.empty
      }

      new InterferenceGraph {
        override def nodes: Set[T] = _nodes

        override val adjList: Map[T, Set[T]] = _adjList.toMap

        override def regRegMoveInsns: Set[T86InsnRef] = _regRegMoveInsns

        override val regRegMoveList: Map[T, Set[T86InsnRef]] = _regRegMoveList.toMap

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
//    sealed trait NodeWorklist
//
//    object NodeWorklist {
//      case object Initial extends NodeWorklist
//      case object Simplify extends NodeWorklist
//      case object Freeze extends NodeWorklist
//      case object SelectStack extends NodeWorklist
//      case object SpilledNodes extends NodeWorklist
//      case object CoalescedNodes extends NodeWorklist
//    }
//
//    class NodeWorklistSet(var initial: Set[T]) {
//      import NodeWorklist._
//
//      val curWorklist: mutable.Map[T, NodeWorklist] = mutable.Map.from(initial.map(node => node -> Initial))
//
//      var simplify = List.empty[T]
//      var freeze = List.empty[T]
//      var spill = List.empty[T]
//      var selectStack = List.empty[T]
//      var spilledNodes = List.empty[T]
//      var coalescedNodes = List.empty[T]
//
//      private def isInWorklistSlow(node: T, worklist: NodeWorklist): Boolean = worklist match {
//        case Initial => initial.contains(node)
//        case Simplify => simplify.contains(node)
//        case Freeze => freeze.contains(node)
//        case SelectStack => selectStack.contains(node)
//        case SpilledNodes => spilledNodes.contains(node)
//        case CoalescedNodes => coalescedNodes.contains(node)
//      }
//
//      @inline
//      def isInWorklist(node: T, worklist: NodeWorklist): Boolean = curWorklist(node) == worklist
//
//      def addToWorklist(node: T, worklist: NodeWorklist): Unit = {
//        if(curWorklist(node) == worklist)
//          return
//
//        assert(!isInWorklistSlow(node, curWorklist(node)), s"$node was not removed from ${curWorklist(node)}")
//        curWorklist(node) = worklist
//
//        worklist match {
//          case Initial => initial ::= node
//          case Simplify => simplify ::= node
//          case Freeze => freeze ::= node
//          case SelectStack => selectStack ::= node
//          case SpilledNodes => spilledNodes ::= node
//          case CoalescedNodes => coalescedNodes ::= node
//        }
//      }
//
//      def popWorklist(worklist: NodeWorklist): Unit = worklist match {
//        case Initial =>
//        case Simplify =>
//        case Freeze =>
//        case SelectStack =>
//        case SpilledNodes =>
//        case CoalescedNodes =>
//      }
//    }

    def apply(interf: InterferenceGraph, getSpillCost: T => Double): InterferenceGraphColoring = {
      val K = machineRegs.size

      // mutually exclusive sets of nodes
      var _initial = interf.nodes -- machineRegs // set of nodes excluding precolored nodes
      var _simplifyWorklist = Set.empty[T]
      var _freezeWorklist = Set.empty[T]
      var _spillWorklist = Set.empty[T]
      var _selectStack = List.empty[T] // removed from graph
      var _spilledNodes = Set.empty[T] // removed from graph
      var _coalescedNodes = Set.empty[T] // removed from graph

      // mutually exclusive sets of MOV Rx, Ry
      var _worklistMoves = interf.regRegMoveInsns
      var _disabledMoves = Set.empty[T86InsnRef]
      var _coalescedMoves = List.empty[T86InsnRef] // removed from graph
      var _constrainedMoves = List.empty[T86InsnRef] // removed from graph
      var _frozenMoves = Set.empty[T86InsnRef] // removed from graph

      val _aliases = new DSU[T] // aliases for coalesced nodes

      /** Adjacency list containing edges currently in the graph. Defined for nodes in (_initial ++ _simplifyWorklist ++ _freezeWorklist ++ _spillWorklist). */
      val _adjList = mutable.Map.from(interf.adjList)
      val _regRegMoveList = mutable.Map.from(interf.regRegMoveList)

      /** Adjacency list containing edges that are or sometime have been in the graph. */
      val _adjList2 = mutable.Map.from(interf.adjList)

      val _spillCosts = mutable.Map.empty[T, Double].withDefault(getSpillCost)

      def isMoveRelated(node: T): Boolean = _regRegMoveList(node).nonEmpty

      def degree(node: T): Int = _adjList(node).size

      // sort nodes from _initial into worklists
      for (node <- _initial) {
        if (degree(node) >= K)
          _spillWorklist += node
        else if (isMoveRelated(node))
          _freezeWorklist += node
        else
          _simplifyWorklist += node
      }
      _initial = Set.empty

      // move moves related to these nodes from _disabledMoves to _worklistMoves
      def enableRelatedMoves(nodes: Set[T]): Unit = {
        _disabledMoves = _disabledMoves.filter(insn => {
          val BinaryT86Insn(_, dest: T @unchecked, src: T @unchecked) = insn()
          if (nodes.contains(dest) || nodes.contains(src)) {
            _worklistMoves += insn
            false
          } else true
        })
      }

      // removes edge u -> v from the adjacency list (we usually remove u from the graph)
      // decrements degree of adj
      def removeEdge(node: T, adj: T): Unit = {
        if(machineRegs.contains(adj))
          return

        _adjList(adj) -= node // decrement degree for nodes still in graph
        if (degree(adj) == K - 1) { // node has no longer a significant degree
          enableRelatedMoves(_adjList(adj) + adj)

          _spillWorklist -= adj
          if (isMoveRelated(adj))
            _freezeWorklist += adj
          else
            _simplifyWorklist += adj
        }
      }

      def freezeMoves(node: T): Unit = {
        val u = _aliases.find(node)
        _regRegMoveList(node).foreach(insn => {
          val v = {
            val BinaryT86Insn(_, dest: T @unchecked, src: T @unchecked) = insn()
            val rDest = _aliases.find(dest)
            if (rDest == u)
              _aliases.find(src)
            else
              rDest
          }

          _disabledMoves -= insn
          _regRegMoveList(u) -= insn
          _regRegMoveList(v) -= insn
          _frozenMoves += insn
          if (_freezeWorklist.contains(v) && !isMoveRelated(v)) {
            _freezeWorklist -= v
            _simplifyWorklist += v
          }
        })
      }

      while (_simplifyWorklist.nonEmpty || _worklistMoves.nonEmpty || _spillWorklist.nonEmpty || _freezeWorklist.nonEmpty) {
        if (_simplifyWorklist.nonEmpty) {
          val node = _simplifyWorklist.head
          _simplifyWorklist -= node

          log(s"simplify $node")
          _selectStack ::= node // mark node for coloring
          for (adj <- _adjList(node)) {
            removeEdge(node, adj)
          }
        } else if (_worklistMoves.nonEmpty) {
          val insn = _worklistMoves.head
          _worklistMoves -= insn

          def addWorkList(node: T): Unit = {
            if (!machineRegs.contains(node) && !isMoveRelated(node) && degree(node) < K) {
              _freezeWorklist -= node
              _simplifyWorklist += node
            }
          }

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

          def addEdge(u: T, v: T): Unit = {
            if (u != v) {
              if (!machineRegs.contains(u)) {
                _adjList(u) += v
                _adjList2(u) += v
              }
              if (!machineRegs.contains(v)) {
                _adjList(v) += u
                _adjList2(v) += u
              }
            }
          }

          def combine(u: T, v: T): Unit = {
            // if we combine precolored and non-precolored nodes, we need to keep the precolored one (u) in the graph
            // remove v from the graph
            _freezeWorklist -= v
            _spillWorklist -= v
            _coalescedNodes += v // mark node as coalesced
            _aliases.union(u, v)

            // copy edges
            _spillCosts(u) += _spillCosts(v)
            _regRegMoveList(u) ++= _regRegMoveList(v)
            enableRelatedMoves(Set(v))
            _adjList(v).foreach(adj => {
              addEdge(u, adj)
              removeEdge(v, adj)
            })
            if (_freezeWorklist.contains(u) && degree(u) >= K) {
              _freezeWorklist -= u
              _spillWorklist += u
            }
          }

          val (u, v) = {
            val BinaryT86Insn(_, dest: T @unchecked, src: T @unchecked) = insn()
            val rDest = _aliases.find(dest)
            val rSrc = _aliases.find(src)
            if (machineRegs.contains(rDest))
              (rDest, rSrc)
            else (rSrc, rDest)
          }

          if (u == v) {
            _coalescedMoves ::= insn
            _regRegMoveList(u) -= insn
            _regRegMoveList(v) -= insn
            addWorkList(u)
          } else if (machineRegs.contains(v) || _adjList2(v).contains(u)) {
            // registers are interfering (we don't store edges between machineRegs in _origAdjList)
            log(s"move $insn ($u, $v) constrained")
            _constrainedMoves ::= insn
            _regRegMoveList(u) -= insn
            _regRegMoveList(v) -= insn
            addWorkList(u)
            addWorkList(v)
          } else {
            log(s"move $insn ($u, $v) candidate for coalescing")
            if (shouldCoalesce(u, v)) {
              log(s"combine $insn ($u, $v)")
              _coalescedMoves ::= insn
              _regRegMoveList(u) -= insn
              _regRegMoveList(v) -= insn
              combine(u, v) // combines u and v to uv
              addWorkList(u)
            } else {
              log(s"disable $insn ($u, $v)")
              _disabledMoves += insn // mark move as pending, can be enabled later by enableRelatedMoves
            }
          }
        } else if (_freezeWorklist.nonEmpty) {
          val node = _freezeWorklist.head
          _freezeWorklist -= node
          _simplifyWorklist += node

          freezeMoves(node)
        } else if (_spillWorklist.nonEmpty) {
          val node = _spillWorklist.minBy(_spillCosts)
          _spillWorklist -= node

          log(s"candidate for spill: $node")
          _simplifyWorklist += node
          freezeMoves(node)
        }
      }

      val _colorMap = mutable.Map.from(machineRegs.map(r => (r -> r)))
      for (node <- _selectStack) {
        var okColors = machineRegs
        for (adj <- _adjList2(node)) {
          _colorMap.get(_aliases.find(adj)).foreach(color => okColors -= color)
        }
        if (okColors.isEmpty) {
          _spilledNodes += node
        } else {
          val newColor = okColors.head
          _colorMap(node) = newColor
        }
      }

      for (node <- _coalescedNodes) {
        _colorMap(node) = _colorMap(_aliases.find(node))
      }

      new InterferenceGraphColoring {
        override def colorMap: Map[T, T] = _colorMap.toMap

        override def spilledNodes: Set[T] = _spilledNodes
      }
    }
  }

  override def name: String = "GenericGraphColoringRegisterAllocator"

  def remapCalleeSaveRegs(fun: T86Fun): Unit = {
    val _calleeSaveRegs = calleeSaveRegs.toSeq
    val _regMap = mutable.Map.empty[T, T]

    val backupCode = _calleeSaveRegs.map(reg => {
      val newReg = reg match {
        case reg: Operand.Reg => fun.freshReg()
        case freg: Operand.FReg => fun.freshFReg()
      }
      _regMap(reg) = newReg.asInstanceOf[T]
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

  def removeRedundantMoves(cfg: Cfg[T86BasicBlock]): Unit = {
    val livenessResult = new LivenessAnalysis(cfg).result()

    cfg.nodes.foreach(bb => {
      val newBodyReversed = IndexedSeq.newBuilder[T86ListingElement]
      var live = livenessResult.getLiveOut(bb)

      bb.body.reverse.foreach({
        case insn: T86Insn =>
          val DefUse(defines, uses) = getInsnDefUse(insn)
          // live now contains registers that can be read after this instruction

          if (isRegRegMove(insn)) {
            val BinaryT86Insn(_, dest: T @unchecked, src: T @unchecked) = insn
            if (dest != src && live.contains(dest))
              newBodyReversed += insn
            else
              log(s"removed redundant $insn")
          } else {
            newBodyReversed += insn
          }

          live = (live -- defines) ++ uses

        case elem => newBodyReversed += elem
      })

      bb.body = newBodyReversed.result().reverse
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

//      log(new T86AsmPrinter().printToString(fun.flatten))
    } while (didSpill)

    removeRedundantMoves(cfg)
  }
}