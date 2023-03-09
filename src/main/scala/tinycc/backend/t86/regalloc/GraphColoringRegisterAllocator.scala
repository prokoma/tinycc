package tinycc.backend.t86.regalloc

import tinycc.backend.BackendException
import tinycc.backend.t86.T86Opcode.MOV
import tinycc.backend.t86._
import tinycc.common._

import scala.collection.mutable

class GraphColoringRegisterAllocator(program: T86Program) extends T86RegisterAllocator(program: T86Program) {

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
              Console.err.println(s"Spilling $reg into $newReg")
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

  override def result(): T86Program = {
    program.funs.foreach(processFun)
    program
  }

  def processFun(fun: T86Fun): Unit = {
    // handle float regs first, because to spill them we need regular regs because of MOV operand limitations
    Console.err.println("FReg")
    fregRegisterAllocator.processFun(fun)
//    Console.err.println("Reg")
    regRegisterAllocator.processFun(fun)
  }
}

trait GenericGraphColoringRegisterAllocator[T <: Operand] extends T86GenericRegisterAllocator[T] {

  // backwards must dataflow analysis
  class LivenessAnalysis(cfg: T86BasicBlockCfg)
    extends DataflowAnalysis.Builder[T86BasicBlock](cfg, forward = false)
      with FixpointComputation.Naive {

    import LivenessAnalysis._

    type NodeState = Set[T] // set of live-in variables for a basic block

    override def nodeStateLattice: Lattice[NodeState] = Lattice.powersetLat(???)

    override def cfgNodes: Seq[T86BasicBlock] = cfg.nodes

    private val bbDefUse: Map[CfgNode, DefUse] = cfg.nodes.map(bb => bb -> getBasicBlockDefUse(bb)).toMap

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

    def edgeSet: Set[(T, T)]

    def adjList: Map[T, Set[T]]

    def regRegMoveInsns: Set[T86InsnRef]

    def regRegMoveList: Map[T, Set[T86InsnRef]]

    def getDefUseCount(node: T): Int

    def getLiveRangeSize(node: T): Int
  }

  object InterferenceGraph {
    def apply(cfg: T86BasicBlockCfg): InterferenceGraph =
      apply(cfg, new LivenessAnalysis(cfg).result())

    def apply(cfg: T86BasicBlockCfg, livenessResult: LivenessAnalysis.Result): InterferenceGraph = {
      val _regRegMoveList = mutable.Map.empty[T, Set[T86InsnRef]].withDefaultValue(Set.empty)
      var _regRegMoveInsns = Set.empty[T86InsnRef]
      var _edgeSet = Set.empty[(T, T)]
      val _adjList = mutable.Map.empty[T, Set[T]].withDefaultValue(Set.empty)
      var _nodes = machineRegs

      def addEdge(u: T, v: T): Unit = {
        if (u != v && !_edgeSet.contains((u, v))) {
          _edgeSet += ((u, v))
          _edgeSet += ((v, u))
          if (!machineRegs.contains(u)) {
            _adjList(u) += v
          }
          if (!machineRegs.contains(v)) {
            _adjList(v) += u
          }
        }
      }

      val _defUseCounts = mutable.Map.empty[T, Int].withDefaultValue(0)
      val _liveRangeSizes = mutable.Map.empty[T, Int].withDefaultValue(0)

      cfg.nodes.foreach(bb => {
        var live = livenessResult.getLiveOut(bb)
        val inLoop = cfg.isInLoop(bb)

        for ((insn, insnRef) <- bb.insnsWithRefs.reverse) {
          val DefUse(defines, uses) = getInsnDefUse(insn)
          _nodes ++= defines ++ uses

          (defines ++ uses).foreach(node => {
            _defUseCounts(node) = _defUseCounts(node) + (if (inLoop) 10 else 1)
          })

          if (isRegRegMove(insn)) {
            // We just copy one register to another of the same type
            // The live ranges don't overlap, because if we color the temps with the same color, the move is redundant
            live --= uses
            for (temp <- defines ++ uses) {
              _regRegMoveList(temp) += insnRef
            }
            _regRegMoveInsns += insnRef
          }

          live ++= defines
          for(l <- live; d <- defines)
            addEdge(d, l)
          live = (live -- defines) ++ uses

          for(l <- live)
            _liveRangeSizes(l) = _liveRangeSizes(l) + 1
        }
      })

      for (temp <- _nodes)
        if (!_adjList.contains(temp))
          _adjList(temp) = Set.empty

      Console.err.println(_liveRangeSizes)

      new InterferenceGraph {
        override def nodes: Set[T] = _nodes

        override def edgeSet: Set[(T, T)] = _edgeSet

        override def adjList: Map[T, Set[T]] = _adjList.toMap

        override def regRegMoveInsns: Set[T86InsnRef] = _regRegMoveInsns

        override def regRegMoveList: Map[T, Set[T86InsnRef]] = _regRegMoveList.toMap

        override def getDefUseCount(node: T): Int = _defUseCounts(node)

        override def getLiveRangeSize(node: T): Int = _liveRangeSizes(node)
      }
    }
  }

  trait InterferenceGraphColoring {
    def colorMap: Map[T, T]

    def spilledNodes: Set[T]
  }

  object InterferenceGraphColoring {
    def apply(interf: InterferenceGraph): InterferenceGraphColoring = {
      var _selectStack = List.empty[T]
      var _simplifyWorklist = List.empty[T]

      var _spillWorklist = Set.empty[T]
      var _spilledNodes = Set.empty[T]

      var _initial = interf.nodes -- machineRegs // set of nodes excluding precolored nodes
      val _origAdjList = interf.adjList
      val _adjList = mutable.Map.from(_origAdjList)
      val _regRegMoveList = interf.regRegMoveList

      def isMoveRelated(node: T): Boolean = _regRegMoveList.contains(node)

      def getSpillCost(node: T): Double = {
        if(interf.getLiveRangeSize(node) == 1)
          Double.PositiveInfinity
        else
         interf.getDefUseCount(node) / _origAdjList(node).size
      }

      for (node <- _initial) {
        if (_adjList(node).size >= machineRegs.size)
          _spillWorklist += node
        //        else if(isMoveRelated(node))
        //          freezeWorklist ::= node
        else
          _simplifyWorklist ::= node
      }
      _initial = Set.empty

      while (_simplifyWorklist.nonEmpty || _spillWorklist.nonEmpty) {
        if (_simplifyWorklist.nonEmpty) {
          val node = _simplifyWorklist.head
          _simplifyWorklist = _simplifyWorklist.tail

          _selectStack ::= node // mark node for coloring
          for (adj <- _adjList(node)) {
            _adjList(adj) -= node // decrement degree
            if (_adjList(adj).size == machineRegs.size) { // this condition ensures that there are no dupes in _simplifyWorklist
              _spillWorklist -= adj
              //            if(isMoveRelated(adj))
              //              freezeWorklist ::= adj
              //            else
              _simplifyWorklist ::= adj
            }
          }
        } else if (_spillWorklist.nonEmpty) {
          val node = _spillWorklist.minBy(getSpillCost)
          _spillWorklist -= node

          Console.err.println(s"Candidate for spill: $node")

          _simplifyWorklist ::= node
        }
      }

      val _colorMap = mutable.Map.from(machineRegs.map(r => (r -> r)))
      for (node <- _selectStack) {
        var okColors = machineRegs
        for (adj <- _origAdjList(node)) {
          _colorMap.get(adj).foreach(color => okColors -= color)
        }
        if (okColors.isEmpty) {
          _spilledNodes += node
        } else {
          val newColor = okColors.head
          _colorMap(node) = newColor
        }
      }

      new InterferenceGraphColoring {
        override def colorMap: Map[T, T] = _colorMap.toMap

        override def spilledNodes: Set[T] = _spilledNodes
      }
    }
  }

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

  def removeRedundantMoves(cfg: T86BasicBlockCfg): Unit = {
    val livenessResult = new LivenessAnalysis(cfg).result()

    cfg.nodes.foreach(bb => {
      val newBodyReversed = IndexedSeq.newBuilder[T86ListingElement]
      var live = livenessResult.getLiveOut(bb)

      bb.body.reverse.foreach({
        case insn: T86Insn =>
          val DefUse(defines, uses) = getInsnDefUse(insn)
          // live now contains registers that can be read after this instruction

          if(isRegRegMove(insn)) {
            val BinaryT86Insn(_, dest: T, src: T) = insn
            if(dest != src && live.contains(dest))
              newBodyReversed += insn
            else
              Console.err.println(s"Removed redundant move $insn")
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

  def processFun(fun: T86Fun): Unit = {
    // insert code to backup and restore all callee save registers
    remapCalleeSaveRegs(fun)

    val cfg = T86BasicBlockCfg(fun)

    var hasSpilledNodes = false
    do {
      Console.err.println(new T86AsmPrinter().printToString(fun.flatten))

      val interferenceGraph = InterferenceGraph(cfg)
//      Console.err.println(s"Interfering registers for ${fun.irFun.get.name}: ${interferenceGraph.edgeSet}")

      val coloring = InterferenceGraphColoring(interferenceGraph)
      Console.err.println(s"Registers to spill: ${coloring.spilledNodes}")

      if (coloring.spilledNodes.nonEmpty) {
        hasSpilledNodes = true
        rewriteFunWithSpilledNodes(fun, coloring.spilledNodes)
      } else {
        hasSpilledNodes = false
        remapRegistersInFun(fun, coloring.colorMap.withDefault(reg => reg))
      }
    } while (hasSpilledNodes)

    removeRedundantMoves(cfg)
  }
}