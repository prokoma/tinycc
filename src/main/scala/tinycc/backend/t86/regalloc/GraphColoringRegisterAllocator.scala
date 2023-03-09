package tinycc.backend.t86.regalloc

import tinycc.backend.BackendException
import tinycc.backend.t86.T86Opcode.MOV
import tinycc.backend.t86._
import tinycc.backend.t86.regalloc.GraphColoringRegisterAllocator.InterferenceGraph
import tinycc.common._

import scala.collection.mutable

class GraphColoringRegisterAllocator(program: T86Program) extends T86RegisterAllocator(program: T86Program) {

  import T86RegisterAllocator._

  override def result(): T86Program = {
    program.funs.foreach(processFun)
    program
  }

  def remapCalleeSaveRegs(fun: T86Fun): Unit = {
    val regMap = mutable.Map.empty[Operand.Reg, Operand.Reg]
    val backupCode = T86Utils.calleeSaveRegs.toSeq.map(reg => {
      val newReg = fun.freshReg()
      regMap(reg) = newReg
      T86Insn(MOV, newReg, reg)
    })
    val restoreCode = T86Utils.calleeSaveRegs.toSeq.map(reg => {
      T86Insn(MOV, reg, regMap(reg))
    })

    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.flatMap({
        case T86SpecialLabel.FunPrologueMarker => T86SpecialLabel.FunPrologueMarker +: backupCode
        case T86SpecialLabel.FunEpilogueMarker => restoreCode :+ T86SpecialLabel.FunEpilogueMarker

        case elem => Seq(elem)
      })
    })
  }

//  def remapCallerSaveRegs(fun: T86Fun): Unit = {
//    val fregMap = mutable.Map.empty[Operand.FReg, Operand.FReg]
//
//    fun.basicBlocks.foreach(bb => {
//      bb.body = bb.body.flatMap({
//        case T86SpecialLabel.CallPrologueMarker => {
//          fregMap.clear()
//          T86SpecialLabel.FunPrologueMarker +: T86Utils.callerSaveFRegs.toSeq.map(reg => {
//            val newReg = fun.freshFReg()
//            fregMap(reg) = newReg
//            T86Insn(MOV, newReg, reg)
//          })
//        }
//        case T86SpecialLabel.CallEpilogueMarker => T86Utils.callerSaveFRegs.toSeq.map(reg => {
//          T86Insn(MOV, reg, fregMap(reg))
//        }) :+ T86SpecialLabel.FunEpilogueMarker
//
//        case elem => Seq(elem)
//      })
//    })
//  }

  def processFun(fun: T86Fun): Unit = {
    // insert code to backup and restore all callee save registers
//    remapCalleeSaveRegs(fun)
//    remapCallerSaveRegs(fun)

    val cfg = T86BasicBlockCfg(fun)
    val interferenceGraph = InterferenceGraph(cfg, machineRegs)

    Console.err.println(s"Interfering registers for ${fun.irFun.get.name}: ${interferenceGraph.edgeSet}")

    def colorInterfGraph(interf: InterferenceGraph): Map[Temp, Temp] = {
      var selectStack = List.empty[Temp]
      var simplifyWorklist = List.empty[Temp]
      val K = machineRegs.size

      var initial = interf.nodes -- machineRegs // set of nodes excluding precolored nodes
      val origAdjList = interf.adjList
      val adjList = mutable.Map.from(origAdjList)

      for(temp <- initial) {
        if(adjList(temp).size >= K)
          throw new BackendException(s"Node degree too high: $temp.")
        else
          simplifyWorklist ::= temp
      }
      initial = Set.empty

      while(simplifyWorklist.nonEmpty) {
        val temp = simplifyWorklist.head
        simplifyWorklist = simplifyWorklist.tail

        selectStack ::= temp // mark node for coloring
        for(adj <- adjList(temp)) {
          adjList(adj) -= temp // decrement degree
          if(adjList(adj).size == K)
            simplifyWorklist ::= adj
        }
      }

      val colorMap = mutable.Map.from(machineRegs.map(r => (r -> r)))
      for(temp <- selectStack) {
        var okColors = machineRegs.filter(_.isSameType(temp))
        for(adj <- origAdjList(temp)) {
          colorMap.get(adj).foreach(color => okColors -= color)
        }
        if(okColors.isEmpty)
          throw new BackendException(s"No colors available for $temp.")
        val newColor = okColors.head
        colorMap(temp) = newColor
      }

      colorMap.toMap
    }

    val regMap = colorInterfGraph(interferenceGraph).withDefault(reg => reg)
    remapRegistersInFun(fun, regMap)
  }

//    // 1. construct CFG
//    val cfg = T86BasicBlockCfg(fun)
//
//    // 2. run liveness analysis on basic blocks
//    val
//
//    // 3. compute live temporaries for each instruction in basic block and build interference graph
//    val initial = mutable.Set.empty[Temp] // set of all virtual registers, which we need to remap to machine registers
//
//
//    val simplifyWorklist = mutable.Buffer.from(initial)
//    val nodeDegrees = mutable.Map.from(interf.nodes.map(node => (node -> interf.getDegree(node))))
//
//    val coloredNodes = mutable.Map.empty[Temp, Temp]
//    val selectStack = mutable.Stack.empty[Temp]
//
//    // 4. iteratively search for nodes with small degree and push them onto coloring stack
//    while (simplifyWorklist.nonEmpty) {
//      if (simplifyWorklist.nonEmpty)
//        simplify();
//    }
//
//
//    // 5. pop from the coloring stack and assign colors
//
//    // 6. rewrite program with the assigned registers
//
//    Console.out.println(interf.edges)
//  }
//
//  final class FunImpl(fun: T86Fun, cfg: T86BasicBlockCfg) {
//    type Node = Temp
//
//    var livenessResult: T86BasicBlockLivenessAnalysis.Result = _
//    var interf: InterferenceGraph = _
//    val initial: mutable.Set[Temp] = mutable.Set.empty
//    val simplifyWorklist: mutable.Buffer[Temp] = mutable.Buffer.empty
//    val moveWorklist: mutable.Buffer[Temp] = mutable.Buffer.empty
//    // nodes removed from the graph to be assigned a color
//    val selectStack: mutable.Stack[Temp] = mutable.Stack.empty
//
//    initial ++= collectTempsToColor()
//    main()
//
//    private def collectTempsToColor(): Set[Temp] =
//      fun.insns.flatMap(insn => {
//        val DefUse(defines, uses) = getInsnDefUse(insn)
//        (defines ++ uses).filter(!isMachineOrSpecialTemp(_))
//      }).toSet
//
//    @tailrec
//    private def main(): Unit = {
//      livenessAnalysis()
//      build()
//      makeWorklist()
//      while (simplifyWorklist.nonEmpty) {
//        if (simplifyWorklist.nonEmpty)
//          simplify()
//      }
//      assignColors()
//    }
//
//    private def livenessAnalysis(): Unit = {
//      livenessResult = new T86BasicBlockLivenessAnalysis(cfg).result()
//    }
//
//    private def build(): Unit = {
//      val interfBuilder = InterferenceGraph.newBuilder
//      fun.basicBlocks.foreach(bb => {
//        var live = livenessResult.getLiveOut(bb)
//
//        for (insn <- bb.insns.reverse) {
//          val DefUse(defines, uses) = getInsnDefUse(insn)
//
//          if (isRegRegMove(insn)) {
//            // We just copy one register to another of the same type
//            // The live ranges don't overlap, because if we color the temps with the same color, the move is redundant
//            live --= uses
////            for (temp <- defines ++ uses) {
////              moveList(temp) += insn
////            }
//            moveWorklist += insn
//          }
//
//          live ++= defines
//          for (d <- defines if !isSpecialTemp(d); l <- live if !isSpecialTemp(l))
//            interfBuilder.addEdge(d, l)
//          live = (live -- defines) ++ uses
//        }
//      })
//      interf = interfBuilder.result()
//    }
//
//    private def makeWorklist(): Unit = {
//      for(temp <- initial) {
//        if (degree(temp) > K)
//          spillWorklist += temp
//        else if(isRegRegMove(temp))
//          freezeWorklist += temp
//        else
//          simplifyWorklist += temp
//      }
//    }
//
//    private def simplify(): Unit = {
//      val temp = simplifyWorklist.remove(0)
//      selectStack.push(temp)
//      for (m <- adjacent(temp)) {
//        decrementDegree(m)
//      }
//    }
//
//    private def decrementDegree(node: Temp): Unit = {
//
//    }
//  }
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

    private val bbDefUse: Map[CfgNode, DefUse] = cfg.nodes.map(bb => bb -> getBasicBlockDefUse(bb).exclSpecialRegs).toMap

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

  trait InterferenceGraph {
    def nodes: Set[Temp]

    def edgeSet: Set[(Temp, Temp)]

    def adjList: Map[Temp, Set[Temp]]

    def regRegMoves: Set[T86InsnRef]
  }

  case class T86InsnRef(bb: T86BasicBlock, index: Int) {
    def apply(): T86Insn = bb.body(index).asInstanceOf[T86Insn]
  }

  object InterferenceGraph {
    def apply(cfg: T86BasicBlockCfg, machineRegs: Set[Temp]): InterferenceGraph =
      apply(cfg.nodes, new T86BasicBlockLivenessAnalysis(cfg).result(), machineRegs)

    def apply(basicBlocks: Iterable[T86BasicBlock], livenessResult: T86BasicBlockLivenessAnalysis.Result, machineRegs: Set[Temp]): InterferenceGraph = {
      val _regRegMoveMap = mutable.Map.empty[Temp, mutable.Set[T86InsnRef]].withDefaultValue(mutable.Set.empty)
      var _regRegMoves = Set.empty[T86InsnRef]
      var _edgeSet = Set.empty[(Temp, Temp)]
      val _adjList = mutable.Map.empty[Temp, Set[Temp]].withDefaultValue(Set.empty)
      var _nodes = machineRegs

      def addEdge(u: Temp, v: Temp): Unit = {
        if(u != v && !_edgeSet.contains((u, v))) {
          _edgeSet += ((u, v))
          _edgeSet += ((v, u))
          if(!machineRegs.contains(u)) {
            _adjList(u) += v
          }
          if(!machineRegs.contains(v)) {
            _adjList(v) += u
          }
        }
      }

      basicBlocks.foreach(bb => {
        var live = livenessResult.getLiveOut(bb)
        val insnsWithIndex = bb.body.zipWithIndex.collect({ case (insn: T86Insn, i) => (insn, i) })

        for ((insn, i) <- insnsWithIndex.reverse) {
          val DefUse(defines, uses) = getInsnDefUse(insn).exclSpecialRegs

          _nodes ++= defines
          _nodes ++= uses

          if (isRegRegMove(insn)) {
            // We just copy one register to another of the same type
            // The live ranges don't overlap, because if we color the temps with the same color, the move is redundant
            live --= uses
            val insnRef = T86InsnRef(bb, i)
            for (temp <- defines ++ uses) {
              _regRegMoveMap(temp) += insnRef
            }
            _regRegMoves += insnRef
          }

          live ++= defines
          for (d <- defines; l <- live) addEdge(d, l)
          live = (live -- defines) ++ uses
        }
      })

      for(temp <- _nodes)
        if(!_adjList.contains(temp))
          _adjList(temp) = Set.empty

      new InterferenceGraph {
        override def nodes: Set[Temp] = _nodes

        override def edgeSet: Set[(Temp, Temp)] = _edgeSet

        override def adjList: Map[Temp, Set[Temp]] = _adjList.toMap

        override def regRegMoves: Set[T86InsnRef] = _regRegMoves
      }
    }
  }
}