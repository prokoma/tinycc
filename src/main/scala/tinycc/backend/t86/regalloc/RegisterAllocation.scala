package tinycc.backend.t86.regalloc

import tinycc.backend.t86.T86Opcode._
import tinycc.backend.t86._
import tinycc.common._

import scala.collection.mutable
import scala.language.implicitConversions

object T86RegisterAllocation {
  case class Temp(op: Operand) {
    require(op.isInstanceOf[Operand.Reg] || op.isInstanceOf[Operand.FReg]) // union types are complicated in scala
  }

  implicit def reg2tmp(reg: Operand.Reg): Temp = Temp(reg)

  implicit def freg2tmp(freg: Operand.FReg): Temp = Temp(freg)

  /**
   * @param defines kill set
   * @param uses    gen set
   */
  case class DefUse(defines: Set[Temp], uses: Set[Temp]) {
    def ++(that: DefUse): DefUse = DefUse(defines.union(that.defines), uses.union(that.uses))
  }

  object DefUse {
    val empty: DefUse = DefUse(Set.empty, Set.empty)
  }

  /** Return which registers the operand defines and uses when used as a destination. */
  def getOperandWriteDefUse(op: Operand): DefUse = op match {
    case op: Operand.Reg => DefUse(Set(op), Set.empty)
    case op: Operand.FReg => DefUse(Set(op), Set.empty)
    case Operand.MemImm(addr) => DefUse.empty
    case Operand.MemReg(addrReg) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegImm(addrReg, addrOffset) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegReg(addrReg, addrReg2) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegScaled(addrReg, addrScale) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegImmReg(addrReg, addrOffset, addrReg2) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => DefUse(Set.empty, Set(addrReg, addrScaledReg))

    case op => throw new IllegalArgumentException(s"Operand $op cannot be used as a destination.")
  }

  def getOperandReadDefUse(op: Operand): DefUse = op match {
    case _: Operand.Imm | _: Operand.Label | _: Operand.MemImm | _: Operand.FImm => DefUse.empty
    case reg: Operand.Reg => DefUse(Set.empty, Set(reg))
    case Operand.RegImm(reg, offset) => DefUse(Set.empty, Set(reg))
    case Operand.RegReg(reg, reg2) => DefUse(Set.empty, Set(reg, reg2))
    case Operand.RegScaled(reg, scale) => DefUse(Set.empty, Set(reg))
    case Operand.RegImmReg(reg, offset, reg2) => DefUse(Set.empty, Set(reg, reg2))
    case Operand.RegRegScaled(reg, scaledReg, scale) => DefUse(Set.empty, Set(reg, scaledReg))
    case Operand.RegImmRegScaled(reg, offset, scaledReg, scale) => DefUse(Set.empty, Set(reg, scaledReg))
    case Operand.MemReg(addrReg) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegImm(addrReg, addrOffset) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegReg(addrReg, addrReg2) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegScaled(addrReg, addrScale) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegImmReg(addrReg, addrOffset, addrReg2) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => DefUse(Set.empty, Set(addrReg, addrScaledReg))
    case reg: Operand.FReg => DefUse(Set.empty, Set(reg))
  }

  def getOperandReadWriteDefUse(op: Operand): DefUse = getOperandReadDefUse(op) ++ getOperandWriteDefUse(op)

  def getInsnDefUse(insn: T86Insn): DefUse = insn match {
    case NullaryT86Insn(op) => DefUse.empty

    case UnaryT86Insn(JMP | CALL | PUSH | FPUSH | POP | FPOP | PUTCHAR | PUTNUM | _: CondJmpOp, operand0) => getOperandReadDefUse(operand0)
    case UnaryT86Insn(POP | FPOP | GETCHAR, operand0) => getOperandWriteDefUse(operand0)
    case UnaryT86Insn(INC | DEC | NOT | NEG, operand0) => getOperandReadWriteDefUse(operand0)

    case BinaryT86Insn(CMP | FCMP, operand0, operand1) => getOperandReadDefUse(operand0) ++ getOperandReadDefUse(operand1)
    case BinaryT86Insn(MOV | LEA | EXT | NRW, operand0, operand1) => getOperandWriteDefUse(operand0) ++ getOperandReadDefUse(operand1)
    case BinaryT86Insn(ADD | SUB | MUL | DIV | MOD | IMUL | IDIV | AND | OR | XOR | LSH | RSH | FADD | FSUB | FMUL | FDIV | LOOP, operand0, operand1) =>
      getOperandReadWriteDefUse(operand0) ++ getOperandReadDefUse(operand1)
  }

  def getBasicBlockDefUse(bb: T86BasicBlock): DefUse =
    bb.body.foldLeft(DefUse.empty)((prev, elem) => elem match {
      case insn: T86Insn =>
        val cur = getInsnDefUse(insn)
        DefUse(prev.defines ++ cur.defines, (prev.uses -- cur.defines) ++ cur.uses)

      case _ => prev
    })

  def isRegRegMove(insn: T86Insn): Boolean = insn match {
    case BinaryT86Insn(MOV, _: Operand.Reg, _: Operand.Reg) => true
    case BinaryT86Insn(MOV, _: Operand.FReg, _: Operand.FReg) => true
    case _ => false
  }

  trait Graph[T] {
    def nodes: Seq[T]

    def getSucc(node: T): Seq[T]

    def getPred(node: T): Seq[T]

    def getOutDegree(node: T): Int = getSucc(node).size

    def getInDegree(node: T): Int = getPred(node).size

    def getDegree(node: T): Int = getOutDegree(node) + getInDegree(node)
  }

  // allows us to get entry node, exit nodes, successors and predecessors
  trait T86BasicBlockCfg extends Graph[T86BasicBlock] {
    def nodes: Seq[T86BasicBlock]

    def entry: T86BasicBlock

    def exit: Seq[T86BasicBlock]

    def getSucc(block: T86BasicBlock): Seq[T86BasicBlock]

    def getPred(block: T86BasicBlock): Seq[T86BasicBlock]
  }

  object T86BasicBlockCfg {
    def apply(fun: T86Fun, irFun: ir.IrFun, basicBlockMap: Map[ir.BasicBlock, T86BasicBlock]): T86BasicBlockCfg = {
      val invBasicBlockMap = basicBlockMap.map(_.swap)
      // TODO: maybe build the graph beforehand, so map lookups and converting to seq are not necessary
      new T86BasicBlockCfg {
        override def nodes: Seq[T86BasicBlock] = fun.basicBlocks.toSeq

        override def entry: T86BasicBlock = basicBlockMap(irFun.entryBlock)

        override def exit: Seq[T86BasicBlock] = irFun.exitPoints.map(insn => basicBlockMap(insn.basicBlock))

        override def getSucc(block: T86BasicBlock): Seq[T86BasicBlock] = invBasicBlockMap(block).succ.map(basicBlockMap(_))

        override def getPred(block: T86BasicBlock): Seq[T86BasicBlock] = invBasicBlockMap(block).pred.map(basicBlockMap(_))
      }
    }
  }

  //  implicit class T86BasicBlockCfgOps(that: T86BasicBlock)(implicit cfg: T86BasicBlockCfg) {
  //    def pred: Seq[T86BasicBlock] = cfg.getPred(that)
  //
  //    def succ: Seq[T86BasicBlock] = cfg.getSucc(that)
  //  }

  // backwards must dataflow analysis
  class T86BasicBlockLivenessAnalysis(cfg: T86BasicBlockCfg)
    extends DataflowAnalysis.Builder[T86BasicBlock](cfg, forward = false)
      with FixpointComputation.Naive {

    import T86BasicBlockLivenessAnalysis._

    type NodeState = Set[Temp] // set of live-in variables for a basic block

    override protected def nodeStateLattice: Lattice[NodeState] = PowersetLattice(() => ???)

    override def cfgNodes: Seq[T86BasicBlock] = cfg.nodes

    private val bbDefUse: Map[CfgNode, DefUse] = Map.from(cfg.nodes.map(bb => bb -> getBasicBlockDefUse(bb)))

    /** Returns live-in variables for this basic block. */
    override def transfer(bb: T86BasicBlock, liveOutVars: Set[Temp]): Set[Temp] = {
      val DefUse(defines, uses) = bbDefUse(bb)
      (liveOutVars -- defines) ++ uses
    }

    lazy val result: Result = Result(fixpoint(), join)
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

      val edges: mutable.Set[(Temp, Temp)] = mutable.Set.empty

      def addEdge(u: Temp, v: Temp): Unit = {
        if(u != v) {
          adjMap(u).addOne(v)
          adjMap(v).addOne(u)
          edges.addOne((u, v))
          edges.addOne((v, u))
        }
      }

      def result(): InterferenceGraph = new InterferenceGraph {

        override def edges: Seq[(Temp, Temp)] = edges

        override def nodes: Seq[Temp] = adjMap.keySet.toSeq

        override def getSucc(node: Temp): Seq[Temp] = adjMap(node).toSeq

        override def getPred(node: Temp): Seq[Temp] = adjMap(node).toSeq

        override def getDegree(node: Temp): Int = adjMap(node).size

        override def getInDegree(node: Temp): Int = throw new UnsupportedOperationException()

        override def getOutDegree(node: Temp): Int = throw new UnsupportedOperationException()
      }
    }

    def newBuilder: Builder = new Builder
  }

  def allocateRegisters(fun: T86Fun, cfg: T86BasicBlockCfg): Unit = {
    val livenessAnalysisResult = new T86BasicBlockLivenessAnalysis(cfg).result

    val interfBuilder = InterferenceGraph.newBuilder
    fun.basicBlocks.foreach(bb => {
      var live = livenessAnalysisResult.getLiveOut(bb)

      for (insn: T86Insn <- bb.body.reverseIterator) {
        val DefUse(defines, uses) = getInsnDefUse(insn)

        if (isRegRegMove(insn)) {
          live -= uses
          // TODO: add to move worklist
        }

        live ++= defines
        for (d <- defines; l <- live)
          interfBuilder.addEdge(d, l)
        live = (live -- defines) ++ uses
      }
    })
    val interf = interfBuilder.result()

    Console.out.println(interf.edges)
  }
}