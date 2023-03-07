package tinycc.backend.t86.regalloc

import tinycc.backend.RegisterAllocator
import tinycc.backend.t86.T86Opcode._
import tinycc.backend.t86._
import tinycc.common._

import scala.collection.mutable
import scala.language.implicitConversions

object T86RegisterAllocation {
  /** Wrapper type over all register types. */
  case class Temp(op: Operand) {
    require(op.isInstanceOf[Operand.Reg] || op.isInstanceOf[Operand.FReg]) // union types are complicated in scala

    def toReg: Operand.Reg = op.asInstanceOf[Operand.Reg]

    def toFReg: Operand.FReg = op.asInstanceOf[Operand.FReg]
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

  def isMachineTemp(temp: Temp): Boolean = temp match {
    case Temp(reg: Operand.Reg) => T86Utils.machineRegs.contains(reg)
    case Temp(freg: Operand.FReg) => T86Utils.machineFRegs.contains(freg)
    case _ => false
  }

  def isMachineOrSpecialTemp(temp: Temp): Boolean = temp match {
    case Temp(reg: Operand.Reg) => T86Utils.machineRegs.contains(reg) || T86Utils.specialRegs.contains(reg)
    case Temp(freg: Operand.FReg) => T86Utils.machineFRegs.contains(freg)
    case _ => false
  }

  //  implicit class T86BasicBlockCfgOps(that: T86BasicBlock)(implicit cfg: T86BasicBlockCfg) {
  //    def pred: Seq[T86BasicBlock] = cfg.getPred(that)
  //
  //    def succ: Seq[T86BasicBlock] = cfg.getSucc(that)
  //  }

  // backwards must dataflow analysis
  //  class T86BasicBlockLivenessAnalysis(cfg: T86BasicBlockCfg)
  //    extends DataflowAnalysis.Builder[T86BasicBlock](cfg, forward = false)
  //      with FixpointComputation.Naive {
  //
  //    import T86BasicBlockLivenessAnalysis._
  //
  //    type NodeState = Set[Temp] // set of live-in variables for a basic block
  //
  //    override protected def nodeStateLattice: Lattice[NodeState] = PowersetLattice(() => ???)
  //
  //    override def cfgNodes: Seq[T86BasicBlock] = cfg.nodes
  //
  //    private val bbDefUse: Map[CfgNode, DefUse] = Map.from(cfg.nodes.map(bb => bb -> getBasicBlockDefUse(bb)))
  //
  //    /** Returns live-in variables for this basic block. */
  //    override def transfer(bb: T86BasicBlock, liveOutVars: Set[Temp]): Set[Temp] = {
  //      val DefUse(defines, uses) = bbDefUse(bb)
  //      (liveOutVars -- defines) ++ uses
  //    }
  //
  //    def result(): Result = Result(fixpoint(), join)
  //  }
  //
  //  object T86BasicBlockLivenessAnalysis {
  //    type NodeState = Set[Temp]
  //    type CfgState = Map[T86BasicBlock, NodeState]
  //
  //    case class Result(cfgState: CfgState, join: (T86BasicBlock, CfgState) => NodeState) {
  //      def getLiveOut(bb: T86BasicBlock): Set[Temp] = join(bb, cfgState)
  //
  //      def getLiveIn(bb: T86BasicBlock): Set[Temp] = cfgState(bb)
  //    }
  //  }

  trait InterferenceGraph extends Graph[Temp] {
    def edges: Seq[(Temp, Temp)]
  }

  object InterferenceGraph {
    class Builder {
      val adjMap: mutable.Map[Temp, mutable.Set[Temp]] = mutable.Map.empty

      val edgeSet: mutable.Set[(Temp, Temp)] = mutable.Set.empty

      def addEdge(u: Temp, v: Temp): Unit = {
        if (u != v) {
          adjMap(u).addOne(v)
          adjMap(v).addOne(u)
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

  def remapRegisters(operand: Operand, regMap: Map[Temp, Temp]): Operand = operand match {
    case imm: Operand.Imm => imm
    case label: Operand.Label => label

    case reg: Operand.Reg => regMap(reg).op
    case Operand.RegImm(reg, offset) => Operand.RegImm(regMap(reg).toReg, offset)
    case Operand.RegReg(reg, reg2) => Operand.RegReg(regMap(reg).toReg, reg2)
    case Operand.RegScaled(reg, scale) => Operand.RegScaled(regMap(reg).toReg, scale)
    case Operand.RegImmReg(reg, offset, reg2) => Operand.RegImmReg(regMap(reg).toReg, offset, regMap(reg2).toReg)
    case Operand.RegRegScaled(reg, scaledReg, scale) => Operand.RegRegScaled(regMap(reg).toReg, regMap(scaledReg).toReg, scale)
    case Operand.RegImmRegScaled(reg, offset, scaledReg, scale) => Operand.RegImmRegScaled(regMap(reg).toReg, offset, regMap(scaledReg).toReg, scale)

    case Operand.MemImm(addr) => Operand.MemImm(addr)
    case Operand.MemReg(addrReg) => Operand.MemReg(regMap(addrReg).toReg)
    case Operand.MemRegImm(addrReg, addrOffset) => Operand.MemRegImm(regMap(addrReg).toReg, addrOffset)
    case Operand.MemRegReg(addrReg, addrReg2) => Operand.MemRegReg(regMap(addrReg).toReg, regMap(addrReg2).toReg)
    case Operand.MemRegScaled(addrReg, addrScale) => Operand.MemRegScaled(regMap(addrReg).toReg, addrScale)
    case Operand.MemRegImmReg(addrReg, addrOffset, addrReg2) => Operand.MemRegImmReg(regMap(addrReg).toReg, addrOffset, regMap(addrReg2).toReg)
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => Operand.MemRegRegScaled(regMap(addrReg).toReg, regMap(addrScaledReg).toReg, addrScale)
    case imm: Operand.FImm => imm
    case reg: Operand.FReg => regMap(reg).toFReg
  }

  def remapRegisters(insn: T86Insn, regMap: Map[Temp, Temp]): T86Insn = insn match {
    case insn: NullaryT86Insn => insn
    case UnaryT86Insn(op, operand0) => UnaryT86Insn(op, remapRegisters(operand0, regMap))
    case BinaryT86Insn(op, operand0, operand1) => BinaryT86Insn(op, remapRegisters(operand0, regMap), remapRegisters(operand1, regMap))
  }

  class NaiveRegisterAllocator(program: T86Program) extends RegisterAllocator(program: T86Program) {
    def result(): T86Program = {
      program.funs.foreach(processFun)
      program
    }

    def processFun(fun: T86Fun): Unit = {
      // remap all non-machine registers into machine regs
      val regMap = mutable.Map.empty[Temp, Temp]

      val availableRegs = mutable.Queue.from(T86Utils.machineRegs)
      val availableFRegs = mutable.Queue.from(T86Utils.machineFRegs)

      fun.flatten.foreach({
        case insn: T86Insn => {
          val DefUse(defines, uses) = getInsnDefUse(insn)
          (defines ++ uses).foreach({
            case temp@Temp(reg: Operand.Reg) if !T86Utils.machineRegs.contains(reg) && !T86Utils.specialRegs.contains(reg) =>
              regMap.getOrElseUpdate(temp, availableRegs.dequeue())

            case temp@Temp(freg: Operand.FReg) if !T86Utils.machineFRegs.contains(freg) =>
              regMap.getOrElseUpdate(temp, availableFRegs.dequeue())

            case _ =>
          })
        }

        case _ =>
      })

      val regMap2 = regMap.toMap.withDefault(reg => reg) // default mapping for machine and special registers
      fun.basicBlocks.foreach(bb => {
        bb.body = bb.body.map({
          case insn: T86Insn => remapRegisters(insn, regMap2)
          case elem => elem
        })
      })

      // insert PUSH and POP insns to backup and restore them in the fun prologue and epilogue
      val usedRegs = regMap.values.toSeq // convert to seq, so order is deterministic
      val backupCode = usedRegs.collect({
        case Temp(reg: Operand.Reg) => T86Insn(PUSH, reg)
        case Temp(freg: Operand.FReg) => T86Insn(FPUSH, freg)
      })
      val restoreCode = usedRegs.reverse.collect({
        case Temp(reg: Operand.Reg) => T86Insn(POP, reg)
        case Temp(freg: Operand.FReg) => T86Insn(FPOP, freg)
      })

      fun.basicBlocks.foreach(bb => {
        bb.body = bb.body.flatMap({
          case T86SpecialLabel.FunPrologueMarker => T86SpecialLabel.FunPrologueMarker +: backupCode
          case T86SpecialLabel.FunEpilogueMarker => restoreCode :+ T86SpecialLabel.FunEpilogueMarker

          case elem => Seq(elem)
        })
      })
    }
  }

  class GraphColoringRegisterAllocator(program: T86Program) extends RegisterAllocator(program: T86Program) {
    def result(): T86Program = {
      program.funs.foreach(processFun)
      program
    }

    def processFun(fun: T86Fun): Unit = {}
  }

  //  def allocateRegisters(fun: T86Fun, cfg: T86BasicBlockCfg): Unit = {
  //    val livenessAnalysisResult = new T86BasicBlockLivenessAnalysis(cfg).result
  //
  //    val interfBuilder = InterferenceGraph.newBuilder
  //    fun.basicBlocks.foreach(bb => {
  //      var live = livenessAnalysisResult.getLiveOut(bb)
  //
  //      for (insn <- bb.insns.reverse) {
  //        val DefUse(defines, uses) = getInsnDefUse(insn)
  //
  //        if (isRegRegMove(insn)) {
  //          live --= uses
  //          // TODO: add to move worklist
  //        }
  //
  //        live ++= defines
  //        for (d <- defines; l <- live)
  //          interfBuilder.addEdge(d, l)
  //        live = (live -- defines) ++ uses
  //      }
  //    })
  //    val interf = interfBuilder.result()
  //
  //    Console.out.println(interf.edges)
  //  }
}