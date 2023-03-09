package tinycc.backend.t86.regalloc

import tinycc.backend.RegisterAllocator
import tinycc.backend.t86.T86Opcode._
import tinycc.backend.t86._

import scala.language.implicitConversions

abstract class T86RegisterAllocator(program: T86Program) extends RegisterAllocator[T86Program](program) {
  def result(): T86Program
}

object T86RegisterAllocator {
  /** Wrapper type over all register types. */
  case class Temp(op: Operand) {
    require(op.isInstanceOf[Operand.Reg] || op.isInstanceOf[Operand.FReg]) // union types are complicated in scala

    def toReg: Operand.Reg = op.asInstanceOf[Operand.Reg]

    def toFReg: Operand.FReg = op.asInstanceOf[Operand.FReg]

    def isSameType(other: Temp): Boolean = (op, other.op) match {
      case (_: Operand.Reg, _: Operand.Reg) => true
      case (_: Operand.FReg, _: Operand.FReg) => true
      case _ => false
    }
  }

  implicit def reg2tmp(reg: Operand.Reg): Temp = Temp(reg)

  implicit def freg2tmp(freg: Operand.FReg): Temp = Temp(freg)

  val machineRegs: Set[Temp] = (T86Utils.machineRegs ++ T86Utils.machineFRegs).map(Temp)
  // special registers which are not handled by the register allocator (SP, BP, IP)
  val specialRegs: Set[Temp] = T86Utils.specialRegs.map(Temp)
  val callerSaveRegs: Set[Temp] = T86Utils.callerSaveFRegs.map(Temp)
  val calleeSaveRegs: Set[Temp] = T86Utils.calleeSaveRegs.map(Temp)

  /**
   * @param defines kill set
   * @param uses    gen set
   */
  case class DefUse(defines: Set[Temp], uses: Set[Temp]) {
    def ++(that: DefUse): DefUse = DefUse(defines.union(that.defines), uses.union(that.uses))

    def exclSpecialRegs: DefUse = DefUse(defines.filter(!specialRegs.contains(_)), uses.filter(!specialRegs.contains(_)))

    def isEmpty: Boolean = defines.isEmpty && uses.isEmpty

    def nonEmpty: Boolean = defines.nonEmpty || uses.nonEmpty
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

    case UnaryT86Insn(CALL, operand0) => getOperandReadDefUse(operand0) ++ DefUse(Set.empty, T86Utils.callerSaveFRegs.map(Temp))
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

  def remapRegistersInFun(fun: T86Fun, regMap: Map[Temp, Temp]): Unit = {
    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.map({
        case insn: T86Insn => remapRegisters(insn, regMap)
        case elem => elem
      })
    })
  }

  def apply(program: T86Program): T86RegisterAllocator = new GraphColoringRegisterAllocator(program)
}