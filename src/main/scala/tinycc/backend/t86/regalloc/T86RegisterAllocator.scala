package tinycc.backend.t86.regalloc

import tinycc.backend.t86.T86Opcode._
import tinycc.backend.t86._
import tinycc.common.ProgramTransform

import scala.language.implicitConversions

trait T86GenericRegisterAllocator[T <: Operand] {
  type RegMap = Map[T, T]

  case class DefUse(defines: Set[T], uses: Set[T]) {
    def regs: Set[T] = defines ++ uses

    def withoutIgnoredRegs: DefUse = DefUse(defines.diff(ignoredRegs), uses.diff(ignoredRegs))

    def ++(that: DefUse): DefUse = DefUse(defines.union(that.defines), uses.union(that.uses))
  }

  object DefUse {
    val empty: DefUse = DefUse(Set.empty, Set.empty)
  }

  /** General purpose registers available for allocation. */
  def machineRegs: Set[T]

  /** Special registers, which should be ignored by the register allocator. */
  def ignoredRegs: Set[T]

  /** Registers which are defined by a call instruction. A subset of [[machineRegs]]. */
  def returnValueRegs: Set[T]

  /** Registers which should be live during any function call. A subset of [[machineRegs]]. */
  def callerSaveRegs: Set[T]

  /** Registers which should be live inside whole function body. A subset of [[machineRegs]]. */
  def calleeSaveRegs: Set[T]

  /** Returns what the operand defines and uses when it is used as write target (no read). Defines and uses are subset of [[machineRegs]]. */
  def getOperandWriteDefUse(op: Operand): DefUse

  /** Returns what the operand defines and uses when it is used as read target. Defines and uses are subset of [[machineRegs]]. */
  def getOperandReadDefUse(op: Operand): DefUse

  /** Returns true for MOV Rx, Ry where Rx and Ry are members of [[machineRegs]]. */
  def isRegRegMove(insn: T86Insn): Boolean

  /** Size of register (used for spilling) */
  def regSize: Long

  def getOperandReadWriteDefUse(op: Operand): DefUse = getOperandReadDefUse(op) ++ getOperandWriteDefUse(op)

  def remapRegistersInOperand(operand: Operand, regMap: RegMap): Operand

  def getInsnDefUse(insn: T86Insn): DefUse = insn match {
    case NullaryT86Insn(RET) => DefUse(Set.empty, calleeSaveRegs ++ returnValueRegs)
    case NullaryT86Insn(op) => DefUse.empty

    case UnaryT86Insn(CALL, operand0) => getOperandReadDefUse(operand0) ++ DefUse(callerSaveRegs ++ returnValueRegs, Set.empty)
    case UnaryT86Insn(JMP | PUSH | FPUSH | POP | FPOP | PUTCHAR | PUTNUM | _: CondJmpOp, operand0) => getOperandReadDefUse(operand0)
    case UnaryT86Insn(POP | FPOP | GETCHAR, operand0) => getOperandWriteDefUse(operand0)
    case UnaryT86Insn(INC | DEC | NOT | NEG, operand0) => getOperandReadWriteDefUse(operand0)

    case BinaryT86Insn(CMP | FCMP, operand0, operand1) => getOperandReadDefUse(operand0) ++ getOperandReadDefUse(operand1)
    case BinaryT86Insn(MOV | LEA | EXT | NRW, operand0, operand1) => getOperandWriteDefUse(operand0) ++ getOperandReadDefUse(operand1)
    case BinaryT86Insn(ADD | SUB | MUL | DIV | MOD | IMUL | IDIV | AND | OR | XOR | LSH | RSH | FADD | FSUB | FMUL | FDIV | LOOP, operand0, operand1) =>
      getOperandReadWriteDefUse(operand0) ++ getOperandReadDefUse(operand1)
  }

  def getBasicBlockDefUse(bb: T86BasicBlock): DefUse =
    bb.insns.reverse.foldLeft(DefUse.empty)((prev, insn) => {
      val cur = getInsnDefUse(insn)
      DefUse(prev.defines ++ cur.defines, (prev.uses -- cur.defines) ++ cur.uses)
    })

  def remapRegistersInInsn(insn: T86Insn, regMap: RegMap): T86Insn = insn match {
    case insn: NullaryT86Insn => insn
    case UnaryT86Insn(op, operand0) => UnaryT86Insn(op, remapRegistersInOperand(operand0, regMap))
    case BinaryT86Insn(op, operand0, operand1) => BinaryT86Insn(op, remapRegistersInOperand(operand0, regMap), remapRegistersInOperand(operand1, regMap))
  }

  def remapRegistersInFun(fun: T86Fun, regMap: RegMap): Unit = {
    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.map({
        case insn: T86Insn => remapRegistersInInsn(insn, regMap)
        case elem => elem
      })
    })
  }
}

trait T86RegRegisterAllocator extends T86GenericRegisterAllocator[Operand.Reg] {
  /** General purpose registers available for allocation. */
  override def machineRegs: Set[Operand.Reg] = T86Utils.machineRegs

  override def ignoredRegs: Set[Operand.Reg] = T86Utils.specialRegs

  override def returnValueRegs: Set[Operand.Reg] = Set(T86Utils.returnValueReg)

  /** Registers which should be live during any function call. A subset of [[machineRegs]]. */
  override def callerSaveRegs: Set[Operand.Reg] = Set.empty

  /** Registers which should be live inside whole function body. A subset of [[machineRegs]]. */
  override def calleeSaveRegs: Set[Operand.Reg] = T86Utils.calleeSaveRegs

  /** Returns what the operand defines and uses when it is used as write target (no read). Defines and uses are subset of [[machineRegs]]. */
  override def getOperandWriteDefUse(op: Operand): DefUse = (op match {
    case reg: Operand.Reg => DefUse(Set(reg), Set.empty)

    case Operand.MemImm(addr) => DefUse.empty
    case Operand.MemReg(addrReg) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegImm(addrReg, addrOffset) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegReg(addrReg, addrReg2) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegScaled(addrReg, addrScale) => DefUse(Set.empty, Set(addrReg))
    case Operand.MemRegImmReg(addrReg, addrOffset, addrReg2) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegImmRegScaled(addrReg, addrOffset, addrReg2, addrScale) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => DefUse(Set.empty, Set(addrReg, addrScaledReg))

    case freg: Operand.FReg => DefUse.empty

    case op => throw new IllegalArgumentException(s"Operand $op cannot be used as a destination.")
  }).withoutIgnoredRegs

  /** Returns what the operand defines and uses when it is used as read target. Defines and uses are subset of [[machineRegs]]. */
  override def getOperandReadDefUse(op: Operand): DefUse = (op match {
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
    case Operand.MemRegImmRegScaled(addrReg, addrOffset, addrReg2, addrScale) => DefUse(Set.empty, Set(addrReg, addrReg2))
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => DefUse(Set.empty, Set(addrReg, addrScaledReg))

    case freg: Operand.FReg => DefUse.empty
  }).withoutIgnoredRegs

  /** Returns true for MOV Rx, Ry where Rx and Ry are members of [[machineRegs]]. */
  override def isRegRegMove(insn: T86Insn): Boolean = insn match {
    case BinaryT86Insn(MOV, r1: Operand.Reg, r2: Operand.Reg) if !ignoredRegs.contains(r1) && !ignoredRegs.contains(r2) => true
    case _ => false
  }

  override def remapRegistersInOperand(op: Operand, regMap: RegMap): Operand = op match {
    case _: Operand.Imm | _: Operand.Label | _: Operand.MemImm | _: Operand.FImm => op
    case reg: Operand.Reg => regMap(reg)

    case Operand.RegImm(reg, offset) => Operand.RegImm(regMap(reg), offset)
    case Operand.RegReg(reg, reg2) => Operand.RegReg(regMap(reg), reg2)
    case Operand.RegScaled(reg, scale) => Operand.RegScaled(regMap(reg), scale)
    case Operand.RegImmReg(reg, offset, reg2) => Operand.RegImmReg(regMap(reg), offset, regMap(reg2))
    case Operand.RegRegScaled(reg, scaledReg, scale) => Operand.RegRegScaled(regMap(reg), regMap(scaledReg), scale)
    case Operand.RegImmRegScaled(reg, offset, scaledReg, scale) => Operand.RegImmRegScaled(regMap(reg), offset, regMap(scaledReg), scale)

    case Operand.MemReg(addrReg) => Operand.MemReg(regMap(addrReg))
    case Operand.MemRegImm(addrReg, addrOffset) => Operand.MemRegImm(regMap(addrReg), addrOffset)
    case Operand.MemRegReg(addrReg, addrReg2) => Operand.MemRegReg(regMap(addrReg), regMap(addrReg2))
    case Operand.MemRegScaled(addrReg, addrScale) => Operand.MemRegScaled(regMap(addrReg), addrScale)
    case Operand.MemRegImmReg(addrReg, addrOffset, addrReg2) => Operand.MemRegImmReg(regMap(addrReg), addrOffset, regMap(addrReg2))
    case Operand.MemRegImmRegScaled(addrReg, addrOffset, addrReg2, addrScale) => Operand.MemRegImmRegScaled(regMap(addrReg), addrOffset, regMap(addrReg2), addrScale)
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => Operand.MemRegRegScaled(regMap(addrReg), regMap(addrScaledReg), addrScale)

    case freg: Operand.FReg => freg
  }

  override def regSize: Long = 1
}

trait T86FRegRegisterAllocator extends T86GenericRegisterAllocator[Operand.FReg] {
  /** General purpose registers available for allocation. */
  override def machineRegs: Set[Operand.FReg] = T86Utils.machineFRegs

  override def ignoredRegs: Set[Operand.FReg] = Set.empty

  override def returnValueRegs: Set[Operand.FReg] = Set.empty

  /** Registers which should be live during any function call. A subset of [[machineRegs]]. */
  override def callerSaveRegs: Set[Operand.FReg] = T86Utils.callerSaveFRegs

  /** Registers which should be live inside whole function body. A subset of [[machineRegs]]. */
  override def calleeSaveRegs: Set[Operand.FReg] = Set.empty

  /** Returns what the operand defines and uses when it is used as write target (no read). Defines and uses are subset of [[machineRegs]]. */
  override def getOperandWriteDefUse(op: Operand): DefUse = (op match {
    case op: Operand.Reg => DefUse.empty

    case Operand.MemImm(addr) => DefUse.empty
    case Operand.MemReg(addrReg) => DefUse.empty
    case Operand.MemRegImm(addrReg, addrOffset) => DefUse.empty
    case Operand.MemRegReg(addrReg, addrReg2) => DefUse.empty
    case Operand.MemRegScaled(addrReg, addrScale) => DefUse.empty
    case Operand.MemRegImmReg(addrReg, addrOffset, addrReg2) => DefUse.empty
    case Operand.MemRegImmRegScaled(addrReg, addrOffset, addrScaledReg, addrScale) => DefUse.empty
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => DefUse.empty

    case freg: Operand.FReg => DefUse(Set(freg), Set.empty)

    case op => throw new IllegalArgumentException(s"Operand $op cannot be used as a destination.")
  }).withoutIgnoredRegs

  /** Returns what the operand defines and uses when it is used as read target. Defines and uses are subset of [[machineRegs]]. */
  override def getOperandReadDefUse(op: Operand): DefUse = (op match {
    case _: Operand.Imm | _: Operand.Label | _: Operand.MemImm | _: Operand.FImm => DefUse.empty
    case reg: Operand.Reg => DefUse.empty

    case Operand.RegImm(reg, offset) => DefUse.empty
    case Operand.RegReg(reg, reg2) => DefUse.empty
    case Operand.RegScaled(reg, scale) => DefUse.empty
    case Operand.RegImmReg(reg, offset, reg2) => DefUse.empty
    case Operand.RegRegScaled(reg, scaledReg, scale) => DefUse.empty
    case Operand.RegImmRegScaled(reg, offset, scaledReg, scale) => DefUse.empty

    case Operand.MemReg(addrReg) => DefUse.empty
    case Operand.MemRegImm(addrReg, addrOffset) => DefUse.empty
    case Operand.MemRegReg(addrReg, addrReg2) => DefUse.empty
    case Operand.MemRegScaled(addrReg, addrScale) => DefUse.empty
    case Operand.MemRegImmReg(addrReg, addrOffset, addrReg2) => DefUse.empty
    case Operand.MemRegImmRegScaled(addrReg, addrOffset, addrScaledReg, addrScale) => DefUse.empty
    case Operand.MemRegRegScaled(addrReg, addrScaledReg, addrScale) => DefUse.empty

    case freg: Operand.FReg => DefUse(Set.empty, Set(freg))
  }).withoutIgnoredRegs

  /** Returns true for MOV Rx, Ry where Rx and Ry are members of [[machineRegs]]. */
  override def isRegRegMove(insn: T86Insn): Boolean = insn match {
    case BinaryT86Insn(MOV, r1: Operand.FReg, r2: Operand.FReg) if !ignoredRegs.contains(r1) && !ignoredRegs.contains(r2) => true
    case _ => false
  }

  override def remapRegistersInOperand(op: Operand, regMap: RegMap): Operand = op match {
    case freg: Operand.FReg => regMap(freg)
    case _ => op
  }

  override def regSize: Long = 1
}

/**
 * The purpose of a register allocator is to map virtual registers to real machine registers and backup caller- and callee- saved registers.
 * Some registers can overflow available registers, in that case it should generate code to spill them into memory.
 */
abstract class T86RegisterAllocator extends ProgramTransform[T86Program] {
  def transformProgram(program: T86Program): Unit
}

object T86RegisterAllocator {
  def apply(): T86RegisterAllocator = new GraphColoringRegisterAllocator()
}