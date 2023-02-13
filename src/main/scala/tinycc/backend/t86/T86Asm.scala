package tinycc.backend.t86

import tinycc.backend.t86.Operand.{FReg, Reg}

sealed trait Operand extends Product with Serializable

object Operand {
  case class Imm(value: Long) extends Operand {
    override def toString: String = s"$value"
  }

  sealed trait Reg extends Operand

  case class BasicReg(index: Long) extends Reg {
    override def toString: String = s"R$index"
  }

  case object BP extends Reg {
    override def toString: String = "BP"
  }

  case object SP extends Reg {
    override def toString: String = "SP"
  }

  case object IP extends Reg {
    override def toString: String = "IP"
  }

  case class RegImm(reg: Reg, offset: Long) extends Operand {
    override def toString: String = s"$reg + $offset"
  }

  case class RegReg(reg: Reg, reg2: Reg) extends Operand {
    override def toString: String = s"$reg + $reg2"
  }

  case class RegScaled(reg: Reg, scale: Long) extends Operand {
    override def toString: String = s"$reg * $scale"
  }

  case class RegImmReg(reg: Reg, offset: Long, reg2: Reg) extends Operand {
    override def toString: String = s"$reg + $offset + $reg2"
  }

  case class RegRegScaled(reg: Reg, scaledReg: Reg, scale: Long) extends Operand {
    override def toString: String = s"$reg + $scaledReg * $scale"
  }

  case class RegImmRegScaled(reg: Reg, offset: Long, scaledReg: Reg, scale: Long) extends Operand {
    override def toString: String = s"$reg + $offset + $scaledReg * $scale"
  }

  case class MemImm(addr: Long) extends Operand {
    override def toString: String = s"[$addr]"
  }

  case class MemReg(addrReg: Reg) extends Operand {
    override def toString: String = s"[$addrReg]"
  }

  case class MemRegImm(addrReg: Reg, addrOffset: Long) extends Operand {
    override def toString: String = s"[$addrReg + $addrOffset]"
  }

  case class MemRegReg(addrReg: Reg, addrReg2: Reg) extends Operand {
    override def toString: String = s"[$addrReg + $addrReg2]"
  }

  case class MemRegScaled(addrReg: Reg, addrScale: Long) extends Operand {
    override def toString: String = s"[$addrReg * $addrScale]"
  }

  case class MemRegImmReg(addrReg: Reg, addrOffset: Long, addrReg2: Reg) extends Operand {
    override def toString: String = s"[$addrReg + $addrOffset + $addrReg2]"
  }

  case class MemRegRegScaled(addrReg: Reg, addrScaledReg: Reg, addrScale: Long) extends Operand {
    override def toString: String = s"[$addrReg + $addrScaledReg * $addrScale]"
  }

  case class MemRegImmRegScaled(addrReg: Reg, addrOffset: Long, addrScaledReg: Reg, addrScale: Long) {
    override def toString: String = s"[$addrReg + $addrOffset + $addrScaledReg * $addrScale]"
  }

  case class FImm(value: Double) extends Operand {
    override def toString: String = s"$value"
  }

  sealed trait FReg extends Operand

  case class BasicFReg(index: Long) extends Operand {
    override def toString: String = s"FR$index"
  }
}

sealed trait T86Insn {
  def validate(): Unit = {}

  def operands: Seq[Operand] = Seq.empty
}

case class MOV(dest: Operand, value: Operand) extends T86Insn {
  override def operands: Seq[Operand] = Seq(dest, value)
}

trait BinArithInsn extends T86Insn {
  def reg: Reg

  def value: Operand

  override def operands: Seq[Operand] = Seq(reg, value)
}

case class MOD(reg: Reg, value: Operand) extends BinArithInsn
case class ADD(reg: Reg, value: Operand) extends BinArithInsn
case class SUB(reg: Reg, value: Operand) extends BinArithInsn
case class MUL(reg: Reg, value: Operand) extends BinArithInsn
case class DIV(reg: Reg, value: Operand) extends BinArithInsn
case class IMUL(reg: Reg, value: Operand) extends BinArithInsn
case class IDIV(reg: Reg, value: Operand) extends BinArithInsn
case class AND(reg: Reg, value: Operand) extends BinArithInsn
case class OR(reg: Reg, value: Operand) extends BinArithInsn
case class XOR(reg: Reg, value: Operand) extends BinArithInsn
case class LSH(reg: Reg, value: Operand) extends BinArithInsn
case class RSH(reg: Reg, value: Operand) extends BinArithInsn

trait FloatBinArithInsn extends T86Insn {
  def reg: FReg

  def value: Operand

  override def operands: Seq[Operand] = Seq(reg, value)
}

case class FADD(reg: FReg, value: Operand) extends FloatBinArithInsn
case class FSUB(reg: FReg, value: Operand) extends FloatBinArithInsn
case class FMUL(reg: FReg, value: Operand) extends FloatBinArithInsn
case class FDIV(reg: FReg, value: Operand) extends FloatBinArithInsn

trait UnaryArithInsn extends T86Insn {
  def reg: Reg

  override def operands: Seq[Operand] = Seq(reg)
}

case class INC(reg: Reg) extends UnaryArithInsn
case class DEC(reg: Reg) extends UnaryArithInsn
case class NEG(reg: Reg) extends UnaryArithInsn
case class NOT(reg: Reg) extends UnaryArithInsn

case object NOP extends T86Insn

case object HALT extends T86Insn

case class CMP(reg: Reg, value: Operand) extends T86Insn
case class FCMP(reg: FReg, value: Operand) extends T86Insn

case class JMP() extends T86Insn

trait CondJmpInsn extends T86Insn {
  def addr: Operand
}

case class JZ(addr: Operand) extends CondJmpInsn
case class JNZ(addr: Operand) extends CondJmpInsn
case class JE(addr: Operand) extends CondJmpInsn
case class JNE(addr: Operand) extends CondJmpInsn
case class JG(addr: Operand) extends CondJmpInsn
case class JGE(addr: Operand) extends CondJmpInsn
case class JL(addr: Operand) extends CondJmpInsn
case class JLE(addr: Operand) extends CondJmpInsn
case class JA(addr: Operand) extends CondJmpInsn
case class JAE(addr: Operand) extends CondJmpInsn
case class JB(addr: Operand) extends CondJmpInsn
case class JBE(addr: Operand) extends CondJmpInsn
case class JO(addr: Operand) extends CondJmpInsn
case class JNO(addr: Operand) extends CondJmpInsn
case class JS(addr: Operand) extends CondJmpInsn
case class JNS(addr: Operand) extends CondJmpInsn

case class CALL(addr: Operand) extends T86Insn

case object RET extends T86Insn

case class LEA(reg: Reg, mem: Operand) extends T86Insn

case class PUTCHAR(reg: Reg) extends T86Insn

case class PUTNUM(reg: Reg) extends T86Insn

case class GETCHAR(reg: Reg) extends T86Insn