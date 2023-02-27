package tinycc.backend.t86

sealed trait Operand extends Product with Serializable

object Operand {
  case class Imm(value: Long) extends Operand {
    override def toString: String = s"$value"

    def mem: MemImm = MemImm(value)
  }

  case class Label(symbol: Symbol) extends Operand {
    override def toString: String = s"${symbol.name}"
  }

  sealed trait Reg extends Operand {
    def mem: MemReg = MemReg(this)
  }

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

    def mem: MemRegImm = MemRegImm(reg, offset)
  }

  case class RegReg(reg: Reg, reg2: Reg) extends Operand {
    override def toString: String = s"$reg + $reg2"

    def mem: MemRegReg = MemRegReg(reg, reg2)
  }

  case class RegScaled(reg: Reg, scale: Long) extends Operand {
    override def toString: String = s"$reg * $scale"

    def mem: MemRegScaled = MemRegScaled(reg, scale)
  }

  case class RegImmReg(reg: Reg, offset: Long, reg2: Reg) extends Operand {
    override def toString: String = s"$reg + $offset + $reg2"

    def mem: MemRegImmReg = MemRegImmReg(reg, offset, reg2)
  }

  case class RegRegScaled(reg: Reg, scaledReg: Reg, scale: Long) extends Operand {
    override def toString: String = s"$reg + $scaledReg * $scale"

    def mem: MemRegRegScaled = MemRegRegScaled(reg, scaledReg, scale)
  }

  case class RegImmRegScaled(reg: Reg, offset: Long, scaledReg: Reg, scale: Long) extends Operand {
    override def toString: String = s"$reg + $offset + $scaledReg * $scale"

    def mem: MemRegImmRegScaled = MemRegImmRegScaled(reg, offset, scaledReg, scale)
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

  case class BasicFReg(index: Long) extends FReg {
    override def toString: String = s"FR$index"
  }
}

sealed trait T86ProgramElement extends Product with Serializable

case class T86Comment(value: String) extends T86ProgramElement

case class T86SectionLabel(symbol: Symbol) extends T86ProgramElement

object T86SectionLabel {
  def apply(name: String): T86SectionLabel = T86SectionLabel(Symbol(name))
}

case class T86Label(symbol: Symbol) extends T86ProgramElement {
  def toOperand: Operand.Label = Operand.Label(symbol)
}

object T86Label {
  def apply(name: String): T86Label = T86Label(Symbol(name))
}

sealed trait T86Insn extends T86ProgramElement {
  def op: T86Opcode

  def validate(): Unit = {}

  def operands: Seq[Operand] = Seq.empty
}

object T86Insn {
  def apply(op: T86Opcode): NullaryT86Insn = NullaryT86Insn(op)

  def apply(op: T86Opcode, operand: Operand): UnaryT86Insn = UnaryT86Insn(op, operand)

  def apply(op: T86Opcode, left: Operand, right: Operand): BinaryT86Insn = BinaryT86Insn(op, left, right)
}

case class NullaryT86Insn(op: T86Opcode) extends T86Insn

case class UnaryT86Insn(op: T86Opcode, operand0: Operand) extends T86Insn {
  override def operands: Seq[Operand] = Seq(operand0)
}

case class BinaryT86Insn(op: T86Opcode, operand0: Operand, operand1: Operand) extends T86Insn {
  override def operands: Seq[Operand] = Seq(operand0, operand1)
}