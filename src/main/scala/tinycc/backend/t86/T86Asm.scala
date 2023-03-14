package tinycc.backend.t86

import tinycc.common.ir.{BasicBlock, IrFun, IrProgram}

sealed trait Operand extends Product with Serializable

object Operand {
  case class Imm(value: Long) extends Operand {
    override def toString: String = s"$value"

    def mem: MemImm = MemImm(value)
  }

  case class Label(symbol: Symbol) extends Operand {
    override def toString: String = s".${symbol.name}"
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

  case class MemRegImmRegScaled(addrReg: Reg, addrOffset: Long, addrScaledReg: Reg, addrScale: Long) extends Operand {
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

sealed trait T86ListingElement extends Product with Serializable

case class T86Comment(value: String) extends T86ListingElement

case class T86SectionLabel(symbol: Symbol) extends T86ListingElement

sealed trait T86SpecialLabel extends T86ListingElement

object T86SpecialLabel {
  case object FunPrologueMarker extends T86SpecialLabel

  case object FunEpilogueMarker extends T86SpecialLabel

  case object CallPrologueMarker extends T86SpecialLabel

  case object CallEpilogueMarker extends T86SpecialLabel
}

object T86SectionLabel {
  def apply(name: String): T86SectionLabel = T86SectionLabel(Symbol(name))
}

case class T86Label(symbol: Symbol) extends T86ListingElement {
  def toOperand: Operand.Label = Operand.Label(symbol)
}

object T86Label {
  def apply(name: String): T86Label = T86Label(Symbol(name))
}

sealed trait T86Insn extends T86ListingElement {
  def op: T86Opcode

  def validate(): Unit = {}

  def operands: Seq[Operand] = Seq.empty

  override def toString: String =
    op.toString + (if(operands.nonEmpty) " " + operands.mkString(", ") else "")
}

object T86Insn {
  def unapply(insn: T86Insn): Option[T86Opcode] = Some(insn.op)

  def apply(op: T86Opcode.NullaryOp): NullaryT86Insn = NullaryT86Insn(op)

  def apply(op: T86Opcode.UnaryOp, operand: Operand): UnaryT86Insn = UnaryT86Insn(op, operand)

  def apply(op: T86Opcode.BinaryOp, left: Operand, right: Operand): BinaryT86Insn = BinaryT86Insn(op, left, right)
}

case class NullaryT86Insn(op: T86Opcode.NullaryOp) extends T86Insn

case class UnaryT86Insn(op: T86Opcode.UnaryOp, operand0: Operand) extends T86Insn {
  override def operands: Seq[Operand] = Seq(operand0)
}

case class BinaryT86Insn(op: T86Opcode.BinaryOp, operand0: Operand, operand1: Operand) extends T86Insn {
  override def operands: Seq[Operand] = Seq(operand0, operand1)
}

case class T86DataWord(value: Long, rep: Long = 1) extends T86ListingElement

class T86Program(var funs: IndexedSeq[T86Fun], var data: IndexedSeq[T86ListingElement], val irProgram: Option[IrProgram] = None) {
  def flatten: T86Listing = {
    val dataSection = if(data.nonEmpty)
      T86SectionLabel("data") +: data
    else
      Seq.empty
    val textSection = T86SectionLabel("text") +: funs.flatMap(_.flatten)
    dataSection ++ textSection
  }
}

class T86Fun(var basicBlocks: IndexedSeq[T86BasicBlock], var localsSize: Long = 0, var nextReg: Long = 0, var nextFreg: Long = 0, val irFun: Option[IrFun] = None) {
  def name: String = irFun.map(_.name).getOrElse("<anon>")

  def insns: Seq[T86Insn] = basicBlocks.flatMap(_.insns)

  def flatten: T86Listing = (
    Seq(T86Comment(""), T86Comment(s"=== FUNCTION $name START ==="), T86Comment("")) ++
    basicBlocks.flatMap(_.flatten) ++
    Seq(T86Comment(""), T86Comment(s"=== FUNCTION $name END ==="), T86Comment(""))
  )

  def freshReg(): Operand.Reg = {
    nextReg += 1
    Operand.BasicReg(nextReg - 1)
  }

  def freshFReg(): Operand.FReg = {
    nextFreg += 1
    Operand.BasicFReg(nextFreg - 1)
  }

  def freshLocal(size: Long): Operand.MemRegImm = {
    localsSize += size
    Operand.MemRegImm(Operand.BP, -localsSize)
  }
}

class T86BasicBlock(var body: IndexedSeq[T86ListingElement], val irBasicBlock: Option[BasicBlock] = None) {
  def name: String = irBasicBlock.map(_.uniqueName).getOrElse("<anon>")

  def insns: Seq[T86Insn] = body.collect({ case insn: T86Insn => insn })

  def insnRefs: Seq[T86InsnRef] = body.zipWithIndex.collect({ case (_: T86Insn, index) => T86InsnRef(this, index) })

  def insnsWithRefs: Seq[(T86Insn, T86InsnRef)] = body.zipWithIndex.collect({ case (insn: T86Insn, index) => (insn, T86InsnRef(this, index)) })

  def flatten: T86Listing = body
}

case class T86InsnRef(bb: T86BasicBlock, index: Int) {
  def apply(): T86Insn = bb.body(index).asInstanceOf[T86Insn]

  override def toString: String = s"T86InsnRef(${apply()} at #$index in ${bb.name})"
}