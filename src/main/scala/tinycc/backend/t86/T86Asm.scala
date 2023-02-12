package tinycc.backend.t86

// T86 instructions are immutable

sealed trait Operand

case class Register(index: Long) extends Operand {
  override def toString: String = s"Reg$index"
}
case class FloatRegister(index: Long) extends Operand {
  override def toString: String = s"FReg$index"
}

case class RegisterOffset(reg: Register, offset: Long) extends Operand
case class Immediate(value: Long) extends Operand
case class FloatImmediate(value: Double) extends Operand
case class Memory(inner: Operand) extends Operand

case class Mod(reg: Register, value: Operand)


case class MOV()

trait BinArithInsn {
  def reg: Register

  def value: Operand

  value match {
    case _: Register | _: RegisterOffset | _: Immediate =>
    case Memory(_: Register | _: RegisterOffset | _: Immediate) =>
    case _ => throw new AssertionError()
  }
}

case class MOD(reg: Register, value: Operand) extends BinArithInsn
case class ADD(reg: Register, value: Operand) extends BinArithInsn
case class SUB(reg: Register, value: Operand) extends BinArithInsn
case class MUL(reg: Register, value: Operand) extends BinArithInsn
case class DIV(reg: Register, value: Operand) extends BinArithInsn
case class IMUL(reg: Register, value: Operand) extends BinArithInsn
case class IDIV(reg: Register, value: Operand) extends BinArithInsn
case class AND(reg: Register, value: Operand) extends BinArithInsn
case class OR(reg: Register, value: Operand) extends BinArithInsn
case class XOR(reg: Register, value: Operand) extends BinArithInsn
case class LSH(reg: Register, value: Operand) extends BinArithInsn
case class RSH(reg: Register, value: Operand) extends BinArithInsn

trait FloatBinArithInsn {
  def reg: FloatRegister

  def value: Operand
}

case class FADD(reg: FloatRegister, value: Operand) extends FloatBinArithInsn
case class FSUB(reg: FloatRegister, value: Operand) extends FloatBinArithInsn
case class FMUL(reg: FloatRegister, value: Operand) extends FloatBinArithInsn
case class FDIV(reg: FloatRegister, value: Operand) extends FloatBinArithInsn