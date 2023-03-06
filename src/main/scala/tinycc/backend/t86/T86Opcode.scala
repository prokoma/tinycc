package tinycc.backend.t86

sealed trait T86Opcode extends Product with Serializable

object T86Opcode {

  sealed trait NullaryOp extends T86Opcode

  case object NOP extends NullaryOp

  case object HALT extends NullaryOp

  case object DBG extends NullaryOp

  case object BREAK extends NullaryOp

  case object CLF extends NullaryOp

  case object RET extends NullaryOp

  sealed trait UnaryOp extends T86Opcode

  case object NOT extends UnaryOp

  case object NEG extends UnaryOp

  case object INC extends UnaryOp

  case object DEC extends UnaryOp

  case object JMP extends UnaryOp

  case object CALL extends UnaryOp

  case object PUSH extends UnaryOp

  case object FPUSH extends UnaryOp

  case object POP extends UnaryOp

  case object FPOP extends UnaryOp

  case object PUTCHAR extends UnaryOp

  case object PUTNUM extends UnaryOp

  case object GETCHAR extends UnaryOp

  sealed trait CondJmpOp extends UnaryOp {
    def neg: CondJmpOp
  }

  case object JZ extends CondJmpOp {
    override def neg: CondJmpOp = JNZ
  }

  case object JNZ extends CondJmpOp {
    override def neg: CondJmpOp = JZ
  }

  case object JE extends CondJmpOp {
    override def neg: CondJmpOp = JNE
  }

  case object JNE extends CondJmpOp {
    override def neg: CondJmpOp = JE
  }

  case object JG extends CondJmpOp {
    override def neg: CondJmpOp = JLE
  }

  case object JGE extends CondJmpOp {
    override def neg: CondJmpOp = JL
  }

  case object JL extends CondJmpOp {
    override def neg: CondJmpOp = JGE
  }

  case object JLE extends CondJmpOp {
    override def neg: CondJmpOp = JG
  }

  case object JA extends CondJmpOp {
    override def neg: CondJmpOp = JBE
  }

  case object JAE extends CondJmpOp {
    override def neg: CondJmpOp = JB
  }

  case object JB extends CondJmpOp {
    override def neg: CondJmpOp = JAE
  }

  case object JBE extends CondJmpOp {
    override def neg: CondJmpOp = JA
  }

  case object JO extends CondJmpOp {
    override def neg: CondJmpOp = JNO
  }

  case object JNO extends CondJmpOp {
    override def neg: CondJmpOp = JO
  }

  case object JS extends CondJmpOp {
    override def neg: CondJmpOp = JNS
  }

  case object JNS extends CondJmpOp {
    override def neg: CondJmpOp = JS
  }

  sealed trait BinaryOp extends T86Opcode

  case object MOV extends BinaryOp

  case object LEA extends BinaryOp

  case object ADD extends BinaryOp

  case object SUB extends BinaryOp

  case object MUL extends BinaryOp

  case object DIV extends BinaryOp

  case object MOD extends BinaryOp

  case object IMUL extends BinaryOp

  case object IDIV extends BinaryOp

  case object AND extends BinaryOp

  case object OR extends BinaryOp

  case object XOR extends BinaryOp

  case object LSH extends BinaryOp

  case object RSH extends BinaryOp

  case object FADD extends BinaryOp

  case object FSUB extends BinaryOp

  case object FMUL extends BinaryOp

  case object FDIV extends BinaryOp

  case object CMP extends BinaryOp

  case object FCMP extends BinaryOp

  case object EXT extends BinaryOp

  case object NRW extends BinaryOp

  case object LOOP extends BinaryOp
}