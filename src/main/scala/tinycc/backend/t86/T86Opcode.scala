package tinycc.backend.t86

sealed trait T86Opcode extends Product with Serializable

object T86Opcode {
  case object MOV extends T86Opcode

  case object LEA extends T86Opcode

  case object NOP extends T86Opcode

  case object HALT extends T86Opcode

  case object DBG extends T86Opcode

  case object BREAK extends T86Opcode

  case object ADD extends T86Opcode

  case object SUB extends T86Opcode

  case object INC extends T86Opcode

  case object DEC extends T86Opcode

  case object NEG extends T86Opcode

  case object MUL extends T86Opcode

  case object DIV extends T86Opcode

  case object MOD extends T86Opcode

  case object IMUL extends T86Opcode

  case object IDIV extends T86Opcode

  case object AND extends T86Opcode

  case object OR extends T86Opcode

  case object XOR extends T86Opcode

  case object NOT extends T86Opcode

  case object LSH extends T86Opcode

  case object RSH extends T86Opcode

  case object CLF extends T86Opcode

  case object CMP extends T86Opcode

  case object FCMP extends T86Opcode

  case object JMP extends T86Opcode

  case object LOOP extends T86Opcode

  sealed trait CondJmpOp extends T86Opcode {
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

  case object CALL extends T86Opcode

  case object RET extends T86Opcode

  case object PUSH extends T86Opcode

  case object FPUSH extends T86Opcode

  case object POP extends T86Opcode

  case object FPOP extends T86Opcode

  case object PUTCHAR extends T86Opcode

  case object PUTNUM extends T86Opcode

  case object GETCHAR extends T86Opcode

  case object FADD extends T86Opcode

  case object FSUB extends T86Opcode

  case object FMUL extends T86Opcode

  case object FDIV extends T86Opcode

  case object EXT extends T86Opcode

  case object NRW extends T86Opcode
}