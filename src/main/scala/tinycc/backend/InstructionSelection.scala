package tinycc.backend

import tinycc.common.ir.{BinaryArithInsn, Insn}

trait AsmEmitter

trait RewriteRule[R] extends (Insn => (T86Context => R)) {
}

trait T86Context {

}

object IAddRule extends RewriteRule {
  override def matches(insn: Insn): Boolean =
    insn.isInstanceOf[BinaryArithInsn]

  def emit
}

// rewriterule
// matches

// MemoryImm ->  ^^ { case ()
// Imm ->
// Operand -> MemoryImm | Imm
// Reg -> IAdd(Operand, Operand) ^^ { case (left, right) => new ADD(left, right) }