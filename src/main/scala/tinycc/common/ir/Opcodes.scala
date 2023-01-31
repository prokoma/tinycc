package tinycc.common.ir

sealed trait IrOpcode

// Prefixes:
// - I: integer (signed or unsigned)
// - U: unsigned integer
// - S: signed integer
// - F: double

sealed trait IrBinaryOp extends IrOpcode

case object IAdd extends IrBinaryOp {
  override def toString: String = "add"
}

case object ISub extends IrBinaryOp {
  override def toString: String = "sub"
}

case object IAnd extends IrBinaryOp {
  override def toString: String = "iand"
}

case object IOr extends IrBinaryOp {
  override def toString: String = "ior"
}

case object IXor extends IrBinaryOp {
  override def toString: String = "ixor"
}

case object IShl extends IrBinaryOp {
  override def toString: String = "ishl"
}

case object IShr extends IrBinaryOp {
  override def toString: String = "ishr"
}

case object UMul extends IrBinaryOp {
  override def toString: String = "umul"
}

case object UDiv extends IrBinaryOp {
  override def toString: String = "udiv"
}

case object SMul extends IrBinaryOp {
  override def toString: String = "smul"
}

case object SDiv extends IrBinaryOp {
  override def toString: String = "sdiv"
}

case object FAdd extends IrBinaryOp {
  override def toString: String = "fadd"
}

case object FSub extends IrBinaryOp {
  override def toString: String = "fsub"
}

case object FMul extends IrBinaryOp {
  override def toString: String = "fmul"
}

case object FDiv extends IrBinaryOp {
  override def toString: String = "fdiv"
}

sealed trait IrBinaryPred extends IrOpcode

case object CmpIEq extends IrBinaryPred {
  override def toString: String = "cmp.ieq"
}

case object CmpINe extends IrBinaryPred {
  override def toString: String = "cmp.ine"
}

case object CmpULt extends IrBinaryPred {
  override def toString: String = "cmp.ult"
}

case object CmpULe extends IrBinaryPred {
  override def toString: String = "cmp.ule"
}

case object CmpUGt extends IrBinaryPred {
  override def toString: String = "cmp.ugt"
}

case object CmpUGe extends IrBinaryPred {
  override def toString: String = "cmp.uge"
}

case object CmpSLt extends IrBinaryPred {
  override def toString: String = "cmp.slt"
}

case object CmpSLe extends IrBinaryPred {
  override def toString: String = "cmp.sle"
}

case object CmpSGt extends IrBinaryPred {
  override def toString: String = "cmp.sgt"
}

case object CmpSGe extends IrBinaryPred {
  override def toString: String = "cmp.sge"
}

case object CmpFEq extends IrBinaryPred {
  override def toString: String = "cmp.feq"
}

case object CmpFNe extends IrBinaryPred {
  override def toString: String = "cmp.fne"
}

case object CmpFLt extends IrBinaryPred {
  override def toString: String = "cmp.flt"
}

case object CmpFLe extends IrBinaryPred {
  override def toString: String = "cmp.fle"
}

case object CmpFGt extends IrBinaryPred {
  override def toString: String = "cmp.fgt"
}

case object CmpFGe extends IrBinaryPred {
  override def toString: String = "cmp.fge"
}

sealed trait IrUnaryOp extends IrOpcode

case object INeg extends IrUnaryOp {
  override def toString: String = "ineg"
}

case object AllocA extends IrOpcode {
  override def toString: String = "alloca"
}

case object AllocG extends IrOpcode {
  override def toString: String = "allocg"
}

case object Load extends IrOpcode {
  override def toString: String = "load"
}

case object Store extends IrOpcode {
  override def toString: String = "store"
}

case object Imm extends IrOpcode {
  override def toString: String = "imm"
}

case object GetElementPtr extends IrOpcode {
  override def toString: String = "getelementptr"
}

case object GetFunPtr extends IrOpcode {
  override def toString: String = "getfunptr"
}

case object GetArgPtr extends IrOpcode {
  override def toString: String = "getargptr"
}

case object Call extends IrOpcode {
  override def toString: String = "call"
}

case object CallPtr extends IrOpcode {
  override def toString: String = "callptr"
}

case object PutChar extends IrOpcode {
  override def toString: String = "putchar"
}

case object PutNum extends IrOpcode {
  override def toString: String = "putnum"
}

case object GetChar extends IrOpcode {
  override def toString: String = "getchar"
}

sealed trait IrTerminatorOp extends IrOpcode {
  override def toString: String = "ineg"
}

case object Ret extends IrTerminatorOp {
  override def toString: String = "ret"
}

case object RetVoid extends IrTerminatorOp {
  override def toString: String = "retvoid"
}

case object Halt extends IrTerminatorOp {
  override def toString: String = "halt"
}

case object Br extends IrTerminatorOp {
  override def toString: String = "br"
}

case object CondBr extends IrTerminatorOp {
  override def toString: String = "condbr"
}

case object Phi extends IrOpcode {
  override def toString: String = "phi"
}
