package tinycc.common.ir

sealed trait IrOpcode extends Product with Serializable

object IrOpcode {
  // Prefixes:
  // - I: integer (signed or unsigned)
  // - U: unsigned integer
  // - S: signed integer
  // - F: double

  sealed trait BinaryOp extends IrOpcode

  sealed trait BinaryArithOp extends BinaryOp {
    def insnTy: IrTy
  }

  sealed trait IBinaryArithOp extends BinaryArithOp {
    override def insnTy: IrTy = IrTy.Int64Ty
  }

  case object IAdd extends IBinaryArithOp {
    override def toString: String = "add"
  }

  case object ISub extends IBinaryArithOp {
    override def toString: String = "sub"
  }

  case object IAnd extends IBinaryArithOp {
    override def toString: String = "iand"
  }

  case object IOr extends IBinaryArithOp {
    override def toString: String = "ior"
  }

  case object IXor extends IBinaryArithOp {
    override def toString: String = "ixor"
  }

  /** Arithmetic shift left */
  case object IShl extends IBinaryArithOp {
    override def toString: String = "ishl"
  }

  /** Arithmetic shift right */
  case object IShr extends IBinaryArithOp {
    override def toString: String = "ishr"
  }

  case object UMul extends IBinaryArithOp {
    override def toString: String = "umul"
  }

  case object UDiv extends IBinaryArithOp {
    override def toString: String = "udiv"
  }

  case object SMul extends IBinaryArithOp {
    override def toString: String = "smul"
  }

  case object SDiv extends IBinaryArithOp {
    override def toString: String = "sdiv"
  }

  sealed trait FBinaryArithOp extends BinaryArithOp {
    override def insnTy: IrTy = IrTy.DoubleTy
  }

  case object FAdd extends FBinaryArithOp {
    override def toString: String = "fadd"
  }

  case object FSub extends FBinaryArithOp {
    override def toString: String = "fsub"
  }

  case object FMul extends FBinaryArithOp {
    override def toString: String = "fmul"
  }

  case object FDiv extends FBinaryArithOp {
    override def toString: String = "fdiv"
  }

  sealed trait CmpOp extends BinaryOp {
    def operandTy: IrTy
  }

  sealed trait ICmpOp extends CmpOp {
    def operandTy: IrTy = IrTy.Int64Ty
  }

  case object CmpIEq extends ICmpOp {
    override def toString: String = "cmpieq"
  }

  case object CmpINe extends ICmpOp {
    override def toString: String = "cmpine"
  }

  case object CmpULt extends ICmpOp {
    override def toString: String = "cmpult"
  }

  case object CmpULe extends ICmpOp {
    override def toString: String = "cmpule"
  }

  case object CmpUGt extends ICmpOp {
    override def toString: String = "cmpugt"
  }

  case object CmpUGe extends ICmpOp {
    override def toString: String = "cmpuge"
  }

  case object CmpSLt extends ICmpOp {
    override def toString: String = "cmpslt"
  }

  case object CmpSLe extends ICmpOp {
    override def toString: String = "cmpsle"
  }

  case object CmpSGt extends ICmpOp {
    override def toString: String = "cmpsgt"
  }

  case object CmpSGe extends ICmpOp {
    override def toString: String = "cmpsge"
  }

  sealed trait FCmpOp extends CmpOp {
    def operandTy: IrTy = IrTy.DoubleTy
  }

  case object CmpFEq extends FCmpOp {
    override def toString: String = "cmpfeq"
  }

  case object CmpFNe extends FCmpOp {
    override def toString: String = "cmpfne"
  }

  case object CmpFLt extends FCmpOp {
    override def toString: String = "cmpflt"
  }

  case object CmpFLe extends FCmpOp {
    override def toString: String = "cmpfle"
  }

  case object CmpFGt extends FCmpOp {
    override def toString: String = "cmpfgt"
  }

  case object CmpFGe extends FCmpOp {
    override def toString: String = "cmpfge"
  }

  sealed trait UnaryOp extends IrOpcode

  case object AllocL extends IrOpcode {
    override def toString: String = "allocl"
  }

  case object AllocG extends IrOpcode {
    override def toString: String = "allocg"
  }

  case object Load extends IrOpcode {
    override def toString: String = "load"
  }

  case object LoadArg extends IrOpcode {
    override def toString: String = "loadarg"
  }

  case object Store extends IrOpcode {
    override def toString: String = "store"
  }

  sealed trait Imm extends IrOpcode

  case object IImm extends Imm {
    override def toString: String = "iimm"
  }

  case object FImm extends Imm {
    override def toString: String = "fimm"
  }

  case object GetElementPtr extends IrOpcode {
    override def toString: String = "getelementptr"
  }

  case object GetFunPtr extends IrOpcode {
    override def toString: String = "getfunptr"
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

  case object Phi extends IrOpcode {
    override def toString: String = "phi"
  }

  sealed trait IrTerminatorOp extends IrOpcode

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

  sealed trait CastOp extends IrOpcode {
    def operandTy: IrTy

    def resultTy: IrTy
  }

  case object BitcastInt64ToDouble extends CastOp {
    override def toString: String = "bitcastint64todouble"

    override def operandTy: IrTy = IrTy.Int64Ty

    override def resultTy: IrTy = IrTy.DoubleTy
  }

  case object SInt64ToDouble extends CastOp {
    override def toString: String = "sint64todouble"

    override def operandTy: IrTy = IrTy.Int64Ty

    override def resultTy: IrTy = IrTy.DoubleTy
  }

//  case object UInt64ToDouble extends CastOp {
//    override def toString: String = "uint64todouble"
//
//    override def resultTy: IrTy = IrTy.DoubleTy
//  }

  case object BitcastDoubleToInt64 extends CastOp {
    override def toString: String = "bitcastdoubletoint64"

    override def operandTy: IrTy = IrTy.DoubleTy

    override def resultTy: IrTy = IrTy.Int64Ty
  }

  case object DoubleToSInt64 extends CastOp {
    override def toString: String = "doubletosint64"

    override def operandTy: IrTy = IrTy.DoubleTy

    override def resultTy: IrTy = IrTy.Int64Ty
  }

//  case object DoubleToUInt64 extends CastOp {
//    override def toString: String = "doubletouint64"
//
//    override def resultTy: IrTy = IrTy.Int64Ty
//  }
}