package tinycc.common.ir

import tinycc.common.ir.IrTy.{DoubleTy, Int64Ty}

/** An instruction, which terminates a BasicBlock. */
sealed trait TerminatorInsn extends Insn {
  def succBlockRefs: Seq[TerminatorSuccRef]

  def succBlocks: Seq[BasicBlock] = succBlockRefs.flatMap(_())

  override def resultTy: IrTy = IrTys.void

  def copy(newBlock: BasicBlock): TerminatorInsn

  override def releaseRefs(): Unit = {
    super.releaseRefs()
    succBlockRefs.foreach(_.apply(None))
  }
}

/* === Basic Instructions === */

class IImmInsn(var value: Long, val basicBlock: BasicBlock) extends Insn {
  override def op: IrOpcode = IrOpcode.IImm

  override def resultTy: IrTy = Int64Ty

  override def copy(newBlock: BasicBlock): IImmInsn = new IImmInsn(value, newBlock)
}

object IImmInsn {
  def unapply(insn: IImmInsn): Option[Long] = Some(insn.value)
}

class FImmInsn(var value: Double, val basicBlock: BasicBlock) extends Insn {
  override def op: IrOpcode = IrOpcode.FImm

  override def resultTy: IrTy = DoubleTy

  override def copy(newBlock: BasicBlock): FImmInsn = new FImmInsn(value, newBlock)
}

object FImmInsn {
  def unapply(insn: FImmInsn): Option[Double] = Some(insn.value)
}

sealed abstract class BinaryInsn(leftOpTgt: Insn, rightOpTgt: Insn, val basicBlock: BasicBlock) extends Insn {
  val leftOp: OperandRef = new OperandRef(this, Some(leftOpTgt))
  val rightOp: OperandRef = new OperandRef(this, Some(rightOpTgt))

  override def operandRefs: Seq[OperandRef] = Seq(leftOp, rightOp)
}

class BinaryArithInsn(val op: IrOpcode.BinaryArithOp, leftOpTgt: Insn, rightOpTgt: Insn, basicBlock: BasicBlock) extends BinaryInsn(leftOpTgt, rightOpTgt, basicBlock) {
  override def resultTy: IrTy = {
    assert(leftOp.get.resultTy == rightOp.get.resultTy)
    leftOp.get.resultTy
  }

  override def copy(newBlock: BasicBlock): Insn = new BinaryArithInsn(op, leftOp.get, rightOp.get, newBlock)
}

/** All compare instructions return either 0 or 1. */
class CmpInsn(val op: IrOpcode.CmpOp, leftOpTgt: Insn, rightOpTgt: Insn, basicBlock: BasicBlock) extends BinaryInsn(leftOpTgt, rightOpTgt, basicBlock) {
  override def resultTy: IrTy = IrTy.Int64Ty

  override def copy(newBlock: BasicBlock): Insn = new CmpInsn(op, leftOp.get, rightOp.get, newBlock)
}

/* === Memory Instructions === */

sealed abstract class AllocInsn(val contentTy: IrTy, val basicBlock: BasicBlock) extends Insn {
  override def resultTy: IrTy = IrTy.PtrTy
}

class AllocLInsn(contentTy: IrTy, basicBlock: BasicBlock) extends AllocInsn(contentTy, basicBlock) {
  override def op: IrOpcode = IrOpcode.AllocL

  override def copy(newBlock: BasicBlock): AllocLInsn = new AllocLInsn(contentTy, newBlock)
}

class AllocGInsn(contentTy: IrTy, val initData: Seq[Byte], basicBlock: BasicBlock) extends AllocInsn(contentTy, basicBlock) {
  override def op: IrOpcode = IrOpcode.AllocL

  override def copy(newBlock: BasicBlock): AllocGInsn = new AllocGInsn(contentTy, initData, newBlock)

  override def validate(): Unit = {
    super.validate()
    if (initData.size > contentTy.sizeBytes)
      throw new IrException(s"AllocG init data is too large (initData.size: ${initData.size}, capacity: ${contentTy.sizeBytes})")
  }
}

class LoadInsn(addrTgt: Insn, val basicBlock: BasicBlock) extends Insn {
  val addr: OperandRef = new OperandRef(this, Some(addrTgt))

  override def op: IrOpcode = IrOpcode.Load

  override def resultTy: IrTy = IrTy.Int64Ty

  override def operandRefs: Seq[OperandRef] = Seq(addr)

  override def copy(newBlock: BasicBlock): LoadInsn = new LoadInsn(addr.get, newBlock)
}

class StoreInsn(addrTgt: Insn, valueTgt: Insn, val basicBlock: BasicBlock) extends Insn {
  val addr: OperandRef = new OperandRef(this, Some(addrTgt))
  val value: OperandRef = new OperandRef(this, Some(valueTgt))

  override def op: IrOpcode = IrOpcode.Store

  override def resultTy: IrTy = IrTy.VoidTy

  override def operandRefs: Seq[OperandRef] = Seq(addr, value)

  override def copy(newBlock: BasicBlock): StoreInsn = new StoreInsn(addr.get, value.get, newBlock)
}

class GetElementPtr(baseTgt: Insn, indexTgt: Insn, val elemSizeCells: Int, val fieldOffsetCells: Int, val basicBlock: BasicBlock) extends Insn {
  val base: OperandRef = new OperandRef(this, Some(baseTgt))
  val index: OperandRef = new OperandRef(this, Some(indexTgt))

  override def op: IrOpcode = IrOpcode.GetElementPtr

  override def resultTy: IrTy = IrTy.PtrTy

  override def operandRefs: Seq[OperandRef] = Seq(base, index)

  override def copy(newBlock: BasicBlock): GetElementPtr = new GetElementPtr(base.get, index.get, elemSizeCells, fieldOffsetCells, newBlock)
}

/* === Function call instructions === */

class GetFunPtrInsn(targetFunTgt: IrFun, val basicBlock: BasicBlock) extends Insn {
  val targetFun: IrFunRef = new IrFunRef(Some(targetFunTgt))

  override def op: IrOpcode = IrOpcode.GetFunPtr

  override def resultTy: IrTy = IrTy.PtrTy

  override def copy(newBlock: BasicBlock): GetFunPtrInsn = new GetFunPtrInsn(targetFun.get, newBlock)
}

class GetArgPtrInsn(val index: Int, val basicBlock: BasicBlock) extends Insn {
  override def op: IrOpcode = IrOpcode.GetArgPtr

  override def resultTy: IrTy = IrTy.PtrTy

  override def copy(newBlock: BasicBlock): GetArgPtrInsn = new GetArgPtrInsn(index, newBlock)
}

sealed trait CallInsnBase extends Insn {
  def args: Seq[OperandRef]

  def funSig: IrFunSignature

  override def validate(): Unit = {
    super.validate()
    if (args.size != funSig.argTypes.size)
      throw new IrException(s"Wrong number of arguments in $this (expected ${funSig.argTypes.size}, ${args.size} given).")
    args.zip(funSig.argTypes).zipWithIndex.foreach({ case ((arg, sigTy), i) =>
      if (arg.isDefined && arg.get.resultTy != sigTy)
        throw new IrException(s"Wrong type of $i-th argument in $this (expected ${sigTy}, ${arg.get.resultTy} given).")
    })
  }
}

/** Direct call */
class CallInsn(targetFunTgt: IrFun, argTgts: Seq[Insn], val basicBlock: BasicBlock) extends CallInsnBase {
  val targetFun: IrFunRef = new IrFunRef(Some(targetFunTgt))
  val args: Seq[OperandRef] = argTgts.map(tgt => new OperandRef(this, Some(tgt)))

  override def op: IrOpcode = IrOpcode.Call

  override def resultTy: IrTy = targetFun.get.returnTy

  override def operandRefs: Seq[OperandRef] = args

  override def funSig: IrFunSignature = targetFun.get.signature

  override def copy(newBlock: BasicBlock): CallInsn = new CallInsn(targetFun.get, args.map(_.get), newBlock)

  override def releaseRefs(): Unit = {
    super.releaseRefs()
    targetFun.apply(None)
  }
}

/** Indirect call */
class CallPtrInsn(val funSig: IrFunSignature, funPtrTgt: Insn, argTgts: Seq[Insn], val basicBlock: BasicBlock) extends CallInsnBase {
  val funPtr: OperandRef = new OperandRef(this, Some(funPtrTgt))
  val args: Seq[OperandRef] = argTgts.map(tgt => new OperandRef(this, Some(tgt)))

  override def op: IrOpcode = IrOpcode.CallPtr

  override def resultTy: IrTy = funSig.returnType

  override def operandRefs: Seq[OperandRef] = Seq(funPtr) ++ args

  override def validate(): Unit = {
    super.validate()
    if (funPtr.isDefined && funPtr.get.resultTy != IrTy.PtrTy)
      throw new IrException(s"Target of $this must be a pointer, ${funPtr.get.resultTy} given.")
  }

  override def copy(newBlock: BasicBlock): CallPtrInsn = new CallPtrInsn(funSig, funPtr.get, args.map(_.get), newBlock)
}

/* === Other instructions === */

class PutCharInsn(argTgt: Insn, val basicBlock: BasicBlock) extends Insn {
  val arg: OperandRef = new OperandRef(this, Some(argTgt))

  override def op: IrOpcode = IrOpcode.PutChar

  override def resultTy: IrTy = IrTy.VoidTy

  override def operandRefs: Seq[OperandRef] = Seq(arg)

  override def copy(newBlock: BasicBlock): PutCharInsn = new PutCharInsn(arg.get, newBlock)
}

class GetCharInsn(val basicBlock: BasicBlock) extends Insn {
  override def op: IrOpcode = IrOpcode.GetChar

  override def resultTy: IrTy = IrTy.Int64Ty

  override def copy(newBlock: BasicBlock): GetCharInsn = new GetCharInsn(newBlock)
}

class CastInsn(argTgt: Insn, val basicBlock: BasicBlock) extends Insn {
  val arg: OperandRef = new OperandRef(this, Some(argTgt))
}

/* === Terminators === */

sealed trait IrFunExitPoint extends TerminatorInsn

class RetInsn(retValTgt: Insn, val basicBlock: BasicBlock) extends IrFunExitPoint {
  override def op: IrOpcode = IrOpcode.Ret

  val retVal: OperandRef = new OperandRef(this, Some(retValTgt))

  override def operandRefs: Seq[OperandRef] = Seq(retVal)

  override def succBlockRefs: Seq[TerminatorSuccRef] = Seq.empty

  override def copy(newBlock: BasicBlock): RetInsn = new RetInsn(retVal.get, newBlock)
}

class RetVoidInsn(val basicBlock: BasicBlock) extends IrFunExitPoint {
  override def op: IrOpcode = IrOpcode.RetVoid

  override def succBlockRefs: Seq[TerminatorSuccRef] = Seq.empty

  override def copy(newBlock: BasicBlock): RetVoidInsn = new RetVoidInsn(newBlock)
}

class HaltInsn(val basicBlock: BasicBlock) extends IrFunExitPoint {
  override def op: IrOpcode = IrOpcode.Halt

  override def succBlockRefs: Seq[TerminatorSuccRef] = Seq.empty

  override def copy(newBlock: BasicBlock): HaltInsn = new HaltInsn(newBlock)
}

class BrInsn(succBlockTgt: BasicBlock, val basicBlock: BasicBlock) extends TerminatorInsn {
  override def op: IrOpcode = IrOpcode.Br

  val succBlock: TerminatorSuccRef = new TerminatorSuccRef(this, Some(succBlockTgt))

  override def succBlockRefs: Seq[TerminatorSuccRef] = Seq(succBlock)

  override def copy(newBlock: BasicBlock): BrInsn = new BrInsn(succBlock.get, newBlock)
}

class CondBrInsn(condOpTgt: Insn, trueBlockTgt: BasicBlock, falseBlockTgt: BasicBlock, val basicBlock: BasicBlock) extends TerminatorInsn {
  override def op: IrOpcode = IrOpcode.CondBr

  val condOp: OperandRef = new OperandRef(this, Some(condOpTgt))

  val trueBlock: TerminatorSuccRef = new TerminatorSuccRef(this, Some(trueBlockTgt))
  val falseBlock: TerminatorSuccRef = new TerminatorSuccRef(this, Some(falseBlockTgt))

  override def operandRefs: Seq[OperandRef] = Seq(condOp)

  override def succBlockRefs: Seq[TerminatorSuccRef] = Seq(trueBlock, falseBlock)

  override def copy(newBlock: BasicBlock): CondBrInsn = new CondBrInsn(condOp.get, trueBlock.get, falseBlock.get, newBlock)
}