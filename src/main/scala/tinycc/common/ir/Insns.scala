package tinycc.common.ir

import tinycc.common.ir.IrTy.{DoubleTy, Int64Ty, PtrTy, VoidTy}
import tinycc.util.NameGen

/** An instruction, which terminates a BasicBlock. */
sealed trait TerminatorInsn extends Insn {
  def succBlockRefs: Seq[OperandBlockRef]

  def succBlocks: Seq[BasicBlock] = succBlockRefs.map(_.get)

  override def resultTy: IrTy = VoidTy

  override def hasSideEffects: Boolean = true

  def copy(newBlock: BasicBlock): TerminatorInsn

  override def releaseRefs(): Unit = {
    super.releaseRefs()
    succBlockRefs.foreach(_.release())
  }

  override def validate(): Unit = {
    super.validate()
    succBlockRefs.zipWithIndex.foreach({ case (ref, index) =>
      assert(ref.isDefined, s"succesor block #$index is not defined")
    })
    succBlocks.foreach(bb => {
      assert(bb.fun == fun, s"terminator references block $bb from other function")
    })
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

sealed abstract class BinaryInsn(_left: Option[Insn], _right: Option[Insn], val basicBlock: BasicBlock) extends Insn {
  val leftRef: OperandRef = new OperandRef(this, _left)
  val rightRef: OperandRef = new OperandRef(this, _right)

  def left: Insn = leftRef.get

  def right: Insn = rightRef.get

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(leftRef, rightRef)
}

class BinaryArithInsn(val op: IrOpcode.BinaryArithOp, _left: Option[Insn], _right: Option[Insn], basicBlock: BasicBlock) extends BinaryInsn(_left, _right, basicBlock) {
  def this(op: IrOpcode.BinaryArithOp, _left: Insn, _right: Insn, basicBlock: BasicBlock) = this(op, Some(_left), Some(_right), basicBlock)

  override def resultTy: IrTy = op.insnTy

  override def validate(): Unit = {
    super.validate()
    assert(left.resultTy == op.insnTy)
    assert(right.resultTy == op.insnTy)
  }

  override def copy(newBlock: BasicBlock): Insn = new BinaryArithInsn(op, leftRef(), rightRef(), newBlock)
}

/** All compare instructions return either 0 or 1. */
class CmpInsn(val op: IrOpcode.CmpOp, _left: Option[Insn], _right: Option[Insn], basicBlock: BasicBlock) extends BinaryInsn(_left, _right, basicBlock) {
  def this(op: IrOpcode.CmpOp, _left: Insn, _right: Insn, basicBlock: BasicBlock) = this(op, Some(_left), Some(_right), basicBlock)

  override def resultTy: IrTy = Int64Ty

  override def validate(): Unit = {
    super.validate()
    assert(left.resultTy == op.operandTy)
    assert(right.resultTy == op.operandTy)
  }

  override def copy(newBlock: BasicBlock): Insn = new CmpInsn(op, leftRef(), rightRef(), newBlock)
}

/* === Memory Instructions === */

sealed abstract class AllocInsn(val varTy: IrTy, val basicBlock: BasicBlock) extends Insn {
  override def resultTy: IrTy = PtrTy
}

class AllocLInsn(varTy: IrTy, basicBlock: BasicBlock) extends AllocInsn(varTy, basicBlock) {
  override def op: IrOpcode = IrOpcode.AllocL

  override def copy(newBlock: BasicBlock): AllocLInsn = new AllocLInsn(varTy, newBlock)
}

/**
 * Represents a global or static variable.
 *
 * @param initData initial data (64 bit words), padded with zeroes if shorter than [[varTy.sizeWords]]
 */
class AllocGInsn(varTy: IrTy, val initData: Seq[Long], basicBlock: BasicBlock) extends AllocInsn(varTy, basicBlock) {
  override def parentNameGen: NameGen = fun.program.nameGen // names of AllocGInsn are unique in the entire program

  override def op: IrOpcode = IrOpcode.AllocG

  override def copy(newBlock: BasicBlock): AllocGInsn = new AllocGInsn(varTy, initData, newBlock)

  override def validate(): Unit = {
    super.validate()
    assert(initData.size <= varTy.sizeWords)
  }
}

class LoadInsn(val valueTy: IrTy, _ptr: Option[Insn], val basicBlock: BasicBlock) extends Insn {
  def this(valueTy: IrTy, _ptr: Insn, basicBlock: BasicBlock) = this(valueTy, Some(_ptr), basicBlock)

  val ptrRef: OperandRef = new OperandRef(this, _ptr)

  def ptr: Insn = ptrRef.get

  override def op: IrOpcode = IrOpcode.Load

  override def resultTy: IrTy = valueTy

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(ptrRef)

  override def copy(newBlock: BasicBlock): LoadInsn = new LoadInsn(valueTy, ptrRef(), newBlock)
}

class StoreInsn(_ptr: Option[Insn], _value: Option[Insn], val basicBlock: BasicBlock) extends Insn {
  def this(_ptr: Insn, _value: Insn, basicBlock: BasicBlock) = this(Some(_ptr), Some(_value), basicBlock)

  val ptrRef: OperandRef = new OperandRef(this, _ptr)
  val valueRef: OperandRef = new OperandRef(this, _value)

  def ptr: Insn = ptrRef.get

  def value: Insn = valueRef.get

  override def op: IrOpcode = IrOpcode.Store

  override def resultTy: IrTy = VoidTy

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(ptrRef, valueRef)

  override def hasSideEffects: Boolean = true

  override def copy(newBlock: BasicBlock): StoreInsn = new StoreInsn(ptrRef(), valueRef(), newBlock)
}

class GetElementPtrInsn(_ptr: Option[Insn], _index: Option[Insn], val elemTy: IrTy, val fieldIndex: Int, val basicBlock: BasicBlock) extends Insn {
  def this(_ptr: Insn, _index: Insn, elemTy: IrTy, fieldIndex: Int, basicBlock: BasicBlock) = this(Some(_ptr), Some(_index), elemTy, fieldIndex, basicBlock)

  val ptrRef: OperandRef = new OperandRef(this, _ptr)
  val indexRef: OperandRef = new OperandRef(this, _index)

  def ptr: Insn = ptrRef.get

  def index: Insn = indexRef.get

  override def op: IrOpcode = IrOpcode.GetElementPtr

  override def resultTy: IrTy = PtrTy

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(ptrRef, indexRef)

  override def validate(): Unit = {
    super.validate()
    assert(ptr.resultTy == PtrTy)
    assert(index.resultTy == Int64Ty)
  }

  override def copy(newBlock: BasicBlock): GetElementPtrInsn = new GetElementPtrInsn(ptrRef(), indexRef(), elemTy, fieldIndex, newBlock)
}

class SizeOfInsn(val varTy: IrTy, val basicBlock: BasicBlock) extends Insn {
  override def op: IrOpcode = IrOpcode.SizeOf

  override def resultTy: IrTy = Int64Ty

  override def copy(newBlock: BasicBlock): SizeOfInsn = new SizeOfInsn(varTy, newBlock)
}

/* === Function call instructions === */

class GetFunPtrInsn(_targetFun: Option[IrFun], val basicBlock: BasicBlock) extends Insn {
  def this(_targetFun: IrFun, basicBlock: BasicBlock) = this(Some(_targetFun), basicBlock)

  val targetFunRef: OperandFunRef = new OperandFunRef(this, _targetFun)

  def targetFun: IrFun = targetFunRef.get

  override def releaseRefs(): Unit = {
    super.releaseRefs()
    targetFunRef.release()
  }

  override def op: IrOpcode = IrOpcode.GetFunPtr

  override def resultTy: IrTy = PtrTy

  override def copy(newBlock: BasicBlock): GetFunPtrInsn = new GetFunPtrInsn(targetFunRef(), newBlock)
}

class LoadArgInsn(val index: Int, val basicBlock: BasicBlock) extends Insn {
  override def op: IrOpcode = IrOpcode.LoadArg

  override def resultTy: IrTy = basicBlock.fun.argTys(index)

  override def copy(newBlock: BasicBlock): LoadArgInsn = new LoadArgInsn(index, newBlock)
}

sealed trait CallInsnBase extends Insn {
  def argRefs: IndexedSeq[OperandRef]

  def args: IndexedSeq[Insn] = argRefs.map(_.get)

  def funSig: IrFunSignature

  override def hasSideEffects: Boolean = true

  override def validate(): Unit = {
    super.validate()
    if (argRefs.size != funSig.argTys.size)
      throw new IrException(s"Wrong number of arguments in $this (expected ${funSig.argTys.size}, ${argRefs.size} given).")
    argRefs.zip(funSig.argTys).zipWithIndex.foreach({ case ((arg, sigTy), i) =>
      if (arg.isDefined && arg.get.resultTy != sigTy)
        throw new IrException(s"Wrong type of $i-th argument in $this (expected ${sigTy}, ${arg.get.resultTy} given).")
    })
  }
}

/** Direct call */
class CallInsn(_targetFun: Option[IrFun], _args: IndexedSeq[Option[Insn]], val basicBlock: BasicBlock) extends CallInsnBase {
  def this(_targetFun: IrFun, _args: IndexedSeq[Insn], basicBlock: BasicBlock) = this(Some(_targetFun), _args.map(Some(_)), basicBlock)

  val targetFunRef: OperandFunRef = new OperandFunRef(this, _targetFun)

  override val argRefs: IndexedSeq[OperandRef] = _args.map(new OperandRef(this, _))

  def targetFun: IrFun = targetFunRef.get

  override def op: IrOpcode = IrOpcode.Call

  override def resultTy: IrTy = targetFun.returnTy

  override def operandRefs: IndexedSeq[OperandRef] = argRefs

  override def funSig: IrFunSignature = targetFun.signature

  override def copy(newBlock: BasicBlock): CallInsn = new CallInsn(targetFunRef(), argRefs.map(_.apply()), newBlock)

  override def releaseRefs(): Unit = {
    super.releaseRefs()
    targetFunRef.release()
  }
}

/** Indirect call */
class CallPtrInsn(override val funSig: IrFunSignature, _funPtr: Option[Insn], _args: IndexedSeq[Option[Insn]], val basicBlock: BasicBlock) extends CallInsnBase {
  def this(funSig: IrFunSignature, _funPtr: Insn, _args: IndexedSeq[Insn], basicBlock: BasicBlock) = this(funSig, Some(_funPtr), _args.map(Some(_)), basicBlock)

  val funPtrRef: OperandRef = new OperandRef(this, _funPtr)
  override val argRefs: IndexedSeq[OperandRef] = _args.map(new OperandRef(this, _))

  def funPtr: Insn = funPtrRef.get

  override def op: IrOpcode = IrOpcode.CallPtr

  override def resultTy: IrTy = funSig.returnTy

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(funPtrRef) ++ argRefs

  override def validate(): Unit = {
    super.validate()
    assert(funPtr.resultTy == PtrTy)
  }

  override def copy(newBlock: BasicBlock): CallPtrInsn = new CallPtrInsn(funSig, funPtrRef(), argRefs.map(_.apply()), newBlock)
}

/* === Other instructions === */

class PutCharInsn(_arg: Option[Insn], val basicBlock: BasicBlock) extends Insn {
  def this(_arg: Insn, basicBlock: BasicBlock) = this(Some(_arg), basicBlock)

  val argRef: OperandRef = new OperandRef(this, _arg)

  def arg: Insn = argRef.get

  override def op: IrOpcode = IrOpcode.PutChar

  override def resultTy: IrTy = VoidTy

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(argRef)

  override def hasSideEffects: Boolean = true

  override def validate(): Unit = {
    super.validate()
    assert(arg.resultTy == Int64Ty)
  }

  override def copy(newBlock: BasicBlock): PutCharInsn = new PutCharInsn(argRef(), newBlock)
}

class PutNumInsn(_arg: Option[Insn], val basicBlock: BasicBlock) extends Insn {
  def this(_arg: Insn, basicBlock: BasicBlock) = this(Some(_arg), basicBlock)

  val argRef: OperandRef = new OperandRef(this, _arg)

  def arg: Insn = argRef.get

  override def op: IrOpcode = IrOpcode.PutNum

  override def resultTy: IrTy = VoidTy

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(argRef)

  override def hasSideEffects: Boolean = true

  override def validate(): Unit = {
    super.validate()
    assert(arg.resultTy == Int64Ty)
  }

  override def copy(newBlock: BasicBlock): PutNumInsn = new PutNumInsn(argRef(), newBlock)
}

class GetCharInsn(val basicBlock: BasicBlock) extends Insn {
  override def op: IrOpcode = IrOpcode.GetChar

  override def resultTy: IrTy = Int64Ty

  override def hasSideEffects: Boolean = true

  override def copy(newBlock: BasicBlock): GetCharInsn = new GetCharInsn(newBlock)
}

class PhiInsn(_args: IndexedSeq[(Option[Insn], Option[BasicBlock])], val basicBlock: BasicBlock) extends Insn {
//  def this(_args: IndexedSeq[(Insn, BasicBlock)], basicBlock: BasicBlock) = this(_args.map({ case (insn, bb) => (Some(insn), Some(bb)) }), basicBlock)

  val argRefs: IndexedSeq[(OperandRef, OperandBlockRef)] = _args.map({ case (insn, bb) => (new OperandRef(this, insn), new OperandBlockRef(this, bb)) })

  def args: IndexedSeq[(Insn, BasicBlock)] = argRefs.map({ case (insn, bb) => (insn.get, bb.get) })

  override def op: IrOpcode = IrOpcode.Phi

  override def operandRefs: IndexedSeq[OperandRef] = argRefs.map(_._1)

  override def resultTy: IrTy = args.head._1.resultTy

  override def validate(): Unit = {
    super.validate()
    assert(args.nonEmpty)
    assert(args.map(_._2).toSet == basicBlock.pred.toSet, s"phi node must reference all predecessors of the basic block it is in")
  }

  override def copy(newBlock: BasicBlock): Insn = new PhiInsn(argRefs.map({ case (insnRef, bbRef) => (insnRef(), bbRef()) }), newBlock)
}

class CastInsn(val op: IrOpcode.CastOp, _arg: Option[Insn], val basicBlock: BasicBlock) extends Insn {
  def this(op: IrOpcode.CastOp, _arg: Insn, basicBlock: BasicBlock) = this(op, Some(_arg), basicBlock)

  val argRef: OperandRef = new OperandRef(this, _arg)

  def arg: Insn = argRef.get

  override def resultTy: IrTy = op.resultTy

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(argRef)

  override def validate(): Unit = {
    super.validate()
    assert(arg.resultTy == op.operandTy)
  }

  override def copy(newBlock: BasicBlock): Insn = new CastInsn(op, argRef(), newBlock)
}

/* === Terminators === */

sealed trait IrFunExitPoint extends TerminatorInsn

class RetInsn(_arg: Option[Insn], val basicBlock: BasicBlock) extends IrFunExitPoint {
  def this(_arg: Insn, basicBlock: BasicBlock) = this(Some(_arg), basicBlock)

  val argRef: OperandRef = new OperandRef(this, _arg)

  def arg: Insn = argRef.get

  override def op: IrOpcode = IrOpcode.Ret

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(argRef)

  override def succBlockRefs: Seq[OperandBlockRef] = Seq.empty

  override def validate(): Unit = {
    super.validate()
    assert(arg.resultTy == basicBlock.fun.returnTy)
  }

  override def copy(newBlock: BasicBlock): RetInsn = new RetInsn(argRef(), newBlock)
}

class RetVoidInsn(val basicBlock: BasicBlock) extends IrFunExitPoint {
  override def op: IrOpcode = IrOpcode.RetVoid

  override def succBlockRefs: Seq[OperandBlockRef] = Seq.empty

  override def validate(): Unit = {
    super.validate()
    assert(basicBlock.fun.returnTy == VoidTy)
  }

  override def copy(newBlock: BasicBlock): RetVoidInsn = new RetVoidInsn(newBlock)
}

class HaltInsn(val basicBlock: BasicBlock) extends IrFunExitPoint {
  override def op: IrOpcode = IrOpcode.Halt

  override def succBlockRefs: Seq[OperandBlockRef] = Seq.empty

  override def copy(newBlock: BasicBlock): HaltInsn = new HaltInsn(newBlock)
}

class BrInsn(_succBlock: Option[BasicBlock], val basicBlock: BasicBlock) extends TerminatorInsn {
  def this(_succBlock: BasicBlock, basicBlock: BasicBlock) = this(Some(_succBlock), basicBlock)

  val succBlockRef: OperandBlockRef = new OperandBlockRef(this, _succBlock)

  def succBlock: BasicBlock = succBlockRef.get

  override def op: IrOpcode = IrOpcode.Br

  override def succBlockRefs: Seq[OperandBlockRef] = Seq(succBlockRef)

  override def copy(newBlock: BasicBlock): BrInsn = new BrInsn(succBlockRef(), newBlock)
}

/** If condOp is non-zero, the successor is trueBlock, otherwise falseBlock. */
class CondBrInsn(_arg: Option[Insn], _trueBlock: Option[BasicBlock], _falseBlock: Option[BasicBlock], val basicBlock: BasicBlock) extends TerminatorInsn {
  def this(_arg: Insn, _trueBlock: BasicBlock, _falseBlock: BasicBlock, basicBlock: BasicBlock) = this(Some(_arg), Some(_trueBlock), Some(_falseBlock), basicBlock)

  override def op: IrOpcode = IrOpcode.CondBr

  val argRef: OperandRef = new OperandRef(this, _arg)

  val trueBlockRef: OperandBlockRef = new OperandBlockRef(this, _trueBlock)
  val falseBlockRef: OperandBlockRef = new OperandBlockRef(this, _falseBlock)

  def arg: Insn = argRef.get

  def trueBlock: BasicBlock = trueBlockRef.get

  def falseBlock: BasicBlock = falseBlockRef.get

  override def operandRefs: IndexedSeq[OperandRef] = IndexedSeq(argRef)

  override def succBlockRefs: Seq[OperandBlockRef] = Seq(trueBlockRef, falseBlockRef)

  override def copy(newBlock: BasicBlock): CondBrInsn = new CondBrInsn(argRef(), trueBlockRef(), falseBlockRef(), newBlock)

  override def validate(): Unit = {
    super.validate()
    assert(arg.resultTy == Int64Ty)
  }
}