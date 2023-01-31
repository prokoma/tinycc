package tinycc.common.ir

import scala.collection.mutable

sealed trait IrObject {
  def validate(): Unit

  override def toString: String = IrObject.printer.printToString(this)
}

object IrObject {
  val printer: IrPrinter = new IrPrinter
}

trait Ref[T] extends IterableOnce[T] {
  def apply(): Option[T]

  def apply(t: Option[T]): Unit

  def apply(t: T): Unit = apply(Some(t))

  // Option[T] compatibility

  override def iterator: Iterator[T] = apply().iterator

  def get: T = apply().get

  def isEmpty: Boolean = apply().isEmpty

  def isDefined: Boolean = apply().isDefined

  def contains(t: T): Boolean = apply().contains(t)

  def foreach[U](f: T => U): Unit = apply().foreach(f)

  def map[B](f: T => B): Option[B] = apply().map(f)

  def flatMap[B](f: T => Option[B]): Option[B] = apply().flatMap(f)
}

object Ref {
  def unapply[T](ref: Ref[T]): Option[T] = ref()
}

trait UseTracking[R <: Ref[T], T] {
  val uses: mutable.Set[R] = mutable.Set.empty[R]

  def replaceUses(rep: Option[T]): Unit = uses.foreach(_.apply(rep))

  def replaceUses(rep: T): Unit = uses.foreach(_.apply(rep))

  def removeUses(): Unit = uses.foreach(_.apply(None))
}

/** A smart reference to IR instruction. */
class InsnRef(private var target: Option[Insn]) extends Ref[Insn] {
  target.foreach(_.uses.add(this))

  def apply(): Option[Insn] = target

  def apply(t: Option[Insn]): Unit = {
    target.foreach(_.uses.remove(this))
    target = t
    target.foreach(_.uses.add(this))
  }
}

/** A smart reference to BasicBlock. */
class BasicBlockRef(private var target: Option[BasicBlock]) extends Ref[BasicBlock] {
  target.foreach(_.uses.add(this))

  def apply(): Option[BasicBlock] = target

  def apply(t: Option[BasicBlock]): Unit = {
    target.foreach(_.uses.remove(this))
    target = t
    target.foreach(_.uses.add(this))
  }
}

/** An instruction operand. */
class OperandRef(val insn: Insn, target: Option[Insn]) extends InsnRef(target) {
  override def get: Insn = apply()
    .getOrElse(throw new IRException(s"Cannot access null operand of $insn"))
}

class IRFunRef(private var target: Option[IRFun]) extends Ref[IRFun] {
  if (target.isDefined)
    target.get.uses.add(this)

  def apply(): Option[IRFun] = target

  def apply(t: Option[IRFun]): Unit = {
    if (target.isDefined)
      target.get.uses.remove(this)
    target = t
    if (t.isDefined)
      target.get.uses.add(this)
  }
}

trait IrInsn extends UseTracking[InsnRef, IrInsn] with IRObject {
  def op: IrOpcode

  def pred: Option[IrInsn] = basicBlock.getInsnPred(this)

  def succ: Option[IrInsn] = basicBlock.getInsnSucc(this)

  def basicBlock: BasicBlock

  def fun: IRFun = basicBlock.fun

  def operandRefs: Seq[OperandRef] = Seq.empty[OperandRef]

  def operands: Seq[IrInsn] = operandRefs.flatMap(_())

  def resultType: IRType

  val name: String = fun.nameGen()

  override def validate(): Unit = ()

  def remove(force: Boolean = false): Unit = basicBlock.removeInsn(this, force)

  def replace(newInsn: Insn, replaceUses: Boolean = false): Unit = basicBlock.replaceInsn(this, newInsn, replaceUses)

  def copy(newBlock: BasicBlock): Insn

  def releaseRefs(): Unit =
    operandRefs.foreach(_.apply(None))

  override def toString: String = s"${getClass.getSimpleName}(${super.toString})"
}

class IrBinaryOpInsn(val op: IrBinaryOp, left: IrInsn, right: IrInsn) extends IrInsn {

}

class BasicBlock extends IrObject

class IrFunction extends IrObject

class IrProgram extends IrObject