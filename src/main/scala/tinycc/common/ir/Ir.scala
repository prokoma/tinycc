package tinycc.common.ir

import tinycc.util.NameGen

import scala.collection.mutable

class IrException(message: String) extends RuntimeException(message)

sealed trait IrObject {
  def validate(): Unit

  override def toString: String = IrObject.printer.printToString(this)
}

object IrObject {
  val printer: IrPrinter = new IrPrinter
}

trait Ref[T] extends IterableOnce[T] {
  def owner: IrObject

  def apply(): Option[T]

  def apply(t: Option[T]): Unit

  def apply(t: T): Unit = apply(Some(t))

  def release(): Unit = apply(None)

  // Option[T] compatibility

  override def iterator: Iterator[T] = apply().iterator

  def get: T = apply().getOrElse(throw new NoSuchElementException(s"Cannot dereference empty $this"))

  def isEmpty: Boolean = apply().isEmpty

  def isDefined: Boolean = apply().isDefined

  def contains(t: T): Boolean = apply().contains(t)

  def foreach[U](f: T => U): Unit = apply().foreach(f)

  def map[B](f: T => B): Option[B] = apply().map(f)

  def flatMap[B](f: T => Option[B]): Option[B] = apply().flatMap(f)

  override def toString: String = s"Ref(owner=$owner, target=${apply().getOrElse("<null>")})"
}

object Ref {
  def unapply[T](ref: Ref[T]): Option[(IrObject, Option[T])] = Some((ref.owner, ref()))
}

trait UseTracking[R <: Ref[T], T] {
  val uses: mutable.Set[R] = mutable.Set.empty[R]

  def replaceUses(rep: Option[T]): Unit = uses.foreach(_.apply(rep))

  def replaceUses(rep: T): Unit = uses.foreach(_.apply(rep))

  def removeUses(): Unit = uses.foreach(_.apply(None))
}

/** A smart reference to IR instruction. */
abstract class InsnRef(private var target: Option[Insn]) extends Ref[Insn] {
  def this(target: Insn) = this(Some(target))

  target.foreach(_.uses.add(this))

  def apply(): Option[Insn] = target

  def apply(t: Option[Insn]): Unit = {
    target.foreach(_.uses.remove(this))
    target = t
    target.foreach(_.uses.add(this))
  }
}

/** An instruction operand. */
class OperandRef(val owner: Insn, _target: Option[Insn]) extends InsnRef(_target) {
  override def toString: String = s"OperandRef(owner=$owner, target=${apply().getOrElse("<null>")})"
}

object OperandRef {
  def unapply(ref: OperandRef): Option[(Insn, Option[Insn])] = Some((ref.owner, ref()))
}

/** A smart reference to BasicBlock. */
abstract class BasicBlockRef(private var target: Option[BasicBlock]) extends Ref[BasicBlock] {
  def this(target: BasicBlock) = this(Some(target))

  target.foreach(_.uses.add(this))

  def apply(): Option[BasicBlock] = target

  def apply(t: Option[BasicBlock]): Unit = {
    target.foreach(_.uses.remove(this))
    target = t
    target.foreach(_.uses.add(this))
  }
}

class OperandBlockRef(val owner: Insn, _target: Option[BasicBlock]) extends BasicBlockRef(_target) {
  override def toString: String = s"OperandBlockRef(owner=$owner, target=${apply().getOrElse("<null>")})"
}

class EntryBlockRef(val owner: IrFun, _target: Option[BasicBlock]) extends BasicBlockRef(_target) {
  override def toString: String = s"EntryBlockRef(owner=$owner, target=${apply().getOrElse("<null>")})"
}

abstract class IrFunRef(private var target: Option[IrFun]) extends Ref[IrFun] {
  def this(target: IrFun) = this(Some(target))

  target.foreach(_.uses.add(this))

  def apply(): Option[IrFun] = target

  def apply(t: Option[IrFun]): Unit = {
    target.foreach(_.uses.remove(this))
    target = t
    target.foreach(_.uses.add(this))
  }
}

class OperandFunRef(val owner: Insn, _target: Option[IrFun]) extends IrFunRef(_target) {
  override def toString: String = s"OperandFunRef(owner=$owner, target=${apply().getOrElse("<null>")})"
}

class EntryFunRef(val owner: IrProgram, _target: Option[IrFun]) extends IrFunRef(_target) {
  override def toString: String = s"EntryFunRef(owner=$owner, target=${apply().getOrElse("<null>")})"
}

abstract class Insn(val op: IrOpcode, val basicBlock: BasicBlock) extends IrObject with UseTracking[InsnRef, Insn] {
  def pred: Option[Insn] = basicBlock.getInsnPred(this)

  def succ: Option[Insn] = basicBlock.getInsnSucc(this)

  def fun: IrFun = basicBlock.fun

  def operandRefs: IndexedSeq[OperandRef] = IndexedSeq.empty

  def operands: IndexedSeq[Insn] = operandRefs.flatMap(_())

  def hasSideEffects: Boolean = false

  def resultTy: IrTy

  def parentNameGen: NameGen = fun.nameGen

  // Name is local to the function by default and automatically set.
  protected var _name: String = parentNameGen.apply()

  def name: String = _name

  def name(newName: String): this.type = {
    parentNameGen.releaseName(_name)
    _name = fun.nameGen(newName);
    this
  }

  override def validate(): Unit = {
    operandRefs.zipWithIndex.foreach({ case (ref, index) =>
      assert(ref.isDefined, s"operand #$index is not defined")
    })
  }

  def remove(removeUses: Boolean = false): Unit = basicBlock.removeInsn(this, removeUses)

  def copy(newBlock: BasicBlock): Insn

  def releaseRefs(): Unit = {
    parentNameGen.releaseName(name)
    operandRefs.foreach(_.apply(None))
  }

  override def toString: String = s"${getClass.getSimpleName}(${super.toString})"
}

/* === BasicBlock & IrFun === */

class BasicBlock(_name: String, val fun: IrFun) extends IrObject with UseTracking[BasicBlockRef, BasicBlock] {
  val name: String = fun.bbNameGen(_name)

  var body: IndexedSeq[Insn] = IndexedSeq.empty

  def uniqueName: String = fun.name + "$" + name

  def terminator: Option[TerminatorInsn] = body.collectFirst({ case t: TerminatorInsn => t })

  def isFunEntryBlock: Boolean = fun.entryBlockRef.contains(this)

  def pred: Seq[BasicBlock] = fun.getBlockPred(this)

  def succRefs: Seq[BasicBlockRef] = terminator.map(_.succBlockRefs).getOrElse(Seq.empty)

  def succ: Seq[BasicBlock] = succRefs.flatMap(_())

  def linPred: Option[BasicBlock] = fun.getBlockLinPred(this)

  def linSucc: Option[BasicBlock] = fun.getBlockLinSucc(this)

  def append[T <: Insn](insn: T): T = {
    if (insn.basicBlock != this)
      throw new IrException(s"Cannot append '$insn' owned by '${insn.basicBlock.name}.'")
    if (terminator.isDefined)
      throw new IrException(s"Cannot append '$insn' to terminated block '$name' in '${fun.name}'.")
    body = body :+ insn
    insn
  }

  override def validate(): Unit = {
    body.foreach(insn => {
      assert(insn.basicBlock == this, s"$insn is owned by ${insn.basicBlock}")
      insn.validate()
    })
    assert(terminator.isDefined, "unterminated basic block")
    assert(terminator.get == body.last, "terminator is not the last instruction in basic block")
    assert(body.count(_.isInstanceOf[TerminatorInsn]) == 1, "multiple terminator instructions in a basic block")
  }

  def releaseRefs(): Unit = {
    fun.bbNameGen.releaseName(name)
    body.foreach(_.releaseRefs())
  }

  // Insn Ops

  def findInsn(insn: Insn): Option[Int] = body.indexOf(insn) match {
    case -1 => None
    case idx => Some(idx)
  }

  def getInsnSucc(insn: Insn): Option[Insn] = findInsn(insn).flatMap({
    case idx if idx == body.length - 1 => None
    case idx => Some(body(idx + 1))
  })

  def getInsnPred(insn: Insn): Option[Insn] = findInsn(insn).flatMap({
    case idx if idx == 0 => None
    case idx => Some(body(idx - 1))
  })

  def removeInsn(insn: Insn, removeUses: Boolean = false): Unit = {
    if (insn.basicBlock != this)
      throw new IrException(s"Cannot remove insn '$insn' owned by '${insn.basicBlock.name}' from '$name'.")

    if (removeUses)
      insn.removeUses()
    else if (insn.uses.nonEmpty)
      throw new IrException(s"Cannot remove still referenced insn '$insn' (referenced by ${insn.uses}).")

    insn.releaseRefs()
    body = body.filterNot(_ == insn)
  }

  def remove(removeUses: Boolean = false): Unit = fun.removeBlock(this, removeUses)

  override def toString: String = s"BasicBlock($name, fun=$fun)"
}

case class IrFunSignature(returnTy: IrTy, argTys: IndexedSeq[IrTy])

class IrFun(val _name: String, val signature: IrFunSignature, val program: IrProgram) extends IrObject with UseTracking[IrFunRef, IrFun] {
  def this(_name: String, returnTy: IrTy, argTys: IndexedSeq[IrTy], program: IrProgram) = this(_name, IrFunSignature(returnTy, argTys), program)

  val name: String = program.nameGen(_name)

  var basicBlocks: IndexedSeq[BasicBlock] = IndexedSeq.empty

  def returnTy: IrTy = signature.returnTy

  def argTys: IndexedSeq[IrTy] = signature.argTys

  def insns: Seq[Insn] = basicBlocks.flatMap(_.body)

  def getBlockPred(basicBlock: BasicBlock): Seq[BasicBlock] = basicBlocks.filter(_.succ.contains(basicBlock)).toSeq

  val entryBlockRef: EntryBlockRef = new EntryBlockRef(this, None)

  def entryBlock: BasicBlock = entryBlockRef.get

  def exitPoints: Seq[IrFunExitPoint] = basicBlocks.flatMap(_.terminator).collect({ case ep: IrFunExitPoint => ep }).toSeq

  def locals: Seq[AllocLInsn] = insns.collect({ case al: AllocLInsn => al })

  val nameGen: NameGen = program.nameGen.newChild()

  val bbNameGen: NameGen = new NameGen

  def releaseRefs(): Unit = {
    program.nameGen.releaseName(name)
    entryBlockRef.release()
    basicBlocks.foreach(_.releaseRefs())
  }

  def append(basicBlock: BasicBlock): BasicBlock = {
    if (!basicBlocks.contains(basicBlock))
      basicBlocks = basicBlocks :+ basicBlock
    if (entryBlockRef.isEmpty)
      entryBlockRef(basicBlock)
    basicBlock
  }

  override def validate(): Unit = {
    if (basicBlocks.toSet.size != basicBlocks.size)
      throw new IrException(s"$this: Function contains duplicate basic blocks.")
    basicBlocks.foreach(bb => {
      assert(bb.fun == this, s"$bb is owned by ${bb.fun}")
      bb.validate()
    })
    if (entryBlockRef.isEmpty)
      throw new IrException(s"Function '$name' doesn't have entry block set.")
    if (entryBlock.pred.nonEmpty)
      throw new IrException(s"Entry block '${entryBlock.name}' of function '$name' can't have any predecessors.")
  }

  override def toString: String = s"IrFun($name)"

  // Block Ops

  def findBlock(block: BasicBlock): Option[Int] = basicBlocks.indexOf(block) match {
    case -1 => None
    case idx => Some(idx)
  }

  def removeBlock(basicBlock: BasicBlock, removeUses: Boolean = false): Unit = {
    if (basicBlock.fun != this)
      throw new IrException(s"Cannot remove block '${basicBlock.name}' owned by '${basicBlock.fun.name}' from '$name'.")

    if (removeUses)
      basicBlock.removeUses()
    else if (basicBlock.uses.nonEmpty)
      throw new IrException(s"Cannot remove still referenced block '${basicBlock.name}'.")

    basicBlock.releaseRefs()
    basicBlocks = basicBlocks.filterNot(_ == basicBlock)
  }

  /** Get successor of the block in program order (not cfg). */
  def getBlockLinSucc(block: BasicBlock): Option[BasicBlock] = findBlock(block).flatMap({
    case idx if idx == basicBlocks.length - 1 => None
    case idx => Some(basicBlocks(idx + 1))
  })

  def getBlockLinPred(block: BasicBlock): Option[BasicBlock] = findBlock(block).flatMap({
    case idx if idx == 0 => None
    case idx => Some(basicBlocks(idx - 1))
  })
}

class IrProgram extends IrObject {
  var funs: IndexedSeq[IrFun] = IndexedSeq.empty

  val nameGen: NameGen = new NameGen

  def basicBlocks: Iterable[BasicBlock] = funs.flatMap(_.basicBlocks)

  def insns: Iterable[Insn] = funs.flatMap(_.insns)

  val entryFunRef: EntryFunRef = new EntryFunRef(this, None)

  def entryFun: IrFun = entryFunRef.get

  def globals: Iterable[AllocGInsn] = insns.collect({ case al: AllocGInsn => al })

  def append(fun: IrFun): IrFun = {
    if (!funs.contains(fun))
      funs = funs :+ fun
    fun
  }

  def appendEntryFun(): IrFun = {
    if (entryFunRef.isDefined)
      throw new IrException("entryFun is already defined.")
    val entryFun = append(new IrFun(IrProgram.entryFunName, IrTy.VoidTy, IndexedSeq.empty, this))
    entryFunRef(entryFun)
    entryFun
  }

  override def validate(): Unit = {
    funs.foreach(_.validate())
    assert(entryFunRef.isDefined, "entryFun is not defined")
  }

  def removeFun(fun: IrFun, force: Boolean = false): Unit = {
    if (fun.program != this)
      throw new IrException(s"Cannot remove fun '${fun.name}' owned by '${fun.program}' from '$this'.")

    if (force)
      fun.removeUses()
    else if (fun.uses.nonEmpty)
      throw new IrException(s"Cannot remove still referenced function '${fun.name}'.")

    funs = funs.filterNot(_ == fun)
  }
}

object IrProgram {
  val entryFunName: String = "entry"
}