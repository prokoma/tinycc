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

trait User[R <: Ref[T], T] {
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

/** A smart reference to IrFun. */
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

abstract class Insn(val op: IrOpcode, val basicBlock: BasicBlock) extends IrObject with User[InsnRef, Insn] {
  def pred: Option[Insn] = basicBlock.getInsnPred(this)

  def succ: Option[Insn] = basicBlock.getInsnSucc(this)

  def fun: IrFun = basicBlock.fun

  def operandRefs: IndexedSeq[OperandRef] = IndexedSeq.empty

  def operands: IndexedSeq[Insn] = operandRefs.flatMap(_())

  def resultTy: IrTy

  /** [[NameGen]] instance used to generate name of this instruction */
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

  /** Create a detached copy of this instruction in [[newBlock]]. */
  def copy(newBlock: BasicBlock): Insn

  def releaseRefs(): Unit = {
    parentNameGen.releaseName(name)
    operandRefs.foreach(_.apply(None))
  }

  override def toString: String = s"${getClass.getSimpleName}(${super.toString})"
}

/* === BasicBlock & IrFun === */

class BasicBlock(_name: String, val fun: IrFun) extends IrObject with User[BasicBlockRef, BasicBlock] {
  val name: String = fun.bbNameGen(_name)

  var body: IndexedSeq[Insn] = IndexedSeq.empty

  def uniqueName: String = fun.name + "$" + name

  def terminatorOption: Option[TerminatorInsn] = body.collectFirst({ case t: TerminatorInsn => t })

  def terminator: TerminatorInsn = terminatorOption.get

  def isFunEntryBlock: Boolean = fun.entryBlockRef.contains(this)

  /** Get list of predecessors of this basic block in the CFG. */
  def pred: Seq[BasicBlock] = fun.getBlockPred(this)

  /** Get list of successors of this basic block in the CFG. */
  def succ: Seq[BasicBlock] = terminator.succBlocks

  /** Get the predecessor of this basic block in the program order (linearized). */
  def linPred: Option[BasicBlock] = fun.getBlockLinPred(this)

  /** Get the successor of this basic block in the program order (linearized). */
  def linSucc: Option[BasicBlock] = fun.getBlockLinSucc(this)

  def append[T <: Insn](insn: T): T = {
    require(insn.basicBlock == this, s"cannot append $insn owned by ${insn.basicBlock} to $this")
    assert(terminatorOption.isEmpty, s"cannot append $insn to terminated block $this")
    assert(!body.contains(insn), s"cannot append duplicate $insn to $this (use IrManipulator for that)")

    body = body :+ insn
    insn
  }

  def getInsnSucc(insn: Insn): Option[Insn] = {
    require(insn.basicBlock == this, s"cannot query successor of $insn owned by ${insn.basicBlock} in $this")
    if (body.last == insn) None
    else {
      val idx = body.indexOf(insn)
      assert(idx != -1, s"cannot query successor of detached $insn")
      Some(body(idx + 1))
    }
  }

  def getInsnPred(insn: Insn): Option[Insn] = {
    require(insn.basicBlock == this, s"cannot query predecessor of $insn owned by ${insn.basicBlock} in $this")
    if (body.head == insn) None
    else {
      val idx = body.indexOf(insn)
      assert(idx != -1, s"cannot query predecessor of detached $insn")
      Some(body(idx - 1))
    }
  }

  def removeInsn(insn: Insn, removeUses: Boolean = false): Unit = {
    require(insn.basicBlock == this, s"cannot remove $insn owned by ${insn.basicBlock} from $this")
    if (removeUses)
      insn.removeUses()
    assert(insn.uses.isEmpty, s"cannot remove still referenced $insn (referenced by ${insn.uses}) from $this")

    insn.releaseRefs()
    body = body.filterNot(_ == insn)
  }

  override def validate(): Unit = {
    body.foreach(insn => {
      assert(insn.basicBlock == this, s"$insn in $this is owned by ${insn.basicBlock}")
      insn.validate()
    })
    assert(terminatorOption.isDefined, s"unterminated basic block $this")
    assert(terminatorOption.get == body.last, s"terminator is not the last instruction in basic block $this")
    assert(body.count(_.isInstanceOf[TerminatorInsn]) == 1, s"multiple terminator instructions in a basic block $this")
    assert(body.toSet.size == body.size, s"basic block $this contains duplicate instructions")
  }

  def releaseRefs(): Unit = {
    fun.bbNameGen.releaseName(name)
    body.foreach(_.releaseRefs())
  }

  def remove(removeUses: Boolean = false): Unit = fun.removeBlock(this, removeUses)

  override def toString: String = s"BasicBlock($name, fun=$fun)"
}

case class IrFunSignature(returnTy: IrTy, argTys: IndexedSeq[IrTy])

class IrFun(val _name: String, val signature: IrFunSignature, val program: IrProgram) extends IrObject with User[IrFunRef, IrFun] {
  def this(_name: String, returnTy: IrTy, argTys: IndexedSeq[IrTy], program: IrProgram) = this(_name, IrFunSignature(returnTy, argTys), program)

  val name: String = program.nameGen(_name)

  var basicBlocks: IndexedSeq[BasicBlock] = IndexedSeq.empty

  def returnTy: IrTy = signature.returnTy

  def argTys: IndexedSeq[IrTy] = signature.argTys

  def insns: Seq[Insn] = basicBlocks.flatMap(_.body)

  val entryBlockRef: EntryBlockRef = new EntryBlockRef(this, None)

  def entryBlock: BasicBlock = entryBlockRef.get

  def exitPoints: Seq[IrFunExitPoint] = basicBlocks.flatMap(_.terminatorOption).collect({ case ep: IrFunExitPoint => ep }).toSeq

  def locals: Seq[AllocLInsn] = insns.collect({ case al: AllocLInsn => al })

  /** [[NameGen]] instance for instruction names */
  val nameGen: NameGen = program.nameGen.newChild()

  /** [[NameGen]] instance for basic block names */
  val bbNameGen: NameGen = new NameGen

  def releaseRefs(): Unit = {
    program.nameGen.releaseName(name)
    entryBlockRef.release()
    basicBlocks.foreach(_.releaseRefs())
  }

  def append(block: BasicBlock): BasicBlock = {
    require(block.fun == this, s"cannot append $block owned by ${block.fun} to $this")

    if (!basicBlocks.contains(block))
      basicBlocks = basicBlocks :+ block
    if (entryBlockRef.isEmpty)
      entryBlockRef(block)
    block
  }

  def getBlockPred(block: BasicBlock): Seq[BasicBlock] = basicBlocks.filter(_.succ.contains(block))

  def getBlockLinSucc(basicBlock: BasicBlock): Option[BasicBlock] = {
    require(basicBlock.fun == this, s"cannot query successor of $basicBlock owned by ${basicBlock.fun} in $this")
    if (basicBlocks.last == basicBlock) None
    else {
      val idx = basicBlocks.indexOf(basicBlock)
      assert(idx != -1, s"cannot query successor of detached $basicBlock")
      Some(basicBlocks(idx + 1))
    }
  }

  def getBlockLinPred(basicBlock: BasicBlock): Option[BasicBlock] = {
    require(basicBlock.fun == this, s"cannot query predecessor of $basicBlock owned by ${basicBlock.fun} in $this")
    if (basicBlocks.head == basicBlock) None
    else {
      val idx = basicBlocks.indexOf(basicBlock)
      assert(idx != -1, s"cannot query predecessor of detached $basicBlock")
      Some(basicBlocks(idx - 1))
    }
  }

  def removeBlock(basicBlock: BasicBlock, removeUses: Boolean = false): Unit = {
    require(basicBlock.fun == this, s"cannot remove $basicBlock owned by ${basicBlock.fun} from $this")
    if (removeUses)
      basicBlock.removeUses()
    assert(basicBlock.uses.isEmpty, s"cannot remove still referenced $basicBlock (referenced by ${basicBlock.uses}) from $this")

    basicBlock.releaseRefs()
    basicBlocks = basicBlocks.filterNot(_ == basicBlock)
  }

  override def validate(): Unit = {
    assert(basicBlocks.toSet.size == basicBlocks.size, s"fun $this contains duplicate basic blocks")
    basicBlocks.foreach(bb => {
      assert(bb.fun == this, s"$bb is owned by ${bb.fun}")
      bb.validate()
    })
    assert(entryBlockRef.isDefined, s"fun $name doesn't have entry block")
    assert(entryBlock.pred.isEmpty, s"entry block $entryBlock of $this can't have any predecessors")
  }

  override def toString: String = s"IrFun($name)"
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

  def removeFun(fun: IrFun, removeUses: Boolean = false): Unit = {
    require(fun.program == this, s"cannot remove $fun owned by ${fun.program} from $this")
    if (removeUses)
      fun.removeUses()
    assert(fun.uses.isEmpty, s"cannot remove still referenced $fun (referenced by ${fun.uses}) from $this")

    fun.releaseRefs()
    funs = funs.filterNot(_ == fun)
  }

  def appendEntryFun(): IrFun = {
    assert(entryFunRef.isEmpty, s"entryFun of $this is already defined ($entryFun)")
    val _entryFun = append(new IrFun(IrProgram.entryFunName, IrTy.VoidTy, IndexedSeq.empty, this))
    entryFunRef(_entryFun)
    _entryFun
  }

  override def validate(): Unit = {
    funs.foreach(_.validate())
    assert(entryFunRef.isDefined, "entryFun is not defined")
  }
}

object IrProgram {
  val entryFunName: String = "entry"
}