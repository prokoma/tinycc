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
  def apply(): Option[T]

  def apply(t: Option[T]): Unit

  def apply(t: T): Unit = apply(Some(t))

  def release(): Unit = apply(None)

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
class OperandRef(val insn: Insn, target: Option[Insn]) extends InsnRef(target) {
  def apply(insn: Insn, target: Insn) = new OperandRef(insn, Some(target))

  override def get: Insn = apply()
    .getOrElse(throw new IrException(s"Cannot access null operand of $insn"))
}

/** A smart reference to BasicBlock. */
class BasicBlockRef(private var target: Option[BasicBlock]) extends Ref[BasicBlock] {
  def this(target: BasicBlock) = this(Some(target))

  target.foreach(_.uses.add(this))

  def apply(): Option[BasicBlock] = target

  def apply(t: Option[BasicBlock]): Unit = {
    target.foreach(_.uses.remove(this))
    target = t
    target.foreach(_.uses.add(this))
  }
}

class TerminatorSuccRef(val insn: TerminatorInsn, target: Option[BasicBlock]) extends BasicBlockRef(target) {
  def this(insn: TerminatorInsn, target: BasicBlock) = this(insn, Some(target))
}

class IrFunRef(private var target: Option[IrFun]) extends Ref[IrFun] {
  def this(target: IrFun) = this(Some(target))

  if (target.isDefined)
    target.get.uses.add(this)

  def apply(): Option[IrFun] = target

  def apply(t: Option[IrFun]): Unit = {
    target.foreach(_.uses.remove(this))
    target = t
    target.foreach(_.uses.add(this))
  }
}

trait Insn extends IrObject with UseTracking[InsnRef, Insn] {
  def op: IrOpcode

  def pred: Option[Insn] = basicBlock.getInsnPred(this)

  def succ: Option[Insn] = basicBlock.getInsnSucc(this)

  def basicBlock: BasicBlock

  def fun: IrFun = basicBlock.fun

  def operandRefs: IndexedSeq[OperandRef] = IndexedSeq.empty

  def operands: IndexedSeq[Insn] = operandRefs.flatMap(_())

  def hasSideEffects: Boolean = false

  def resultTy: IrTy

  val name: String = fun.nameGen()

  override def validate(): Unit = () // TODO: check if all operands are defined

  def remove(force: Boolean = false): Unit = basicBlock.removeInsn(this, force)

  def replace(newInsn: Insn, replaceUses: Boolean = false): Unit = basicBlock.replaceInsn(this, newInsn, replaceUses)

  def copy(newBlock: BasicBlock): Insn

  def releaseRefs(): Unit =
    operandRefs.foreach(_.apply(None))

  override def toString: String = s"${getClass.getSimpleName}(${super.toString})"
}

/* === BasicBlock & IrFun === */

class BasicBlock(_name: String, val fun: IrFun) extends IrObject with UseTracking[BasicBlockRef, BasicBlock] {
  val name: String = fun.nameGen(_name + "$")
  val body: mutable.IndexedBuffer[Insn] = mutable.IndexedBuffer.empty[Insn]

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
    body.addOne(insn)
    insn
  }

  override def validate(): Unit = {
    body.foreach(_.validate())
    if (terminator.isEmpty)
      throw new IrException(s"Unterminated BasicBlock '$name' in '${fun.name}'.")
  }

  def releaseRefs(): Unit =
    body.foreach(_.releaseRefs())

  def insertInsnBefore(newInsn: Insn, mark: Insn): Insn = {
    if (newInsn.basicBlock != this)
      throw new IrException(s"Cannot insert '$newInsn' owned by '${newInsn.basicBlock.name}.'")
    body.subtractOne(newInsn)

    findInsn(mark) match {
      case Some(idx) =>
        body.insert(idx, newInsn)
        newInsn
      case None =>
        throw new IrException(s"Invalid insertion marker '$mark' owned by '${mark.basicBlock.name}'.")
    }
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

  def removeInsn(insn: Insn, force: Boolean = false): Unit = {
    if (insn.basicBlock != this)
      throw new IrException(s"Cannot remove insn '$insn' owned by '${insn.basicBlock.name}' from '$name'.")

    if (force)
      insn.removeUses()
    else if (insn.uses.nonEmpty)
      throw new IrException(s"Cannot remove still referenced insn '$insn'.")

    insn.releaseRefs()
    body.subtractOne(insn)
  }

  def replaceInsn(insn: Insn, newInsn: Insn, replaceUses: Boolean = false): Unit = {
    if (newInsn.basicBlock != this)
      throw new IrException(s"Cannot replace insn '$insn' with '$newInsn' owned by '${newInsn.basicBlock.name}' inside '$name'.")

    if (replaceUses)
      insn.replaceUses(newInsn)
    else if (insn.uses.nonEmpty)
      throw new IrException(s"Cannot replace still referenced insn '$insn'.")

    insn.releaseRefs()
    val idx = findInsn(insn).get
    body(idx) = newInsn
  }

  override def toString: String = s"BasicBlock($name, fun=$fun)"
}

case class IrFunSignature(returnType: IrTy, argTypes: Seq[IrTy])

class IrFun(val name: String, val returnTy: IrTy, val argTys: IndexedSeq[IrTy], val program: IrProgram) extends IrObject with UseTracking[IrFunRef, IrFun] {
  var basicBlocks: mutable.IndexedBuffer[BasicBlock] = mutable.IndexedBuffer.empty

  def signature: IrFunSignature = IrFunSignature(returnTy, argTys)

  def insns: Iterable[Insn] = basicBlocks.flatMap(_.body)

  def getBlockPred(basicBlock: BasicBlock): Seq[BasicBlock] = basicBlocks.filter(_.succ.contains(basicBlock)).toSeq

  val entryBlockRef: BasicBlockRef = new BasicBlockRef(None)

  def entryBlock: BasicBlock = entryBlockRef.get

  def exitPoints: Seq[IrFunExitPoint] = basicBlocks.flatMap(_.terminator).collect({ case ep: IrFunExitPoint => ep }).toSeq

  def locals: Iterable[AllocLInsn] = insns.collect({ case al: AllocLInsn => al })

  val nameGen: NameGen = new NameGen

  def append(basicBlock: BasicBlock): BasicBlock = {
    if (!basicBlocks.contains(basicBlock))
      basicBlocks.addOne(basicBlock)
    if (entryBlockRef.isEmpty)
      entryBlockRef(basicBlock)
    basicBlock
  }

  override def validate(): Unit = {
    if(basicBlocks.toSet.size != basicBlocks.size)
      throw new IrException(s"$this: Function contains duplicate basic blocks.")
    basicBlocks.foreach(_.validate())
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

  def removeBlock(basicBlock: BasicBlock, force: Boolean = false): Unit = {
    if (basicBlock.fun != this)
      throw new IrException(s"Cannot remove block '${basicBlock.name}' owned by '${basicBlock.fun.name}' from '$name'.")

    if (force)
      basicBlock.removeUses()
    else if (basicBlock.uses.nonEmpty)
      throw new IrException(s"Cannot remove still referenced block '${basicBlock.name}'.")

    basicBlock.releaseRefs()
    basicBlocks.subtractOne(basicBlock)
  }

  def insertBlockBefore(newBlock: BasicBlock, mark: BasicBlock): BasicBlock = {
    if (newBlock.fun != this)
      throw new IrException(s"Cannot insert '$newBlock' owned by '${newBlock.fun}.'")
    basicBlocks.subtractOne(newBlock)

    findBlock(mark) match {
      case Some(idx) =>
        basicBlocks.insert(idx, newBlock)
        newBlock
      case None =>
        throw new IrException(s"Invalid insertion marker '$mark' owned by '${mark.fun}'.")
    }
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
  val funs: mutable.IndexedBuffer[IrFun] = mutable.IndexedBuffer.empty[IrFun]

  def basicBlocks: Iterable[BasicBlock] = funs.flatMap(_.basicBlocks)

  def insns: Iterable[Insn] = funs.flatMap(_.insns)

  val entryFunRef: IrFunRef = new IrFunRef(None)

  def entryFun: IrFun = entryFunRef.get

  def globals: Iterable[AllocGInsn] = insns.collect({ case al: AllocGInsn => al })

  def append(fun: IrFun): IrFun = {
    if (!funs.contains(fun))
      funs.addOne(fun)
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
    if (entryFunRef.isEmpty)
      throw new IrException("Missing entryFun declaration in program.")
  }

  def removeFun(fun: IrFun, force: Boolean = false): Unit = {
    if (fun.program != this)
      throw new IrException(s"Cannot remove fun '${fun.name}' owned by '${fun.program}' from '$this'.")

    if (force)
      fun.removeUses()
    else if (fun.uses.nonEmpty)
      throw new IrException(s"Cannot remove still referenced function '${fun.name}'.")

    funs.subtractOne(fun)
  }
}

object IrProgram {
  val entryFunName: String = "<entry>"
}