package tinycc.backend.t86

import tinycc.common.ir.{AllocGInsn, AllocLInsn, IrFun, IrProgram}

import scala.collection.mutable

class T86ProgramBuilder(irProgram: Option[IrProgram] = None) {
  def this(irProgram: IrProgram) = this(Some(irProgram))

  protected var _data: mutable.Builder[T86ListingElement, IndexedSeq[T86ListingElement]] = IndexedSeq.newBuilder[T86ListingElement]
  protected var _globalsSize: Long = 0
  protected val _globalsMap = mutable.Map.empty[AllocGInsn, Operand.MemImm]
  protected val _funs: mutable.Builder[T86Fun, IndexedSeq[T86Fun]] = IndexedSeq.newBuilder[T86Fun]

  def freshGlobal(size: Long, initData: Seq[Long] = Seq.empty): Operand.MemImm = {
    require(initData.size <= size)
    _data ++= initData.map(T86DataWord(_))
    if (initData.size < size)
      _data += T86DataWord(0, size - initData.size)
    _globalsSize += size
    Operand.MemImm(_globalsSize - size)
  }

  def resolveAllocG(insn: AllocGInsn): Operand.MemImm =
    _globalsMap.getOrElseUpdate(insn, {
      val res = freshGlobal(insn.varTy.sizeWords, insn.initData)
      _data += T86Comment(s"$res -> ${insn.name}")
      res
    })

  def appendFun(fun: T86Fun): Unit =
    _funs += fun

  def result(): T86Program = new T86Program(_funs.result(), _data.result(), irProgram)
}

class T86FunBuilder(irFun: Option[IrFun] = None) {
  def this(irFun: IrFun) = this(Some(irFun))

  protected var localsSize: Long = 0
  protected val localsMap = mutable.Map.empty[AllocLInsn, Operand.MemRegImm]
  protected var nextReg: Long = T86Utils.machineRegCount
  protected var nextFreg: Long = T86Utils.machineFRegCount
  protected val basicBlocks: mutable.Builder[T86BasicBlock, IndexedSeq[T86BasicBlock]] = IndexedSeq.newBuilder[T86BasicBlock]

  def freshReg(): Operand.Reg = {
    nextReg += 1
    Operand.BasicReg(nextReg - 1)
  }

  def freshFReg(): Operand.FReg = {
    nextFreg += 1
    Operand.BasicFReg(nextFreg - 1)
  }

  def freshLocal(size: Long): Operand.MemRegImm = {
    localsSize += size
    Operand.MemRegImm(Operand.BP, -localsSize)
  }

  def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm =
    localsMap.getOrElseUpdate(insn, freshLocal(insn.varTy.sizeWords))

  def appendBlock(bb: T86BasicBlock): Unit =
    basicBlocks += bb

  def result(): T86Fun = new T86Fun(basicBlocks.result(), localsSize, nextReg, nextFreg, irFun)
}