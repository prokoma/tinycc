package tinycc.backend.t86

import tinycc.common.ir.{AllocGInsn, AllocLInsn, IrFun, IrProgram}

import scala.collection.mutable

trait T86ProgramBuilder {
  def program: T86Program

  protected val globalsMap = mutable.Map.empty[AllocGInsn, Operand.MemImm]
  protected val funs: mutable.Builder[T86Fun, IndexedSeq[T86Fun]] = IndexedSeq.newBuilder[T86Fun]
  protected var data: mutable.Builder[T86ListingElement, IndexedSeq[T86ListingElement]] = IndexedSeq.newBuilder[T86ListingElement]
  protected var dataSize: Long = program.dataSize

  funs ++= program.funs
  data ++= program.data

  def freshGlobal(size: Long, initData: Seq[Long] = Seq.empty): Operand.MemImm = {
    require(initData.size <= size)
    data ++= initData.map(T86DataWord(_))
    if (initData.size < size)
      data += T86DataWord(0, size - initData.size)
    dataSize += size
    Operand.MemImm(dataSize - size)
  }

  def resolveAllocG(insn: AllocGInsn): Operand.MemImm =
    globalsMap.getOrElseUpdate(insn, {
      val res = freshGlobal(insn.varTy.sizeWords, insn.initData)
      data += T86Comment(s"$res -> ${insn.name}")
      res
    })

  def appendFun(fun: T86Fun): Unit =
    funs += fun

  def result(): T86Program = {
    program.funs = funs.result()
    program.data = data.result()
    program
  }
}

object T86ProgramBuilder {
  def apply(_irProgram: IrProgram): T86ProgramBuilder = apply(Some(_irProgram))

  def apply(_irProgram: Option[IrProgram] = None): T86ProgramBuilder = apply(new T86Program(_irProgram))

  def apply(_program: T86Program): T86ProgramBuilder = new T86ProgramBuilder {
    override def program: T86Program = _program
  }
}

trait T86FunBuilder {
  def fun: T86Fun

  protected val localsMap = mutable.Map.empty[AllocLInsn, Operand.MemRegImm]
  protected val basicBlocks: mutable.Builder[T86BasicBlock, IndexedSeq[T86BasicBlock]] = IndexedSeq.newBuilder[T86BasicBlock]

  basicBlocks ++= fun.basicBlocks

  def freshReg(): Operand.Reg = fun.freshReg()

  def freshFReg(): Operand.FReg = fun.freshFReg()

  def freshLocal(size: Long): Operand.MemRegImm = fun.freshLocal(size)

  def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm =
    localsMap.getOrElseUpdate(insn, freshLocal(insn.varTy.sizeWords))

  def appendBlock(bb: T86BasicBlock): Unit =
    basicBlocks += bb

  def result(): T86Fun = {
    fun.basicBlocks = basicBlocks.result()
    fun
  }
}

object T86FunBuilder {
  def apply(_irFun: IrFun): T86FunBuilder = apply(Some(_irFun))

  def apply(_irFun: Option[IrFun] = None): T86FunBuilder = apply(new T86Fun(_irFun))

  def apply(_fun: T86Fun): T86FunBuilder = new T86FunBuilder {
    override def fun: T86Fun = _fun
  }
}