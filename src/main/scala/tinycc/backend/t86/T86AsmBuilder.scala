package tinycc.backend.t86

import tinycc.common.ir.{AllocGInsn, AllocLInsn, IrFun, IrProgram}

import scala.collection.mutable

class T86ProgramBuilder(irProgram: Option[IrProgram] = None) {

  protected var _data: IndexedSeq[Long] = IndexedSeq.empty
  protected val _globalsMap = mutable.Map.empty[AllocGInsn, Operand.MemImm]
  protected val _funs: mutable.Builder[T86Fun, IndexedSeq[T86Fun]] = IndexedSeq.newBuilder[T86Fun]

  def freshGlobal(data: Seq[Long]): Operand.MemImm = {
    val offset = _data.size
    _data ++= data
    Operand.MemImm(offset)
  }

  def resolveAllocG(insn: AllocGInsn): Operand.MemImm =
    _globalsMap.getOrElseUpdate(insn, freshGlobal(insn.initData))

  def appendFun(fun: T86Fun): Unit =
    _funs += fun

  def result(): T86Program = new T86Program(_funs.result(), _data, irProgram)
}

class T86FunBuilder(irFun: Option[IrFun] = None) {

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