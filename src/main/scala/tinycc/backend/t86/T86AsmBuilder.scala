package tinycc.backend.t86

import tinycc.common.ir.{AllocGInsn, AllocLInsn, IrFun, IrProgram}

import scala.collection.mutable

class T86ProgramBuilder(irProgram: Option[IrProgram] = None) {

  import T86Utils._

  protected var globalsSize: Long = 0
  protected val globalsMap = mutable.Map.empty[AllocGInsn, Operand.MemImm]
  protected val funs: mutable.Builder[T86Fun, IndexedSeq[T86Fun]] = IndexedSeq.newBuilder[T86Fun]

  def freshGlobal(size: Long): Operand.MemImm = {
    globalsSize += size
    Operand.MemImm(globalsSize - size)
  }

  def resolveAllocG(insn: AllocGInsn): Operand.MemImm =
    globalsMap.getOrElseUpdate(insn, freshGlobal(getSizeWords(insn.varTy)))

  def appendFun(fun: T86Fun): Unit =
    funs += fun

  def result(): T86Program = new T86Program(funs.result(), globalsSize, irProgram)
}

class T86FunBuilder(irFun: Option[IrFun] = None) {

  import T86Utils._

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
    localsMap.getOrElseUpdate(insn, freshLocal(getSizeWords(insn.varTy)))

  def appendBlock(bb: T86BasicBlock): Unit =
    basicBlocks += bb

  def result(): T86Fun = new T86Fun(basicBlocks.result(), localsSize, irFun)
}