package tinycc.backend.t86

import tinycc.common.ir.IrTy

object T86Utils {
  def getSizeWords(ty: IrTy): Long =
    (ty.sizeBytes / 8).ceil.toLong

  def buildArgsMap(argTys: IndexedSeq[IrTy], offset: Long): IndexedSeq[Operand.MemRegImm] = {
    var argsSize: Long = 0
    argTys.map(ty => {
      val oldSize = argsSize
      argsSize += getSizeWords(ty)
      Operand.MemRegImm(Operand.BP, oldSize + offset)
    })
  }

//  val machineRegCount: Int = 4
//
//  val machineFRegCount: Int = 2

  val machineRegCount: Int = 32

  val machineFRegCount: Int = 8

  val machineRegs: Set[Operand.Reg] = 0.until(machineRegCount).map(Operand.BasicReg(_)).toSet

  val machineFRegs: Set[Operand.FReg] = 0.until(machineFRegCount).map(Operand.BasicFReg(_)).toSet

  /** Special registers ignored by register allocation. */
  val specialRegs: Set[Operand.Reg] = Set(Operand.BP, Operand.SP, Operand.IP)

  val returnValueReg: Operand.BasicReg = Operand.BasicReg(0)
}
