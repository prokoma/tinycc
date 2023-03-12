package tinycc.backend.t86

import tinycc.common.ir.IrTy

object T86Utils {
  def buildArgsMap(argTys: IndexedSeq[IrTy], offset: Long): IndexedSeq[Operand.MemRegImm] = {
    var argsSize: Long = 0
    argTys.map(ty => {
      val oldSize = argsSize
      argsSize += ty.sizeWords
      Operand.MemRegImm(Operand.BP, oldSize + offset)
    })
  }

  val machineRegCount: Int = 32

  val machineFRegCount: Int = 8

  val machineRegs: Set[Operand.Reg] = 0.until(machineRegCount).map(Operand.BasicReg(_)).toSet

  val machineFRegs: Set[Operand.FReg] = 0.until(machineFRegCount).map(Operand.BasicFReg(_)).toSet

  /** Special registers ignored by register allocation. */
  val specialRegs: Set[Operand.Reg] = Set(Operand.BP, Operand.SP, Operand.IP)

  val returnValueReg: Operand.BasicReg = Operand.BasicReg(0)

  val calleeSaveRegs: Set[Operand.Reg] = machineRegs - returnValueReg

  val callerSaveFRegs: Set[Operand.FReg] = machineFRegs
}
