package tinycc.backend.t86

import tinycc.common.ir.IrTy

object T86Utils {
  /** Determines stack location of function arguments. */
  def buildArgsMap(argTys: IndexedSeq[IrTy], offset: Long): IndexedSeq[Operand.MemRegImm] = {
    var argsSize: Long = 0
    argTys.map(ty => {
      val oldSize = argsSize
      argsSize += ty.sizeWords
      Operand.MemRegImm(Operand.BP, oldSize + offset)
    })
  }

  val defaultMachineRegCount: Int = 4

  val defaultMachineFRegCount: Int = 5

  /** The register used to pass return value back to the caller. */
  val returnValueReg: Operand.MachineReg = Operand.MachineReg(0)
}
