package tinycc.common.ir

object IrManipulation {
  private def moveInsnHelper(insn: Insn, newBlock: BasicBlock): Insn = {
    if (insn.basicBlock == newBlock)
      return insn
    val newInsn = insn.copy(newBlock)
    insn.replaceUses(newInsn)
    insn.remove()
    newInsn.name(insn.name)
  }

  def prependInsnTo(insn: Insn, newBlock: BasicBlock): Insn = {
    val newInsn = moveInsnHelper(insn, newBlock)
    newBlock.body = newInsn +: newBlock.body
    newInsn
  }

  def appendInsnTo(insn: Insn, newBlock: BasicBlock): Insn = {
    val newInsn = moveInsnHelper(insn, newBlock)
    newBlock.body = newBlock.body :+ newInsn
    newInsn
  }

//  def insertInsnBefore(insn: Insn, insertBefore: Insn): Insn = {
//    val newBlock = insertBefore.basicBlock
//    val newInsn = moveInsnHelper(insn, newBlock)
//
//    newBlock.body = newBlock.body.filterNot(_ == newInsn)
//  }
//
//  def insertInsnAfter(insn: Insn, insertAfter: Insn): Insn = {
//
//  }
}
