package tinycc.common.ir

/** Helper methods to move instructions inside and between basic blocks. */
object IrManipulation {
  private def moveInsnHelper(insn: Insn, newBlock: BasicBlock): Insn = {
    if (insn.basicBlock == newBlock)
      return insn
    val newInsn = insn.copy(newBlock)
    insn.replaceUses(newInsn)
    insn.remove()
    newInsn.name(insn.name)
  }

  private def insertReplaceInsnHelper(insn: Insn, marker: Insn, idxOffset: Int, replaced: Int): Insn = {
    val newBlock = marker.basicBlock
    val newInsn = moveInsnHelper(insn, newBlock)

    val filteredBody = newBlock.body.filterNot(_ == newInsn)
    val idx = filteredBody.indexOf(marker)
    require(idx != -1, s"insn $marker is not present in ${marker.basicBlock}")

    newBlock.body = filteredBody.patch(idx + idxOffset, Seq(newInsn), replaced)
    newInsn
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

  def insertInsnBefore(insn: Insn, insertBefore: Insn): Insn = insertReplaceInsnHelper(insn, insertBefore, 0, 0)

  def insertInsnAfter(insn: Insn, insertAfter: Insn): Insn = insertReplaceInsnHelper(insn, insertAfter, 1, 0)

//  def replaceInsnWith(insn: Insn, replacement: Insn): Insn = insertReplaceInsnHelper(replacement, insn, 0, 1)
}
