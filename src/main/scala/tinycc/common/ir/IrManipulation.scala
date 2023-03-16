package tinycc.common.ir

/** Helper methods to move instructions inside and between basic blocks. */
object IrManipulation {
  private def moveInsnHelper[T <: Insn](insn: T, newBlock: BasicBlock): T = {
    if (insn.basicBlock == newBlock)
      return insn
    val newInsn = insn.copy(newBlock).asInstanceOf[T]
    insn.replaceUses(newInsn)
    insn.remove()
    newInsn.name(insn.name)
  }

  private def insertInsnHelper[T <: Insn](insn: T, marker: Insn, idxOffset: Int): T = {
    val newBlock = marker.basicBlock
    val newInsn = moveInsnHelper(insn, newBlock)

    val filteredBody = newBlock.body.filterNot(_ == newInsn)
    val idx = filteredBody.indexOf(marker)
    assert(idx != -1, s"insn $marker is not present in ${marker.basicBlock}")

    newBlock.body = filteredBody.patch(idx + idxOffset, Seq(newInsn), 0)
    newInsn
  }

  def prependInsnTo[T <: Insn](insn: T, newBlock: BasicBlock): T = {
    val newInsn = moveInsnHelper(insn, newBlock)
    newBlock.body = newInsn +: newBlock.body
    newInsn
  }

  def appendInsnTo[T <: Insn](insn: T, newBlock: BasicBlock): T = {
    val newInsn = moveInsnHelper(insn, newBlock)
    newBlock.body = newBlock.body :+ newInsn
    newInsn
  }

  def insertInsnBefore[T <: Insn](insn: T, insertBefore: Insn): T = insertInsnHelper(insn, insertBefore, 0)

  def insertInsnAfter[T <: Insn](insn: T, insertAfter: Insn): T = insertInsnHelper(insn, insertAfter, 1)

  def insertInsnBeforeTerminator[T <: Insn](insn: T, newBlock: BasicBlock): T =
    newBlock.terminator match {
      case Some(terminator) => insertInsnBefore(insn, terminator)
      case None => appendInsnTo(insn, newBlock)
    }

  def replaceInsnWith(insn: Insn, replacement: Insn, replaceUses: Boolean = true): Insn = {
    if(insn == replacement)
      return insn

    require(replaceUses || insn.uses.isEmpty, s"insn $insn is still referenced and replaceUses = false")

    val newReplacement = insertInsnAfter(replacement, insn)
    if(replaceUses)
      insn.replaceUses(newReplacement)
    insn.remove()

    newReplacement
  }
}
