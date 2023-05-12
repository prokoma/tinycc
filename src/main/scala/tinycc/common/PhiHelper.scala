package tinycc.common

import tinycc.common.ir.IrManipulation.replaceInsnWith
import tinycc.common.ir._

object PhiHelper {
  protected def removeBlockPhiUse(block: BasicBlock, phiInsn: PhiInsn): Unit = {
    val newArgs = phiInsn.argRefs.filterNot({
      case (_, RefTo(bb)) if bb == block => true
      case _ => false
    }).map({ case (valueRef, predRef) => (valueRef(), predRef()) })
    replaceInsnWith(phiInsn, tryRemoveTrivialPhi(new PhiInsn(newArgs, phiInsn.basicBlock)))
  }

  def tryRemoveTrivialPhi(phiInsn: PhiInsn): Insn = {
    if (!phiInsn.argRefs.forall(_._1.isDefined)) // incomplete phiInsn
      return phiInsn

    val uniqueOperands = phiInsn.operands.filterNot(_ == phiInsn).toSet
    if (uniqueOperands.isEmpty) {
      // self-referencing phiInsn
      phiInsn
    } else if (uniqueOperands.size == 1) {
      val phiPhiUsers = phiInsn.uses.collect({ case OperandRef(owner: PhiInsn, _) if owner != phiInsn => owner })
      phiInsn.replaceUses(uniqueOperands.head)
      // clear references and remove from program
      phiInsn.remove()
      // recurse into users of the phiInsn instruction
      // we have now one phiInsn less, so this has to stop at some point
      phiPhiUsers.foldLeft(uniqueOperands.head)((uniq, user) => {
        val newUser = tryRemoveTrivialPhi(user)
        // check if we removed uniqueOperands.head from the graph,
        // because we don't want to return detached instructions
        if (user == uniq) newUser else uniq
      })
    } else {
      // non-trivial phiInsn
      phiInsn
    }
  }

  def removeBlockPhiUses(block: BasicBlock): Unit = {
    block.uses.foreach({
      case Ref(phiInsn: PhiInsn, _) => removeBlockPhiUse(block, phiInsn)
      case _ =>
    })
  }

  def removeBlockPhiUsesIn(block: BasicBlock, succBlock: BasicBlock): Unit = {
    block.uses.foreach({
      case Ref(phiInsn: PhiInsn, _) if phiInsn.basicBlock == succBlock =>
        removeBlockPhiUse(block, phiInsn)
      case _ =>
    })
  }
}
