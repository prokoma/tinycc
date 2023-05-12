package tinycc.common.transform

import tinycc.common.PhiHelper.removeBlockPhiUsesIn
import tinycc.common.ProgramTransform
import tinycc.common.ir.IrManipulation.replaceInsnWith
import tinycc.common.ir.{BasicBlock, BrInsn, IrProgram, PhiInsn, Ref, RefTo}
import tinycc.util.Profiler.profile

class BasicBlockInlining extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("basicBlockInlining", {
    program.basicBlocks.foreach(simplifyBranchTargets)
  })

  protected def simplifyBranchTargets(block: BasicBlock): Unit = {
    block.terminator.succBlockRefs.map(ref => {
      val oldSucc = ref.get
      val resolved = resolveSuccBlock(List(oldSucc))
      if (resolved.sizeIs > 1 && resolved.head != oldSucc) {
        val newSucc = resolved.head
        log(s"replaced $ref with $newSucc")
        removeBlockPhiUsesIn(block, oldSucc)
        updateBlockPhiUsesIn(newSucc, resolved(1), block)
        ref(newSucc)
      }
    }).contains(true)
  }

  protected def resolveSuccBlock(path: List[BasicBlock]): List[BasicBlock] = {
    val block = path.head
    block.terminator match {
      case insn: BrInsn if block.body.size == 1 => resolveSuccBlock(insn.succBlock :: path)
      case _ => path
    }
  }

  protected def updateBlockPhiUsesIn(block: BasicBlock, oldPred: BasicBlock, newPred: BasicBlock): Unit = {
    oldPred.uses.foreach({
      case Ref(phiInsn: PhiInsn, _) if phiInsn.basicBlock == block =>
        val oldPredVal = phiInsn.argMap(oldPred)
        phiInsn.argMap.get(newPred) match {
          case Some(newPredVal) =>
            assert(newPredVal == oldPredVal, s"$phiInsn already contains $newPred with different value ($oldPredVal)")
          case None =>
            replaceInsnWith(phiInsn, PhiInsn(phiInsn.args :+ (oldPredVal, newPred), phiInsn.basicBlock))
        }
      case _ =>
    })
  }
}
