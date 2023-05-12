package tinycc.common.transform

import tinycc.common.PhiHelper.tryRemoveTrivialPhi
import tinycc.common.ProgramTransform
import tinycc.common.ir.IrManipulation.{prependInsnTo, replaceInsnWith}
import tinycc.common.ir._
import tinycc.util.Profiler.profile

import scala.collection.mutable

/** Replaces locals with Phi nodes.
 * An implementation of https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf */
class MemToReg(removeLocals: Boolean = true) extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("memToReg", {
    program.funs.foreach(transformFun)
  })

  private def optimizeLocal(fun: IrFun, local: AllocLInsn): Unit = {
    val currentDef = mutable.Map.empty[BasicBlock, Insn]
    var sealedBlocks = Set.empty[BasicBlock]
    val incompletePhis = mutable.Map.empty[BasicBlock, PhiInsn]

    def writeVariable(block: BasicBlock, value: Insn): Unit = {
//      log(s"writeVariable $block $value")
      currentDef(block) = value
    }

    def readVariable(block: BasicBlock): Insn = {
//      log(s"readVariable $block")
      currentDef.getOrElse(block, readVariableRecursive(block))
    }

    def readVariableRecursive(block: BasicBlock): Insn = {
      assert(block.pred.nonEmpty, s"uninitialized load in $block")

      val value = if (!sealedBlocks.contains(block)) {
        val emptyPhi = prependInsnTo(new PhiInsn(block.pred.map(bb => (None, Some(bb))).toIndexedSeq, block), block)
        incompletePhis(block) = emptyPhi
        emptyPhi
      } else if (block.pred.size == 1) {
        readVariable(block.pred.head)
      } else {
        val emptyPhi = prependInsnTo(new PhiInsn(block.pred.map(bb => (None, Some(bb))).toIndexedSeq, block), block)
        writeVariable(block, emptyPhi)
        addPhiOperands(emptyPhi)
      }

      writeVariable(block, value)
      value
    }

    def addPhiOperands(phi: PhiInsn): Insn = {
      phi.basicBlock.pred.zipWithIndex.foreach({ case (pred, index) =>
        phi.argRefs(index)._1(readVariable(pred))
      })
      phi
    }

    def fillBlock(block: BasicBlock): Unit = {
//      log(s"enter $block")

      block.body.foreach({
        case insn: LoadInsn if insn.ptr == local =>
          val value = readVariable(block)
          log(s"replaced $insn with $value")
          insn.replaceUses(value)
          if (removeLocals)
            insn.remove()

        case insn: StoreInsn if insn.ptr == local =>
          writeVariable(block, insn.value)
          if (removeLocals)
            insn.remove()

        case _ =>
      })

//      log(s"exit $block")
    }

    def sealBlock(block: BasicBlock): Unit = {
      incompletePhis.get(block).foreach(addPhiOperands)
      sealedBlocks += block
    }

    fun.basicBlocks.foreach(fillBlock)
    fun.basicBlocks.foreach(sealBlock)
    fun.insns.foreach({
      case phi: PhiInsn => tryRemoveTrivialPhi(phi)
      case _ =>
    })

    if (removeLocals)
      local.remove()
  }

  def transformFun(fun: IrFun): Unit = {
    // only optimize locals that are used as address in Load and Store instructions, because we can be sure that they are not aliased
    val localsToOptimize = fun.locals.filter(_.uses.forall({
      case OperandRef(_: LoadInsn, _) => true
      case ref@OperandRef(owner: StoreInsn, _) if ref == owner.ptrRef => true
      case _ => false
    }))

    localsToOptimize.foreach(optimizeLocal(fun, _))
  }
}
