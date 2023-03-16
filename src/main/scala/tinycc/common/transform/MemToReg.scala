package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.ir.IrManipulation.prependInsnTo
import tinycc.common.ir._

import scala.collection.mutable

class MemToReg(removeLocals: Boolean = true) extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit =
    program.funs.foreach(transformFun)

  def transformFun(fun: IrFun): Unit = {
    val currentDef = mutable.Map.empty[AllocLInsn, Map[BasicBlock, Insn]].withDefaultValue(Map.empty)
    val incompletePhis = mutable.Map.empty[BasicBlock, Map[AllocLInsn, PhiInsn]].withDefaultValue(Map.empty)
    var openBlocks = Set.empty[BasicBlock]
    var sealedBlocks = Set.empty[BasicBlock]

    // collect allocl instructions, which are only used as operands of load and store instructions
    val localsToOptimize: Set[Insn] = fun.locals.filter(_.uses.forall({
      case OperandRef(_: LoadInsn | _: StoreInsn, _) => true
      case _ => false
    })).toSet

    def writeVariable(variable: AllocLInsn, block: BasicBlock, value: Insn): Unit =
      currentDef(variable) += (block -> value)

    def readVariable(variable: AllocLInsn, block: BasicBlock): Insn = {
      if(!openBlocks.contains(block))
        fillBlock(block)

      currentDef(variable).getOrElse(block, readVariableRecursive(variable, block))
    }

    def readVariableRecursive(variable: AllocLInsn, block: BasicBlock): Insn = {
      assert(block.pred.nonEmpty, s"uninitialized load of $variable in $block")

      log(s"$block ${block.pred}")

      val value = /*if(!sealedBlocks.contains(block)) {
        val emptyPhi = insertInsnBefore(new PhiInsn(block.pred.map(bb => (None, Some(bb))).toIndexedSeq, block), loc)
        incompletePhis(block) += (variable -> emptyPhi)
        emptyPhi
      } else */if (block.pred.size == 1) {
        readVariable(variable, block.pred.head)
      } else {
        val emptyPhi = prependInsnTo(new PhiInsn(block.pred.map(bb => (None, Some(bb))).toIndexedSeq, block), block)
        writeVariable(variable, block, emptyPhi)
        addPhiOperands(variable, emptyPhi)
      }

      writeVariable(variable, block, value)
      value
    }

    def addPhiOperands(variable: AllocLInsn, phi: PhiInsn): Insn = {
      phi.basicBlock.pred.zipWithIndex.foreach({ case (pred, index) =>
        phi.argRefs(index)._1(readVariable(variable, pred))
      })
      tryRemoveTrivialPhi(phi)
    }

    def tryRemoveTrivialPhi(phi: PhiInsn): Insn = {
      phi
    }

    def fillBlock(block: BasicBlock): Unit = {
      openBlocks += block

      block.body.foreach({
        case insn: LoadInsn if localsToOptimize.contains(insn.ptr) =>
          val value = readVariable(insn.ptr.asInstanceOf[AllocLInsn], block)
          log(s"replaced $insn with $value")
          insn.replaceUses(value)
          if(removeLocals)
            insn.remove()

        case insn: StoreInsn if localsToOptimize.contains(insn.ptr) =>
          writeVariable(insn.ptr.asInstanceOf[AllocLInsn], block, insn.value)
          if(removeLocals)
            insn.remove()

        case _ =>
      })

//      sealBlock(block)
    }

    def sealBlock(block: BasicBlock): Unit = {
      incompletePhis(block).foreach({ case (variable, phi) =>
        addPhiOperands(variable, phi)
      })
      sealedBlocks += block
    }

    fun.basicBlocks.foreach(bb => {
      fillBlock(bb)
    })

    if(removeLocals)
      localsToOptimize.foreach(_.remove())
  }
}
