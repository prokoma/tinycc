package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.ir.IrManipulation.appendInsnTo
import tinycc.common.ir._
import tinycc.util.Profiler.profile

import scala.collection.mutable

class FunInlining extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit = profile("funInlining", {
    program.basicBlocks.foreach(transformBlock)
  })

  private def transformBlock(block: BasicBlock): Unit = {
    val callsToInline = getCallsToInline(block)
    if(callsToInline.isEmpty)
      return

    // detach all instructions in the basic block and start rewriting
    val body = block.body
    block.body = IndexedSeq.empty

    val builder = IrFunBuilder(block.fun)
    builder.enterBlock(block)

    body.foreach({
      case insn: CallInsn if callsToInline.contains(insn) =>
        log(s"inlined call $insn in $block")
        val brInsn = builder.emit(new BrInsn(None, builder.bb))
        val contBlock = new BasicBlock("cont", builder.fun)

        // rewrite phi references of the current block with contBlock
        builder.bb.uses.foreach({
          case ref@OperandBlockRef(_: PhiInsn, _) => ref(contBlock)
          case _ =>
        })

        brInsn.succBlockRef(compileInlinedCallInsn(builder, insn, contBlock))
        builder.appendAndEnterBlock(contBlock)

      case insn =>
        // move insn to the new block
        appendInsnTo(insn, builder.bb)
    })
  }

  private def compileInlinedCallInsn(builder: IrFunBuilder, callInsn: CallInsn, contBlock: BasicBlock): BasicBlock = {
    val blockRewriteMap = mutable.Map.empty[BasicBlock, BasicBlock]
    val insnRewriteMap = mutable.Map.empty[Insn, Insn]

    val targetFun = callInsn.targetFun
    assert(targetFun.exitPoints.size == 1, s"cannot inline $targetFun with multiple exit points (${targetFun
      .exitPoints})")

    // COPY basic blocks and functions from the inlined function
    targetFun.basicBlocks.foreach(bb => {
      blockRewriteMap(bb) = builder.appendAndEnterBlock(new BasicBlock(bb.uniqueName, builder.fun))

      bb.body.foreach({
        case insn: LoadArgInsn =>
          insnRewriteMap(insn) = callInsn.args(insn.index)

        case _: RetVoidInsn | _: RetInsn =>
          builder.emit(new BrInsn(contBlock, builder.bb))

        case insn =>
          insnRewriteMap(insn) = builder.emit(insn.copy(builder.bb))
      })
    })

    // rewrite newly created uses of instructions and basic blocks
    blockRewriteMap.foreach({ case (oldBlock, newBlock) =>
      oldBlock.uses.foreach({
        case ref@OperandBlockRef(owner, _) if owner.fun == builder.fun => ref(newBlock)
        case _ =>
      })
    })
    insnRewriteMap.foreach({ case (oldInsn, newInsn) =>
      oldInsn.uses.foreach({
        case ref@OperandRef(owner, _) if owner.fun == builder.fun => ref(newInsn)
        case _ =>
      })
    })

    // replace the original call instruction with the argument of return insn
    targetFun.exitPoints.foreach({
      case insn: RetInsn =>
        // correctly handle when function returns allocg defined in other function
        callInsn.replaceUses(insnRewriteMap.getOrElse(insn.arg, insn.arg))

      case _ =>
    })
    callInsn.remove()

    // return the new entry block
    blockRewriteMap(targetFun.entryBlock)
  }

  private def getCallsToInline(block: BasicBlock): Set[CallInsn] = {
    val calls = block.body.collect({ case insn: CallInsn => insn })
    val byTargetFun = calls.groupBy(_.targetFun)

    def isLeafFun(fun: IrFun): Boolean = fun.insns.forall({
      case _: CallInsn | _: CallPtrInsn => false
      case _ => true
    })

    calls.filter(call => {
      val targetFun = call.targetFun
      (isLeafFun(targetFun) || targetFun.uses.size == 1) && (byTargetFun(targetFun).size == 1 || byTargetFun(targetFun).size * targetFun.insns.size < 42)
    }).toSet
  }
}
