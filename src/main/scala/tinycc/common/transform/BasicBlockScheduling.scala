package tinycc.common.transform

import tinycc.common.ProgramTransform
import tinycc.common.ir.{BasicBlock, IrException, IrFun, IrProgram}

import scala.collection.mutable

/**
 * This pass filters and reorders basic blocks in each function, so the following constraints hold:
 * - fun.entryBlock is the first basic block
 * - the function contains no unreachable basic blocks
 * - the blocks are sorted so there is a high chance that one of the successors of each block immediately follows it
 */
class BasicBlockScheduling extends ProgramTransform[IrProgram] {
  override def transformProgram(program: IrProgram): Unit =
    program.funs.foreach(transformFun)

  def transformFun(fun: IrFun): Unit = {
    val sortedBasicBlocks = IndexedSeq.newBuilder[BasicBlock]
    val visitedBlocks = mutable.Set.empty[BasicBlock]

    def dfs(bb: BasicBlock): Unit = {
      if (visitedBlocks.contains(bb))
        return
      visitedBlocks += bb
      sortedBasicBlocks += bb
      val terminator = bb.terminatorOption.getOrElse(throw new IrException(s"unterminated, but reachable basic block ${bb.uniqueName}"))
      terminator.succBlocks.foreach(dfs)
    }

    dfs(fun.entryBlock)
    for (bb <- fun.basicBlocks if !visitedBlocks.contains(bb)) {
      log(s"removed unreachable $bb")
      bb.releaseRefs()
    }
    fun.basicBlocks = sortedBasicBlocks.result()
  }
}
