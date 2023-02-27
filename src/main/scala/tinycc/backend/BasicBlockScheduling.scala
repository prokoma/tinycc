package tinycc.backend

import tinycc.common.ir.{BasicBlock, IrFun, IrProgram}

import scala.collection.mutable

/**
 * This pass filters and reorders basic blocks in each function, so the following constraints hold:
 * - fun.entryBlock is the first basic block
 * - the function contains no unreachable basic blocks
 * - the blocks are sorted so there is a high chance that one of the successors of each block immediately follows it
*/
class BasicBlockScheduling {
  def transformProgram(program: IrProgram): Unit = {
    program.funs.foreach(transformFun)
  }

  def transformFun(fun: IrFun): Unit = {
    val sortedBasicBlocks = mutable.IndexedBuffer.empty[BasicBlock]
    val visitedBlocks = mutable.Set.empty[BasicBlock]

    def dfs(bb: BasicBlock): Unit = {
      if(visitedBlocks.contains(bb))
        return
      visitedBlocks += bb
      sortedBasicBlocks += bb
      bb.terminator.get.succBlocks.foreach(dfs)
    }

    dfs(fun.entryBlock)
    fun.basicBlocks = sortedBasicBlocks
  }
}
