package tinycc.common.ir

trait BasicBlockBuilderOps {
  def bb: BasicBlock

  def append[T <: Insn](insn: T): T = bb.append(insn)

  def append[T <: Insn](insn: BasicBlock => T): T = append(insn(bb))
}

trait IrFunBuilderOps extends BasicBlockBuilderOps {
  def fun: IrFun

  var bbOption: Option[BasicBlock] = None

  def bb: BasicBlock = bbOption.get

  def appendBlock(newBlock: BasicBlock): BasicBlock = {
    fun.append(newBlock)
    enterBlock(newBlock)
    newBlock
  }

  def appendBlock(newBlock: IrFun => BasicBlock): BasicBlock = appendBlock(newBlock(fun))

  def appendBlock(name: String): BasicBlock =
    appendBlock(new BasicBlock(name, fun))

  def enterBlock(newBlock: BasicBlock): Unit = {
    bbOption = Some(newBlock)
  }

  def exitBlock(): Unit = {
    bbOption = None
  }
}

trait IrProgramBuilderOps extends IrFunBuilderOps {
  def program: IrProgram

  protected var funOption: Option[IrFun] = None

  def fun: IrFun = funOption.get

  def appendFun(newFun: IrFun): IrFun = {
    program.append(newFun)
    enterFun(newFun)
    newFun
  }

  def enterFun(newFun: IrFun): Unit = {
    funOption = Some(newFun)
    bbOption = newFun.basicBlocks.lastOption
  }

  def exitFun(): Unit = {
    exitBlock()
    funOption = None
  }
}

class BasicBlockBuilder(val bb: BasicBlock) extends BasicBlockBuilderOps

class IrFunBuilder(val fun: IrFun) extends IrFunBuilderOps

class IrProgramBuilder(val program: IrProgram) extends IrProgramBuilderOps