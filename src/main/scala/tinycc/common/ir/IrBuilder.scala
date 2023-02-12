package tinycc.common.ir

trait BasicBlockBuilderOps {
  def bb: BasicBlock

  def append[T <: Insn](insn: T): T = bb.append(insn)

  def append[T <: Insn](insn: BasicBlock => T): T = append(insn(bb))

  def appendIImm(value: Long): IImmInsn = append(new IImmInsn(value, bb))

  def appendFImm(value: Double): FImmInsn = append(new FImmInsn(value, bb))

  def appendBinaryArith(op: IrOpcode.BinaryArithOp, left: Insn, right: Insn): BinaryArithInsn =
    append(new BinaryArithInsn(op, left, right, bb))

  def appendCmp(op: IrOpcode.CmpOp, left: Insn, right: Insn): CmpInsn =
    append(new CmpInsn(op, left, right, bb))
}

trait IrFunBuilderOps extends BasicBlockBuilderOps {
  def fun: IrFun

  var bbOption: Option[BasicBlock] = None

  def bb: BasicBlock = bbOption.get

  def appendAndEnterBlock(newBlock: BasicBlock): BasicBlock = {
    fun.append(newBlock)
    enterBlock(newBlock)
    newBlock
  }

  def appendAndEnterBlock(newBlock: IrFun => BasicBlock): BasicBlock = appendAndEnterBlock(newBlock(fun))

  def appendAndEnterBlock(name: String): BasicBlock =
    appendAndEnterBlock(new BasicBlock(name, fun))

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