package tinycc.common.ir

trait BasicBlockBuilderOps {
  def bb: BasicBlock

  def emit[T <: Insn](insn: T): T = bb.append(insn)

  def emit[T <: Insn](insn: BasicBlock => T): T = emit(insn(bb))

  def emitIImm(value: Long): IImmInsn = emit(new IImmInsn(value, bb))

  def emitFImm(value: Double): FImmInsn = emit(new FImmInsn(value, bb))

  def emitBinaryArith(op: IrOpcode.BinaryArithOp, left: Insn, right: Insn): BinaryArithInsn =
    emit(new BinaryArithInsn(op, left, right, bb))

  def emitCmp(op: IrOpcode.CmpOp, left: Insn, right: Insn): CmpInsn =
    emit(new CmpInsn(op, left, right, bb))
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

  def withFun[T](newFun: IrFun, thunk: => T): T = {
    val oldFunOption = funOption
    val oldBbOption = bbOption
    enterFun(newFun)
    try
      thunk
    finally {
      funOption = oldFunOption
      bbOption = oldBbOption
    }
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