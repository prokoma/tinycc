package tinycc.common.ir

trait BasicBlockBuilder {
  def bb: BasicBlock

  def emit[T <: Insn](insn: T): T = bb.append(insn)

  def emit[T <: Insn](insn: BasicBlock => T): T = emit(insn(bb))

  def emitIImm(value: Long): IImmInsn = {
    val insn = emit(new IImmInsn(value, bb))

    if (value == 0) insn.name("izero")
    else if (value == 1) insn.name("ione")

    insn
  }

  def emitFImm(value: Double): FImmInsn = {
    val insn = emit(new FImmInsn(value, bb))

    if (value == 0) insn.name("fzero")
    else if (value == 1) insn.name("fone")

    insn
  }

  def emitBinaryArith(op: IrOpcode.BinaryArithOp, left: Insn, right: Insn): BinaryArithInsn =
    emit(new BinaryArithInsn(op, left, right, bb))

  def emitCmp(op: IrOpcode.CmpOp, left: Insn, right: Insn): CmpInsn =
    emit(new CmpInsn(op, left, right, bb))
}

object BasicBlockBuilder {
  def apply(_bb: BasicBlock): BasicBlockBuilder = new BasicBlockBuilder {
    override def bb: BasicBlock = _bb
  }
}

trait IrFunBuilder extends BasicBlockBuilder {
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

  def withBlock[T](newBlock: BasicBlock, thunk: => T): T = {
    val oldBbOption = bbOption
    enterBlock(newBlock)
    try
      thunk
    finally {
      bbOption = oldBbOption
    }
  }

  def enterBlock(newBlock: BasicBlock): Unit = {
    bbOption = Some(newBlock)
  }

  def exitBlock(): Unit = {
    bbOption = None
  }
}

object IrFunBuilder {
  def apply(_fun: IrFun): IrFunBuilder = new IrFunBuilder {
    override def fun: IrFun = _fun
  }
}

trait IrProgramBuilder extends IrFunBuilder {
  def program: IrProgram

  protected var funOption: Option[IrFun] = None

  def fun: IrFun = funOption.get

  def appendAndEnterFun(newFun: IrFun): IrFun = {
    program.append(newFun)
    enterFun(newFun)
    newFun
  }

  def appendAndEnterFun(newFun: IrProgram => IrFun): IrFun = appendAndEnterFun(newFun(program))

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

object IrProgramBuilder {
  def apply(_program: IrProgram): IrProgramBuilder = new IrProgramBuilder {
    override def program: IrProgram = _program
  }
}