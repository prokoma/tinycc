package tinycc.common.ir

import tinycc.util.{IndentPrinter, IndentWriter, IterableForeachSep}

class IrPrinter extends IndentPrinter[IrObject] {

  def print(obj: IrObject, out: IndentWriter): Unit = obj match {
    case insn: Insn =>
      printInsn(insn, out)

    case block: BasicBlock =>
      printBasicBlock(block, out)

    case fun: IrFun =>
      printIrFun(fun, out)

    case program: IrProgram =>
      printIrProgram(program, out)
  }

  protected def printType(ty: IrTy, out: IndentWriter): Unit =
    out.write(ty.toString)

  protected def printFunSig(funSig: IrFunSignature, out: IndentWriter): Unit = {
    printType(funSig.returnType, out)
    out.write("(")
    funSig.argTypes.foreachSep(ty => printType(ty, out), out.write(", "))
    out.write(")")
  }

  protected def printInsnRef(insn: Insn, out: IndentWriter): Unit =
    out.write(s"%${insn.name}")

  protected def printInsnRef(ref: InsnRef, out: IndentWriter): Unit = ref() match {
    case Some(value) => printInsnRef(value, out)
    case None => out.write("null")
  }

  protected def printBasicBlockRef(bb: BasicBlock, out: IndentWriter): Unit =
    out.write(s"%${bb.name}")

  protected def printBasicBlockRef(ref: BasicBlockRef, out: IndentWriter): Unit = ref() match {
    case Some(value) => printBasicBlockRef(value, out)
    case None => out.write("null")
  }

  protected def printIrFunRef(fun: IrFun, out: IndentWriter): Unit =
    out.write(fun.name)

  protected def printIrFunRef(ref: IrFunRef, out: IndentWriter): Unit = ref() match {
    case Some(value) => printIrFunRef(value, out)
    case None => out.write("null")
  }

  protected def printOperands(insn: Insn, out: IndentWriter): Unit =
    insn.operandRefs.foreachSep(printInsnRef(_, out), out.write(", "))

  protected def printSuccBlocks(insn: TerminatorInsn, out: IndentWriter): Unit =
    insn.succBlockRefs.foreachSep(printBasicBlockRef(_, out), out.write(", "))

  protected def printInsn(insn: Insn, out: IndentWriter): Unit = {
    printInsnRef(insn, out)
    out.write(" = ")
    out.write(insn.op.toString)

    insn match {
      case insn: IImmInsn => out.write(s" ${insn.value}")
      case insn: FImmInsn => out.write(s" ${insn.value}")

      case insn: GetFunPtrInsn =>
        out.write(" ")
        printIrFunRef(insn.targetFun, out)

      case insn: GetArgPtrInsn => out.write(s" ${insn.index}")
      case insn: GetElementPtr =>
        out.write(s" ")
        printOperands(insn, out)
        out.write(s", ")
        printType(insn.elemTy, out)
        out.write(s", ${insn.fieldIndex}")

      case insn: CallInsn =>
        out.write(s" ")
        printIrFunRef(insn.targetFun, out)

      case insn: CallPtrInsn =>
        out.write(s" ")
        printOperands(insn, out)
        out.write(", ")
        printFunSig(insn.funSig, out)

      case insn: TerminatorInsn =>
        if (insn.operandRefs.nonEmpty) {
          out.write(" ")
          printOperands(insn, out)
        }
        if (insn.succBlockRefs.nonEmpty) {
          out.write(", ")
          printSuccBlocks(insn, out)
        }

      case insn =>
        if (insn.operandRefs.nonEmpty) {
          out.write(" ")
          printOperands(insn, out)
        }
    }
  }

  protected def printBasicBlock(bb: BasicBlock, out: IndentWriter): Unit = {
    out.write(s"${bb.name}:")
    out.withIndent({
      bb.body.foreachSep(printInsn(_, out), out.write("\n"))
    })
  }

  protected def printIrFun(fun: IrFun, out: IndentWriter): Unit = {
    printType(fun.returnTy, out)
    out.write(s" ${fun.name}(")
    fun.argTys.foreachSep(printType(_, out), out.write(", "))
    out.write(") {")
    out.withIndent({
      fun.basicBlocks.foreachSep(printBasicBlock(_, out), out.write("\n"))
    })
    out.write("}")
  }

  protected def printIrProgram(program: IrProgram, out: IndentWriter): Unit = {
    program.funs.foreachSep(printIrFun(_, out), out.write("\n"))
  }
}
