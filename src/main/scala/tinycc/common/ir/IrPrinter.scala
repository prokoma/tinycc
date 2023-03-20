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
    printType(funSig.returnTy, out)
    out.write("(")
    funSig.argTys.foreachSep(ty => printType(ty, out), out.write(", "))
    out.write(")")
  }

  protected def printInsnRef(insn: Insn, out: IndentWriter): Unit =
    out.write(s"%${insn.name}")

  protected def printInsnRef(ref: InsnRef, out: IndentWriter): Unit = ref() match {
    case Some(value) => printInsnRef(value, out)
    case None => out.write("null")
  }

  protected def printBasicBlockRef(bb: BasicBlock, out: IndentWriter): Unit =
    out.write(s"label %${bb.name}")

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
      // %0 = iimm 42
      case insn: IImmInsn => out.write(s" ${insn.value}")

      // %0 = fimm 42.5
      case insn: FImmInsn => out.write(s" ${insn.value}")

      // %0 = allocl i64
      case insn: AllocLInsn =>
        out.write(" ")
        printType(insn.varTy, out)

      // %0 = allocg i64, 42
      case insn: AllocGInsn =>
        out.write(" ")
        printType(insn.varTy, out)
        if (insn.initData.nonEmpty) {
          out.write(", ")
          insn.initData.foreachSep(l => out.write(l.toString), out.write(" "))
        }

      // %0 = sizeof i64
      case insn: SizeOfInsn =>
        out.write(" ")
        printType(insn.varTy, out)

      // %0 = getfunptr foo
      case insn: GetFunPtrInsn =>
        out.write(" ")
        printIrFunRef(insn.targetFunRef, out)

      // %0 = loadarg 0
      case insn: LoadArgInsn => out.write(s" ${insn.index}")

      // %0 = load i64 %1
      case insn: LoadInsn =>
        out.write(" ")
        printType(insn.valueTy, out)
        out.write(" ")
        printOperands(insn, out)

      // %0 = getelementptr i64, ptr %1, [%2].0
      case insn: GetElementPtrInsn =>
        out.write(s" ")
        printType(insn.elemTy, out)
        out.write(", ptr ")
        printInsnRef(insn.ptrRef, out)
        out.write(", [")
        printInsnRef(insn.indexRef, out)
        out.write(s"].${insn.fieldIndex}")

      // %0 = call i32 foo(i32 %1, i32 %2)
      case insn: CallInsn =>
        out.write(s" ")
        printType(insn.funSig.returnTy, out)
        out.write(" ")
        printIrFunRef(insn.targetFunRef, out)
        out.write("(")
        insn.funSig.argTys.zip(insn.argRefs).foreachSep({
          case (argTy, arg) =>
            printType(argTy, out)
            out.write(" ")
            printInsnRef(arg, out)
        }, out.write(", "))
        out.write(")")

      // %0 = callptr i32 %1(i32 %2, i32 %3)
      case insn: CallPtrInsn =>
        out.write(s" ")
        printType(insn.funSig.returnTy, out)
        out.write(" ")
        printInsnRef(insn.funPtrRef, out)
        out.write("(")
        insn.funSig.argTys.zip(insn.argRefs).foreachSep({
          case (argTy, arg) =>
            printType(argTy, out)
            out.write(" ")
            printInsnRef(arg, out)
        }, out.write(", "))
        out.write(")")

      // %0 = condbr %1, label %trueBlock, label %falseBlock
      case insn: TerminatorInsn =>
        if (insn.operandRefs.nonEmpty) {
          out.write(" ")
          printOperands(insn, out)
          if (insn.succBlockRefs.nonEmpty) {
            out.write(", ")
            printSuccBlocks(insn, out)
          }
        } else if (insn.succBlockRefs.nonEmpty) {
          out.write(" ")
          printSuccBlocks(insn, out)
        }

      // %0 = phi [ %1, label %firstPred ], [ %2, label %secondPred ]
      case insn: PhiInsn =>
        if(insn.argRefs.nonEmpty) {
          out.write(" ")
          insn.argRefs.foreachSep({ case (insn, bb) =>
            out.write("[ ")
            printInsnRef(insn, out)
            out.write(", ")
            printBasicBlockRef(bb, out)
            out.write(" ]")
          }, out.write(", "))
        }

      // %0 = iadd %1, %2
      case insn =>
        if (insn.operandRefs.nonEmpty) {
          out.write(" ")
          printOperands(insn, out)
        }
    }
  }

  protected def printBasicBlock(bb: BasicBlock, out: IndentWriter): Unit = {
    out.write(s"${bb.name}:\n")
    out.withIndent({
      bb.body.foreach(insn => {
        printInsn(insn, out)
        out.write("\n")
      })
    })
  }

  protected def printIrFun(fun: IrFun, out: IndentWriter): Unit = {
    out.write("fn ")
    printType(fun.returnTy, out)
    out.write(s" ${fun.name}(")
    fun.argTys.foreachSep(printType(_, out), out.write(", "))
    out.write(") {\n")
    out.withIndent({
      fun.basicBlocks.foreach(bb => printBasicBlock(bb, out))
    })
    out.write("}\n")
  }

  protected def printIrProgram(program: IrProgram, out: IndentWriter): Unit = {
    program.funs.foreach(fun => {
      printIrFun(fun, out)
      out.write("\n")
    })
  }
}
