package tinycc.common.ir

import tinycc.util.IndentWriter

class IrPrinter(val indent: Int = 2) extends IndentPrinter[IrObject] {

  def print(obj: IrObject, out: IndentWriter): Unit = obj match {
    case insn: Insn => printInsn(insn, out)
    case block: BasicBlock => printBasicBlock(block, out)
    case fun: IrFun => printFun(fun, out)
    case program: IrProgram => printProgram(program, out)
  }

  protected def printType(ty: IrTy, out: IndentWriter): Unit =
    out.write(ty.toString)

  protected def printFunSig(funSig: IrFunSignature, out: IndentWriter): Unit = {
    printType(funSig.returnType, out)
    out.write("(")
    funSig.argTypes.foreachSep(ty => printType(ty, out))(_ => out.write(", "))
    out.write(")")
  }

  protected def printInsnRef(insn: Insn, out: IndentWriter): Unit =
    out.write(s"%${insn.name}")

  protected def printInsnRef(ref: InsnRef, out: IndentWriter): Unit = ref() match {
    case Some(value) => printInsnRef(value, out)
    case None => out.write("<null>")
  }

  protected def printOperands(insn: Insn, out: IndentWriter): Unit =
    insn.operandRefs.foreachSep(printInsnRef(_, out), out.write(", "))

  protected def printKindAndOperands(insn: Insn, kind: String, out: IndentWriter): Unit = {
    out.write(kind)
    if(insn.operandRefs.nonEmpty)
      out.write(" ")
    printOperands(insn, out)
  }

  protected def printAllocInsn(insn: AllocInsn, kind: String, out: IndentWriter): Unit = {
    out.write(kind)
    out.write(s" ${insn.sizeCells}")
    insn match {
      case g: AllocGInsn =>
        out.write(s" init=${g.initData}")
      case _ =>
    }
    if(insn.operandRefs.nonEmpty)
      out.write(" ")
    printOperands(insn, out)
  }

  protected def printBasicBlockRef(bb: BasicBlock, out: IndentWriter): Unit =
    out.write(s"%${bb.name}")

  protected def printBasicBlockRef(ref: BasicBlockRef, out: IndentWriter): Unit = ref() match {
    case Some(value) => printBasicBlockRef(value, out)
    case None => out.write("null")
  }

  protected def printSuccBlockRefs(insn: TerminatorInsn, out: IndentWriter): Unit =
    insn.succBlockRefs.foreachSep(printBasicBlockRef(_, out), out.write(", "))

  protected def printInsn(insn: Insn, out: IndentWriter): Unit = {
    printInsnRef(insn, out)
    out.write(" = ")

    insn match {
      case li: IImmInsn => out.write(s"load_imm ${li.value}")

      case lfp: LoadFunPtrInsn => out.write(s"load_fun_ptr ${lfp.targetFun().map(_.name).getOrElse("<null>")}")
      case lap: LoadArgPtrInsn => out.write(s"load_arg_ptr ${lap.index}")
      case lep: LoadElementPtr =>
        printKindAndOperands(lep, "load_element_ptr", out)
        out.write(s" ${lep.elemSizeCells} ${lep.fieldOffsetCells}")

      case i: LoadInsn => printKindAndOperands(i, "load", out)
      case i: StoreInsn => printKindAndOperands(i, "store", out)

      case i: AllocLInsn => printAllocInsn(i, "alloc_l", out)
      case i: AllocGInsn => printAllocInsn(i, "alloc_g", out)

      case i: AddInsn => printKindAndOperands(i, "add", out)
      case i: SubInsn => printKindAndOperands(i, "sub", out)
      case i: MulInsn => printKindAndOperands(i, "mul", out)
      case i: DivInsn => printKindAndOperands(i, "div", out)
      case i: SDivInsn => printKindAndOperands(i, "s_div", out)

      case i: AndInsn => printKindAndOperands(i, "and", out)
      case i: OrInsn => printKindAndOperands(i, "or", out)
      case i: XorInsn => printKindAndOperands(i, "xor", out)
      case i: ShlInsn => printKindAndOperands(i, "shl", out)
      case i: ShrInsn => printKindAndOperands(i, "shr", out)
      case i: NotInsn => printKindAndOperands(i, "not", out)

      case i: CmpLtInsn => printKindAndOperands(i, "cmp_lt", out)
      case i: CmpLEqInsn => printKindAndOperands(i, "cmp_leq", out)
      case i: CmpGtInsn => printKindAndOperands(i, "cmp_gt", out)
      case i: CmpGEqInsn => printKindAndOperands(i, "cmp_geq", out)
      case i: CmpSLtInsn => printKindAndOperands(i, "cmp_s_lt", out)
      case i: CmpSLEqInsn => printKindAndOperands(i, "cmp_s_leq", out)
      case i: CmpSGtInsn => printKindAndOperands(i, "cmp_s_gt", out)
      case i: CmpSGEqInsn => printKindAndOperands(i, "cmp_s_geq", out)
      case i: CmpEqInsn => printKindAndOperands(i, "cmp_eq", out)
      case i: CmpNeInsn => printKindAndOperands(i, "cmp_ne", out)

      case p: PutCharInsn => printKindAndOperands(p, "put_char", out)
      case _: GetCharInsn => out.write("get_char")

      case i: CallInsn => printKindAndOperands(i, s"call ${i.targetFun.get.name}", out)
      case i: CallPtrInsn =>
        out.write("call_ptr ")
        printFunSig(i.funSig, out)
        printKindAndOperands(i, "", out)

      case r: RetInsn => printKindAndOperands(r, "ret", out)
      case _: RetVoidInsn => out.write("ret_void")
      case _: HaltInsn => out.write("halt")

      case b: BrInsn =>
        out.write("br ")
        printSuccBlockRefs(b, out)
      case cb: CondBrInsn =>
        printKindAndOperands(cb, "cond_br", out)
        out.write(", ")
        printSuccBlockRefs(cb, out)
    }
  }

  protected def printBasicBlock(bb: BasicBlock, out: IndentWriter): Unit = {
    out.write(s"${bb.name}:")
    out.indent({
      bb.body.foreachSep(printInsn(_, out), out.write(NL))
    })
  }

  protected def printFun(fun: IrFun, out: IndentWriter): Unit = {
    printType(fun.returnType, out)
    out.write(s" ${fun.name}(")
    fun.argTypes.foreachSep(printType(_, out), out.write(", "))
    out.write(") {")
    out.indent({
      fun.basicBlocks.foreachSep(printBasicBlock(_, out), out.write(NL))
    })
    out.write("}")
  }

  protected def printProgram(program: IRProgram, out: IndentWriter): Unit = {
    program.funs.foreachSep(printFun(_, out), out.write(NL))
  }

}
