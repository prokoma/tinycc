package tinycc.frontend

import scala.collection.mutable

class CompilerException(message: String, loc: SourceLocation) extends ProgramException(message) {
  override def format(reporter: Reporter): String = reporter.formatError("compiler", message, loc)
}

final class Compiler(_declarations: Declarations, _typeMap: TypeMap) {
  implicit protected def declarations: Declarations = _declarations

  implicit protected def typeMap: TypeMap = _typeMap

  def compileProgram(node: AST): IRProgram = new _Impl().compileProgram(node)

  final private class _Impl extends IRProgramBuilderOps {

    val program: IRProgram = new IRProgram
    val efb: IRFunBuilder = new IRFunBuilder(program.appendEntryFun())

    /* Compiler State */

    var breakTargetOption: Option[BasicBlock] = None
    var contTargetOption: Option[BasicBlock] = None

    val allocMap: mutable.Map[ASTVarDecl, AllocInsn] = mutable.Map.empty
    val funMap: mutable.Map[String, IRFun] = mutable.Map.empty

    /* Entry Method */

    def compileProgram(node: AST): IRProgram = {
      efb.appendBlock(new BasicBlock("entry", efb.fun))

      compileStmt(node)

      val mainFun = program.funs.find(_.name == "main").getOrElse(throw new CompilerException("Missing main function declaration", node.location()))
      if (mainFun.argTypes.nonEmpty)
        throw new CompilerException("Invalid main function signature", node.location())

      efb.append(new CallInsn(mainFun, Seq.empty, efb.bb))
      efb.append(new HaltInsn(efb.bb))

      program
    }

    /* Helper Methods */

    private def withBreakTarget[T](breakTarget: BasicBlock, thunk: => T): T = {
      val oldBreakTargetOption = breakTargetOption
      breakTargetOption = Some(breakTarget)
      try
        thunk
      finally {
        breakTargetOption = oldBreakTargetOption
      }
    }

    private def withBreakContTarget[T](breakTarget: BasicBlock, contTarget: BasicBlock, thunk: => T): T = {
      val oldBreakTargetOption = breakTargetOption
      val oldContTargetOption = contTargetOption
      breakTargetOption = Some(breakTarget)
      contTargetOption = Some(contTarget)
      try
        thunk
      finally {
        breakTargetOption = oldBreakTargetOption
        contTargetOption = oldContTargetOption
      }
    }

    /* Visitor Methods */

    private def compileType(ty: Ty): IRType = ty match {
      case VoidTy => IRTypes.void
      case IntTy => IRTypes.i64
      case _: PtrTy => IRTypes.i64
      case _ => ???
    }

    private def compileStmt(node: AST): Unit = node.asRealType match {
      case vd: ASTVarDecl =>
        val varType = vd.getVarType.ty

        allocMap(vd) = funOption match {
          case Some(_) => // local variable
            val alloc = append(new AllocLInsn(varType.allocSizeCells, bb))
            vd.value.foreach(value => {
              val compiledValue = compileExpr(value)
              append(new StoreInsn(alloc, compiledValue, bb))
            })
            alloc

          case None => // global variable - append to entryFun
            val initData = vd.value.map(_.asRealType) match {
              case Some(i: ASTInteger) =>
                Seq(i.getValue)
              case Some(v) =>
                throw new CompilerException(s"Expected integer literal as global variable initializer, got $v", v.location())
              case None =>
                Seq.empty
            }
            efb.append(new AllocGInsn(varType.allocSizeCells, initData, efb.bb))
        }

      case fd: ASTFunDecl =>
        compileFunDecl(fd)

      case _: ASTFunPtrDecl => // ignore

      case i: ASTIf =>
        val trueBlock = new BasicBlock("ifTrue", fun)
        val falseBlock = new BasicBlock("ifFalse", fun)
        val contBlock = new BasicBlock("ifCont", fun)

        val cond = compileExpr(i.cond)
        append(new CondBrInsn(cond, trueBlock, falseBlock, bb))

        appendBlock(trueBlock)
        compileStmt(i.trueCase)
        append(new BrInsn(contBlock, bb))

        appendBlock(falseBlock)
        i.falseCase.foreach(compileStmt)
        append(new BrInsn(contBlock, bb))

        appendBlock(contBlock)

      case w: ASTWhile =>
        val condBlock = new BasicBlock("whileCond", fun)
        val bodyBlock = new BasicBlock("whileBody", fun)
        val contBlock = new BasicBlock("whileCont", fun)

        append(new BrInsn(condBlock, bb))

        appendBlock(condBlock)
        val cond = compileExpr(w.cond)
        append(new CondBrInsn(cond, bodyBlock, contBlock, bb))

        appendBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(w.body)
        })
        append(new BrInsn(condBlock, bb))

        appendBlock(contBlock)

      case dw: ASTDoWhile =>
        val bodyBlock = new BasicBlock("doWhileBody", fun)
        val condBlock = new BasicBlock("doWhileCond", fun)
        val contBlock = new BasicBlock("doWhileCont", fun)

        append(new BrInsn(bodyBlock, bb))

        appendBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(dw.body)
        })
        append(new BrInsn(condBlock, bb))

        appendBlock(condBlock)
        val cond = compileExpr(dw.cond)
        append(new CondBrInsn(cond, bodyBlock, contBlock, bb))

        appendBlock(contBlock)

      case f: ASTFor =>
        val condBlock = new BasicBlock("forCond", fun)
        val bodyBlock = new BasicBlock("forBody", fun)
        val incBlock = new BasicBlock("forInc", fun)
        val contBlock = new BasicBlock("forCont", fun)

        f.init.foreach(compileStmt)
        append(new BrInsn(condBlock, bb))

        appendBlock(condBlock)
        f.cond match {
          case Some(c) =>
            val cond = compileExpr(c)
            append(new CondBrInsn(cond, bodyBlock, contBlock, bb))

          case None =>
            append(new BrInsn(bodyBlock, bb))
        }

        appendBlock(bodyBlock)
        withBreakContTarget(contBlock, incBlock, {
          compileStmt(f.body)
        })
        append(new BrInsn(incBlock, bb))

        appendBlock(incBlock)
        f.increment.foreach(compileExpr)
        append(new BrInsn(condBlock, bb))

        appendBlock(contBlock)

      case s: ASTSwitch =>
        val contBlock = new BasicBlock("switchCont", fun)
        val defaultBlock = new BasicBlock("switchDefault", fun)

        val cond = compileExpr(s.cond) // TODO: store in local?
        val caseBlocks = s.cases.map({ case (value, _) =>
          val caseBlock = new BasicBlock(s"switchCase$value", fun)
          val caseContBlock = new BasicBlock(s"switchCase${value}Cont", fun)

          val valueImm = append(new LoadImmInsn(value, bb))
          val cmpEq = append(new CmpEqInsn(cond, valueImm, bb))
          append(new CondBrInsn(cmpEq, caseBlock, caseContBlock, bb))
          appendBlock(caseContBlock)
          caseBlock
        }).toIndexedSeq

        if(s.defaultCase.isDefined)
          append(new BrInsn(defaultBlock, bb))
        else
          append(new BrInsn(contBlock, bb))

        s.cases.zipWithIndex.foreach({ case ((_, caseBody), i) =>
          val caseBlock = caseBlocks(i)

          appendBlock(caseBlock)
          withBreakTarget(contBlock, {
            compileStmt(caseBody)
          })
          if(i < caseBlocks.size-1)
            append(new BrInsn(caseBlocks(i+1), bb))
          else if(s.defaultCase.isDefined)
            append(new BrInsn(defaultBlock, bb))
          else
            append(new BrInsn(contBlock, bb))
        })

        s.defaultCase.foreach(defaultBody => {
          appendBlock(defaultBlock)
          withBreakTarget(contBlock, {
            compileStmt(defaultBody)
          })
          append(new BrInsn(contBlock, bb))
        })

        appendBlock(contBlock)

      case c: ASTContinue => contTargetOption match {
        case Some(contTarget) =>
          append(new BrInsn(contTarget, bb))
          appendBlock(new BasicBlock("unreachable", fun))

        case None => throw new CompilerException("Unexpected continue stmt outside of loop.", c.location())
      }

      case b: ASTBreak => breakTargetOption match {
        case Some(breakTarget) =>
          append(new BrInsn(breakTarget, bb))
          appendBlock(new BasicBlock("unreachable", fun))

        case None => throw new CompilerException("Unexpected break stmt outside of loop.", b.location())
      }

      case b: ASTBlock =>
        b.body.foreach(compileStmt)

      case r: ASTReturn =>
        r.value match {
          case Some(v) =>
            val retVal = compileExpr(v)
            append(new RetInsn(retVal, bb))

          case None =>
            append(new RetVoidInsn(bb))
        }
        appendBlock(new BasicBlock("unreachable", fun))

      case w: ASTWrite =>
        val value = compileExpr(w.getValue)
        append(new PutCharInsn(value, bb))

      case s: ASTSequence =>
        s.body.foreach(compileStmt)

      case _ => compileExpr(node)
    }

    private def compileFunDecl(node: ASTFunDecl): Unit = {
      val fdTy = node.ty.asInstanceOf[FunTy]
      val irFun = funMap.getOrElseUpdate(node.nameString, new IRFun(
        node.nameString,
        compileType(fdTy.returnTy),
        fdTy.argTys.map(compileType),
        program
      ))

      appendFun(irFun)
      if(node.body.isDefined) {
        appendBlock(new BasicBlock("entry", fun))
        node.body.foreach(compileStmt)

        if (fdTy.returnTy == VoidTy)
          append(new RetVoidInsn(bb)) // implicit return
      }
      exitFun()
    }

    /** Returns instruction, whose result is a pointer to the expr. */
    private def compileExprLvalue(node: AST): Insn = node.asRealType match {
      case i: ASTIdentifier => i.decl match {
        case FunDecl(decl) => append(new LoadFunPtrInsn(funMap(decl.nameString), bb))
        case VarDecl(decl) => allocMap(decl)
        case FunArgDecl(_, index, _) => append(new LoadArgPtrInsn(index, bb))
      }

      case d: ASTDeref => compileExpr(d.getTarget)

      case i: ASTIndex =>
        val base = compileExpr(i.getBase)
        val baseTy = i.getBase.ty.asInstanceOf[IndexableTyBase]
        val index = compileExpr(i.getIndex)
        append(new LoadElementPtr(base, index, baseTy.baseTy.sizeCells, 0, bb))

      case n => throw new CompilerException(s"Expression '$n' is not a l-value.", n.location())
    }

    private def compileExpr(node: AST): Insn = node.asRealType match {
      case i: ASTInteger =>
        append(new LoadImmInsn(i.getValue, bb))

      case uo: ASTUnaryOp =>
        compileUnaryOp(uo)

      case upo: ASTUnaryPostOp =>
        compileUnaryPostOp(upo)

      case bo: ASTBinaryOp =>
        compileBinaryOp(bo)

      case s: ASTSequence =>
        if (s.body.isEmpty)
          throw new CompilerException("Empty sequence.", s.location())
        s.body.map(compileExpr).last

      case _: ASTRead =>
        append(new GetCharInsn(bb))

      case a: ASTAssignment =>
        val lvalue = compileExprLvalue(a.getLvalue)
        val value = compileExpr(a.getValue)
        append(new StoreInsn(lvalue, value, bb))
        value

      // variable access
      case i: ASTIdentifier => i.decl match {
        case FunDecl(node) => ???
        case VarDecl(node) => append(new LoadInsn(allocMap(node), bb))
        case FunArgDecl(_, index, _) =>
          val ptr = append(new LoadArgPtrInsn(index, bb))
          append(new LoadInsn(ptr, bb))
      }

      case a: ASTAddress =>
        compileExprLvalue(a.getTarget)

      case d: ASTDeref =>
        val value = compileExpr(d.getTarget)
        append(new LoadInsn(value, bb))

      case i: ASTIndex =>
        val addr = compileExprLvalue(i)
        append(new LoadInsn(addr, bb))

      case c: ASTCall =>
        c.function.asRealType match {
          case id: ASTIdentifier => // direct call
            id.decl match {
              case FunDecl(decl) =>
                append(new CallInsn(funMap(decl.nameString), compileCallArgs(c.args), bb))
              case _ => throw new CompilerException("Call of non-function", c.location())
            }

          case fun => // indirect call
            val funPtr = compileExprLvalue(fun)
            val funSig = compileFunSignature(fun.ty.asInstanceOf[FunTy])
            append(new CallPtrInsn(funSig, funPtr, compileCallArgs(c.args), bb))
        }

      case c: ASTCast =>
        compileExpr(c.getValue) // only casts from int to ptr and vice versa are supported

      case n => throw new NotImplementedError(s"Expr '$n'")
    }

    private def compileFunSignature(ty: FunTy): IRFunSignature =
      IRFunSignature(compileType(ty.returnTy), ty.argTys.map(compileType))

    private def compileCallArgs(args: Seq[AST]): Seq[Insn] =
      args.map(compileExpr)

    private def castToBool(arg: Insn): Insn = {
      val zero = append(new LoadImmInsn(0, bb))
      append(new CmpNeInsn(zero, arg, bb))
    }

    private def isIntOrPtrTy(ty: Ty): Boolean = ty match {
      case IntTy | _: IndexableTyBase => true
      case _ => false
    }

    private def compileUnaryOp(node: ASTUnaryOp): Insn = (node.getOp, node.getArg.ty) match {
      case (Symbols.add, ty) if isIntOrPtrTy(ty) =>
        compileExpr(node.getArg)

      case (Symbols.sub, ty) if isIntOrPtrTy(ty) =>
        val arg = compileExpr(node.getArg)
        val zero = append(new LoadImmInsn(0, bb))
        append(new SubInsn(zero, arg, bb))

      case (Symbols.not, IntTy) =>
        val arg = compileExpr(node.getArg)
        val zero = append(new LoadImmInsn(0, bb))
        append(new CmpEqInsn(arg, zero, bb))

      case (Symbols.neg, IntTy) =>
        val arg = compileExpr(node.getArg)
        append(new NotInsn(arg, bb))

      case (Symbols.inc, IntTy) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, bb))
        val one = append(new LoadImmInsn(1, bb))
        val newValue = append(new AddInsn(oldValue, one, bb))
        append(new StoreInsn(arg, newValue, bb))
        newValue

      case (Symbols.dec, IntTy) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, bb))
        val one = append(new LoadImmInsn(1, bb))
        val newValue = append(new SubInsn(oldValue, one, bb))
        append(new StoreInsn(arg, newValue, bb))
        newValue

      case (op, argTy) => throw new NotImplementedError(s"UnaryOp '${op.name()}' with '$argTy'.")
    }

    private def compileUnaryPostOp(node: ASTUnaryPostOp): Insn = (node.getOp, node.getArg.ty) match {
      case (Symbols.inc, ty) if isIntOrPtrTy(ty) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, bb))
        val one = append(new LoadImmInsn(1, bb))
        val newValue = append(new AddInsn(oldValue, one, bb))
        append(new StoreInsn(arg, newValue, bb))
        oldValue

      case (Symbols.dec, ty) if isIntOrPtrTy(ty) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, bb))
        val one = append(new LoadImmInsn(1, bb))
        val newValue = append(new SubInsn(oldValue, one, bb))
        append(new StoreInsn(arg, newValue, bb))
        oldValue

      case (op, argTy) => throw new NotImplementedError(s"UnaryPostOp '${op.name()}' with '$argTy'.")
    }

    private def compileBinaryOp(node: ASTBinaryOp): Insn = (node.getOp, node.getLeft.ty, node.getRight.ty) match {
      case (Symbols.add, leftTy, IntTy) if isIntOrPtrTy(leftTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new AddInsn(left, right, bb))

      case (Symbols.sub, leftTy, IntTy) if isIntOrPtrTy(leftTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new SubInsn(left, right, bb))

      case (Symbols.mul, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new MulInsn(left, right, bb))

      case (Symbols.div, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new SDivInsn(left, right, bb))

      case (Symbols.mod, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        val divRes = append(new SDivInsn(left, right, bb))
        val mulRes = append(new MulInsn(divRes, right, bb))
        append(new SubInsn(left, mulRes, bb))

      case (Symbols.and, IntTy, IntTy) =>
        val leftBool = castToBool(compileExpr(node.getLeft))
        val rightBool = castToBool(compileExpr(node.getRight))
        append(new AndInsn(leftBool, rightBool, bb))

      case (Symbols.bitAnd, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new AndInsn(left, right, bb))

      case (Symbols.or, IntTy, IntTy) =>
        val leftBool = castToBool(compileExpr(node.getLeft))
        val rightBool = castToBool(compileExpr(node.getRight))
        append(new OrInsn(leftBool, rightBool, bb))

      case (Symbols.bitOr, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new OrInsn(left, right, bb))

      case (Symbols.xor, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new XorInsn(left, right, bb))

      case (Symbols.lt, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSLtInsn(left, right, bb))

      case (Symbols.lte, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSLEqInsn(left, right, bb))

      case (Symbols.gt, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSGtInsn(left, right, bb))

      case (Symbols.gte, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSGEqInsn(left, right, bb))

      case (Symbols.eq, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpEqInsn(left, right, bb))

      case (Symbols.neq, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpNeInsn(left, right, bb))

      case (op, leftTy, rightTy) => throw new NotImplementedError(s"BinaryOp '${op.name()}' with '$leftTy' and '$rightTy'.")
    }
  }

}
