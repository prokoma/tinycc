package tinycc.frontend

import tinycc.{ErrorLevel, ProgramException}
import tinycc.common.ir._
import tinycc.frontend.Types.FunTy
import tinycc.frontend.analysis.IdentifierDecl.FunDecl
import tinycc.frontend.ast._
import tinycc.util.parsing.SourceLocation

import scala.collection.mutable

class CompilerException(level: ErrorLevel, message: String, val loc: SourceLocation) extends ProgramException(level, message)

final class Compiler(program: AstBlock, _declarations: Declarations, _typeMap: TypeMap) {
  implicit protected def declarations: Declarations = _declarations

  implicit protected def typeMap: TypeMap = _typeMap

  lazy val result: IrProgram = new _Impl().compileProgram(program)

  final private class _Impl extends IrProgramBuilderOps {

    val program: IrProgram = new IrProgram
    val entryFun: IrFun = program.appendEntryFun()

    /* Compiler State */

    var breakTargetOption: Option[BasicBlock] = None
    var contTargetOption: Option[BasicBlock] = None

    val allocMap: mutable.Map[AstVarDecl, AllocInsn] = mutable.Map.empty
    val funMap: mutable.Map[Symbol, IrFun] = mutable.Map.empty

    /* Entry Method */

    def compileProgram(node: AstBlock): IrProgram = {
      enterFun(entryFun)
      appendBlock("entry")

      compileStmt(node)

      enterFun(entryFun)
      val mainFun = program.funs.find(_.name == "main")
        .getOrElse(throw new CompilerException(ErrorLevel.Error, "missing main function declaration", node.loc))

      if (mainFun.argTys.nonEmpty)
        throw new CompilerException(ErrorLevel.Error, "invalid main function signature (expected main())", node.loc)

      append(new CallInsn(mainFun, Seq.empty, _))
      append(new HaltInsn(_))

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

    private def compileType(ty: Types.Ty): IrTy = ty match {
      case Types.VoidTy => IrTy.VoidTy
      case Types.CharTy => IrTy.Int64Ty
      case Types.IntTy => IrTy.Int64Ty
      case Types.DoubleTy => IrTy.DoubleTy
      case _: Types.PtrTy => IrTy.PtrTy
      case Types.ArrayTy(baseTy, numElem) => IrTy.ArrayTy(compileType(baseTy), numElem)

      case Types.StructTy(_, fieldsOption) =>
        val fieldIrTys = fieldsOption match {
          case Some(fields) => fields.map({ case (fieldTy, _) => compileType(fieldTy) }))
          case None => throw new UnsupportedOperationException(s"Cannot compile incomplete struct type.")
        }
        IrTy.StructTy(fieldIrTys)

      case _: Types.FunTy => throw new UnsupportedOperationException(s"Cannot compile FunTy to IR type.")
    }

    private def compileStmt(node: AstNode): Unit = node match {
      case node: AstVarDecl =>
        val varIrTy = compileType(node.varTy.ty)

        allocMap(node) = funOption match {
          case Some(_) => // local variable
            val alloc = append(new AllocLInsn(varIrTy, _))
            node.value.foreach(value => {
              val compiledValue = compileExpr(value)
              append(new StoreInsn(alloc, compiledValue, _))
            })
            alloc

          case None => // global variable - append to entryFun
            enterFun()
            val alloc = efb.append(new AllocGInsn(varIrTy, Seq.empty, _))
            node.value.foreach(value => {
              val compiledValue = compileExpr(value) // TODO: temporarily switch
              efb.append(new StoreInsn(alloc, compiledValue, _))
            })
            alloc
        }

      case node: AstFunDecl =>
        compileFunDecl(node)

      case node: AstIf =>
        val trueBlock = new BasicBlock("ifTrue", fun)
        val falseBlock = new BasicBlock("ifFalse", fun)
        val contBlock = new BasicBlock("ifCont", fun)

        val cond = compileExpr(node.guard)
        append(new CondBrInsn(cond, trueBlock, falseBlock, _))

        appendBlock(trueBlock)
        compileStmt(node.trueCase)
        append(new BrInsn(contBlock, _))

        appendBlock(falseBlock)
        node.falseCase.foreach(compileStmt)
        append(new BrInsn(contBlock, _))

        appendBlock(contBlock)

      case node: AstWhile =>
        val condBlock = new BasicBlock("whileCond", fun)
        val bodyBlock = new BasicBlock("whileBody", fun)
        val contBlock = new BasicBlock("whileCont", fun)

        append(new BrInsn(condBlock, _))

        appendBlock(condBlock)
        val cond = compileExpr(node.guard)
        append(new CondBrInsn(cond, bodyBlock, contBlock, _))

        appendBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(node.body)
        })
        append(new BrInsn(condBlock, _))

        appendBlock(contBlock)

      case node: AstDoWhile =>
        val bodyBlock = new BasicBlock("doWhileBody", fun)
        val condBlock = new BasicBlock("doWhileCond", fun)
        val contBlock = new BasicBlock("doWhileCont", fun)

        append(new BrInsn(bodyBlock, _))

        appendBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(node.body)
        })
        append(new BrInsn(condBlock, _))

        appendBlock(condBlock)
        val cond = compileExpr(node.guard)
        append(new CondBrInsn(cond, bodyBlock, contBlock, _))

        appendBlock(contBlock)

      case node: AstFor =>
        val condBlock = new BasicBlock("forCond", fun)
        val bodyBlock = new BasicBlock("forBody", fun)
        val incBlock = new BasicBlock("forInc", fun)
        val contBlock = new BasicBlock("forCont", fun)

        node.init.foreach(compileStmt)
        append(new BrInsn(condBlock, _))

        appendBlock(condBlock)
        node.guard match {
          case Some(c) =>
            val cond = compileExpr(c)
            append(new CondBrInsn(cond, bodyBlock, contBlock, _))

          case None =>
            append(new BrInsn(bodyBlock, _))
        }

        appendBlock(bodyBlock)
        withBreakContTarget(contBlock, incBlock, {
          compileStmt(node.body)
        })
        append(new BrInsn(incBlock, _))

        appendBlock(incBlock)
        node.increment.foreach(compileExpr)
        append(new BrInsn(condBlock, _))

        appendBlock(contBlock)

      case node: AstSwitch =>
        val contBlock = new BasicBlock("switchCont", fun)
        val defaultBlock = new BasicBlock("switchDefault", fun)

        val cond = compileExpr(node.guard)
        val caseBlocks = node.cases.map({ case (value, _) =>
          val caseBlock = new BasicBlock(s"switchCase$value", fun)
          val caseContBlock = new BasicBlock(s"switchCase${value}Cont", fun)

          val valueImm = append(new IImmInsn(value, _))
          val cmpEq = append(new CmpInsn(IrOpcode.CmpIEq, cond, valueImm, _))
          append(new CondBrInsn(cmpEq, caseBlock, caseContBlock, _))
          appendBlock(caseContBlock)
          caseBlock
        }).toIndexedSeq

        if (node.defaultCase.isDefined)
          append(new BrInsn(defaultBlock, _))
        else
          append(new BrInsn(contBlock, _))

        node.cases.zipWithIndex.foreach({ case ((_, caseBody), i) =>
          val caseBlock = caseBlocks(i)

          appendBlock(caseBlock)
          withBreakTarget(contBlock, {
            compileStmt(caseBody)
          })
          if (i < caseBlocks.size - 1)
            append(new BrInsn(caseBlocks(i + 1), _))
          else if (node.defaultCase.isDefined)
            append(new BrInsn(defaultBlock, _))
          else
            append(new BrInsn(contBlock, _))
        })

        node.defaultCase.foreach(defaultBody => {
          appendBlock(defaultBlock)
          withBreakTarget(contBlock, {
            compileStmt(defaultBody)
          })
          append(new BrInsn(contBlock, _))
        })

        appendBlock(contBlock)

      case c: AstContinue => contTargetOption match {
        case Some(contTarget) =>
          append(new BrInsn(contTarget, _))
          appendBlock(new BasicBlock("unreachable", fun))

        case None => throw new CompilerException("Unexpected continue stmt outside of loop.", c.loc)
      }

      case b: AstBreak => breakTargetOption match {
        case Some(breakTarget) =>
          append(new BrInsn(breakTarget, _))
          appendBlock(new BasicBlock("unreachable", fun))

        case None => throw new CompilerException("Unexpected break stmt outside of loop.", b.loc)
      }

      case b: AstBlock =>
        b.body.foreach(compileStmt)

      case r: AstReturn =>
        r.expr match {
          case Some(v) =>
            val retVal = compileExpr(v)
            append(new RetInsn(retVal, _))

          case None =>
            append(new RetVoidInsn(bb))
        }
        appendBlock(new BasicBlock("unreachable", fun))

      case w: AstWrite =>
        val value = compileExpr(w.expr)
        append(new PutCharInsn(value, _))

      case s: AstSequence =>
        s.body.foreach(compileStmt)

      case s: AstBlock =>
        s.body.foreach(compileStmt)

      case _ => compileExpr(node)
    }

    private def compileFunDecl(node: AstFunDecl): Unit = {
      val funTy = node.ty.asInstanceOf[FunTy]
      val irFun = funMap.getOrElseUpdate(node.symbol, new IrFun(
        node.symbol.name,
        compileType(funTy.returnTy),
        funTy.argTys.map(compileType),
        program
      ))

      appendFun(irFun)
      if (node.body.isDefined) {
        appendBlock(new BasicBlock("entry", fun))
        node.body.foreach(compileStmt)

        if (funTy.returnTy == Types.VoidTy)
          append(new RetVoidInsn(bb)) // implicit return
      }
      exitFun()
    }

    /** Returns instruction, whose result is a pointer to the expr. */
    private def compileExprLvalue(node: AST): Insn = node match {
      case i: ASTIdentifier => i.decl match {
        case FunDecl(decl) => append(new LoadFunPtrInsn(funMap(decl.nameString), _))
        case VarDecl(decl) => allocMap(decl)
        case FunArgDecl(_, index, _) => append(new LoadArgPtrInsn(index, _))
      }

      case d: ASTDeref => compileExpr(d.getTarget)

      case i: ASTIndex =>
        val base = compileExpr(i.getBase)
        val baseTy = i.getBase.ty.asInstanceOf[IndexableTyBase]
        val index = compileExpr(i.getIndex)
        append(new LoadElementPtr(base, index, baseTy.baseTy.sizeCells, 0, _))

      case n => throw new CompilerException(s"Expression '$n' is not a l-value.", n.loc)
    }

    private def compileExpr(node: AST): Insn = node match {
      case i: ASTInteger =>
        append(new IImmInsn(i.getValue, _))

      case uo: ASTUnaryOp =>
        compileUnaryOp(uo)

      case upo: ASTUnaryPostOp =>
        compileUnaryPostOp(upo)

      case bo: ASTBinaryOp =>
        compileBinaryOp(bo)

      case s: ASTSequence =>
        if (s.body.isEmpty)
          throw new CompilerException("Empty sequence.", s.loc)
        s.body.map(compileExpr).last

      case _: ASTRead =>
        append(new GetCharInsn(bb))

      case a: ASTAssignment =>
        val lvalue = compileExprLvalue(a.getLvalue)
        val value = compileExpr(a.getValue)
        append(new StoreInsn(lvalue, value, _))
        value

      // variable access
      case i: ASTIdentifier => i.decl match {
        case FunDecl(node) => ???
        case VarDecl(node) => append(new LoadInsn(allocMap(node), _))
        case FunArgDecl(_, index, _) =>
          val ptr = append(new LoadArgPtrInsn(index, _))
          append(new LoadInsn(ptr, _))
      }

      case a: ASTAddress =>
        compileExprLvalue(a.getTarget)

      case d: ASTDeref =>
        val value = compileExpr(d.getTarget)
        append(new LoadInsn(value, _))

      case i: ASTIndex =>
        val addr = compileExprLvalue(i)
        append(new LoadInsn(addr, _))

      case c: ASTCall =>
        c.function match {
          case id: ASTIdentifier => // direct call
            id.decl match {
              case FunDecl(decl) =>
                append(new CallInsn(funMap(decl.nameString), compileCallArgs(c.args), _))
              case _ => throw new CompilerException("Call of non-function", c.loc)
            }

          case fun => // indirect call
            val funPtr = compileExprLvalue(fun)
            val funSig = compileFunSignature(fun.ty.asInstanceOf[FunTy])
            append(new CallPtrInsn(funSig, funPtr, compileCallArgs(c.args), _))
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
      val zero = append(new IImmInsn(0, _))
      append(new CmpNeInsn(zero, arg, _))
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
        val zero = append(new IImmInsn(0, _))
        append(new SubInsn(zero, arg, _))

      case (Symbols.not, IntTy) =>
        val arg = compileExpr(node.getArg)
        val zero = append(new IImmInsn(0, _))
        append(new CmpEqInsn(arg, zero, _))

      case (Symbols.neg, IntTy) =>
        val arg = compileExpr(node.getArg)
        append(new NotInsn(arg, _))

      case (Symbols.inc, IntTy) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, _))
        val one = append(new IImmInsn(1, _))
        val newValue = append(new AddInsn(oldValue, one, _))
        append(new StoreInsn(arg, newValue, _))
        newValue

      case (Symbols.dec, IntTy) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, _))
        val one = append(new IImmInsn(1, _))
        val newValue = append(new SubInsn(oldValue, one, _))
        append(new StoreInsn(arg, newValue, _))
        newValue

      case (op, argTy) => throw new NotImplementedError(s"UnaryOp '${op.name()}' with '$argTy'.")
    }

    private def compileUnaryPostOp(node: ASTUnaryPostOp): Insn = (node.getOp, node.getArg.ty) match {
      case (Symbols.inc, ty) if isIntOrPtrTy(ty) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, _))
        val one = append(new IImmInsn(1, _))
        val newValue = append(new AddInsn(oldValue, one, _))
        append(new StoreInsn(arg, newValue, _))
        oldValue

      case (Symbols.dec, ty) if isIntOrPtrTy(ty) =>
        val arg = compileExprLvalue(node.getArg)
        val oldValue = append(new LoadInsn(arg, _))
        val one = append(new IImmInsn(1, _))
        val newValue = append(new SubInsn(oldValue, one, _))
        append(new StoreInsn(arg, newValue, _))
        oldValue

      case (op, argTy) => throw new NotImplementedError(s"UnaryPostOp '${op.name()}' with '$argTy'.")
    }

    private def compileBinaryOp(node: ASTBinaryOp): Insn = (node.getOp, node.getLeft.ty, node.getRight.ty) match {
      case (Symbols.add, leftTy, IntTy) if isIntOrPtrTy(leftTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new AddInsn(left, right, _))

      case (Symbols.sub, leftTy, IntTy) if isIntOrPtrTy(leftTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new SubInsn(left, right, _))

      case (Symbols.mul, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new MulInsn(left, right, _))

      case (Symbols.div, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new SDivInsn(left, right, _))

      case (Symbols.mod, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        val divRes = append(new SDivInsn(left, right, _))
        val mulRes = append(new MulInsn(divRes, right, _))
        append(new SubInsn(left, mulRes, _))

      case (Symbols.and, IntTy, IntTy) =>
        val leftBool = castToBool(compileExpr(node.getLeft))
        val rightBool = castToBool(compileExpr(node.getRight))
        append(new AndInsn(leftBool, rightBool, _))

      case (Symbols.bitAnd, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new AndInsn(left, right, _))

      case (Symbols.or, IntTy, IntTy) =>
        val leftBool = castToBool(compileExpr(node.getLeft))
        val rightBool = castToBool(compileExpr(node.getRight))
        append(new OrInsn(leftBool, rightBool, _))

      case (Symbols.bitOr, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new OrInsn(left, right, _))

      case (Symbols.xor, IntTy, IntTy) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new XorInsn(left, right, _))

      case (Symbols.lt, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSLtInsn(left, right, _))

      case (Symbols.lte, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSLEqInsn(left, right, _))

      case (Symbols.gt, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSGtInsn(left, right, _))

      case (Symbols.gte, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpSGEqInsn(left, right, _))

      case (Symbols.eq, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpEqInsn(left, right, _))

      case (Symbols.neq, _, _) =>
        val left = compileExpr(node.getLeft)
        val right = compileExpr(node.getRight)
        append(new CmpNeInsn(left, right, _))

      case (op, leftTy, rightTy) => throw new NotImplementedError(s"BinaryOp '${op.name()}' with '$leftTy' and '$rightTy'.")
    }
  }

}
