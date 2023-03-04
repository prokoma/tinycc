package tinycc.frontend

import tinycc.cli.Reporter
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir._
import tinycc.frontend.Types._
import tinycc.frontend.analysis.IdentifierDecl
import tinycc.frontend.analysis.IdentifierDecl.{FunArgDecl, FunDecl, VarDecl}
import tinycc.frontend.ast._
import tinycc.util.parsing.SourceLocation
import tinycc.{ErrorLevel, ProgramException}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

class TinyCCompilerException(val level: ErrorLevel, message: String, val loc: SourceLocation) extends ProgramException(message) {
  override def format(reporter: Reporter): String = reporter.formatError(level, message, loc)
}

trait TinyCIrProgramBuilder extends IrProgramBuilderOps {
  val program: IrProgram = new IrProgram
  val entryFun: IrFun = program.appendEntryFun()

  var breakTargetOption: Option[BasicBlock] = None
  var contTargetOption: Option[BasicBlock] = None

  def withEntryFun[T](thunk: => T): T = withFun(entryFun, thunk)

  def withBreakTarget[T](breakTarget: BasicBlock, thunk: => T): T = {
    val oldBreakTargetOption = breakTargetOption
    breakTargetOption = Some(breakTarget)
    try
      thunk
    finally {
      breakTargetOption = oldBreakTargetOption
    }
  }

  def withBreakContTarget[T](breakTarget: BasicBlock, contTarget: BasicBlock, thunk: => T): T = {
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
}

final class TinyCCompiler(program: AstProgram, _declarations: Declarations, _typeMap: TypeMap) {
  implicit protected def declarations: Declarations = _declarations

  implicit protected def typeMap: TypeMap = _typeMap

  lazy val result: Either[TinyCCompilerException, IrProgram] = {
    try
      Right(new _Impl().compileProgram(program))
    catch {
      case e: TinyCCompilerException => Left(e)
      case e: Throwable => throw e
    }
  }

  final private class _Impl extends TinyCIrProgramBuilder {

    import ErrorLevel._

    /* Compiler State */

    val allocMap: mutable.Map[IdentifierDecl, AllocInsn] = mutable.Map.empty
    val globalMap: mutable.Map[Symbol, AllocGInsn] = mutable.Map.empty
    val funMap: mutable.Map[Symbol, IrFun] = mutable.Map.empty

    /* Entry Method */

    def compileProgram(node: AstProgram): IrProgram = {
      withEntryFun({
        appendAndEnterBlock("entry")
      })

      compileStmt(node)

      withEntryFun({
        val mainFun = program.funs.find(_.name == "main")
          .getOrElse(throw new TinyCCompilerException(ErrorLevel.Error, "missing main function declaration", node.loc))
        if (mainFun.argTys.nonEmpty)
          throw new TinyCCompilerException(ErrorLevel.Error, "invalid main function signature (expected main())", node.loc)

        emit(new CallInsn(mainFun, IndexedSeq.empty, _))
        emit(new HaltInsn(_))
      })

      program
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
          case Some(fields) => fields.map({ case (fieldTy, _) => compileType(fieldTy) })
          case None => throw new UnsupportedOperationException(s"Cannot compile incomplete struct type.")
        }
        IrTy.StructTy(fieldIrTys)

      case _: Types.FunTy => throw new UnsupportedOperationException(s"Cannot compile FunTy to IR type.")
    }

    private def compileStmt(node: AstNode): Unit = node match {
      case node: AstVarDecl => compileVarDecl(node)
      case node: AstFunDecl => compileFunDecl(node)
      case _: AstStructDecl | _: AstFunPtrDecl => // Ignore, handled by TypeAnalysis

      case node: AstIf =>
        val trueBlock = new BasicBlock("ifTrue", fun)
        val falseBlock = new BasicBlock("ifFalse", fun)
        val contBlock = new BasicBlock("ifCont", fun)

        val cond = compileExpr(node.guard)
        emit(new CondBrInsn(cond, trueBlock, falseBlock, _))

        appendAndEnterBlock(trueBlock)
        compileStmt(node.trueCase)
        emit(new BrInsn(contBlock, _))

        appendAndEnterBlock(falseBlock)
        node.falseCase.foreach(compileStmt)
        emit(new BrInsn(contBlock, _))

        appendAndEnterBlock(contBlock)

      case node: AstWhile =>
        val condBlock = new BasicBlock("whileCond", fun)
        val bodyBlock = new BasicBlock("whileBody", fun)
        val contBlock = new BasicBlock("whileCont", fun)

        emit(new BrInsn(condBlock, _))

        appendAndEnterBlock(condBlock)
        val cond = compileExpr(node.guard)
        emit(new CondBrInsn(cond, bodyBlock, contBlock, _))

        appendAndEnterBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(node.body)
        })
        emit(new BrInsn(condBlock, _))

        appendAndEnterBlock(contBlock)

      case node: AstDoWhile =>
        val bodyBlock = new BasicBlock("doWhileBody", fun)
        val condBlock = new BasicBlock("doWhileCond", fun)
        val contBlock = new BasicBlock("doWhileCont", fun)

        emit(new BrInsn(bodyBlock, _))

        appendAndEnterBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(node.body)
        })
        emit(new BrInsn(condBlock, _))

        appendAndEnterBlock(condBlock)
        val cond = compileExpr(node.guard)
        emit(new CondBrInsn(cond, bodyBlock, contBlock, _))

        appendAndEnterBlock(contBlock)

      case node: AstFor =>
        val condBlock = new BasicBlock("forCond", fun)
        val bodyBlock = new BasicBlock("forBody", fun)
        val incBlock = new BasicBlock("forInc", fun)
        val contBlock = new BasicBlock("forCont", fun)

        node.init.foreach(compileStmt)
        emit(new BrInsn(condBlock, _))

        appendAndEnterBlock(condBlock)
        node.guard match {
          case Some(c) =>
            val cond = compileExpr(c)
            emit(new CondBrInsn(cond, bodyBlock, contBlock, _))

          case None =>
            emit(new BrInsn(bodyBlock, _))
        }

        appendAndEnterBlock(bodyBlock)
        withBreakContTarget(contBlock, incBlock, {
          compileStmt(node.body)
        })
        emit(new BrInsn(incBlock, _))

        appendAndEnterBlock(incBlock)
        node.increment.foreach(compileExpr)
        emit(new BrInsn(condBlock, _))

        appendAndEnterBlock(contBlock)

      case node: AstSwitch =>
        val contBlock = new BasicBlock("switchCont", fun)
        val defaultBlock = new BasicBlock("switchDefault", fun)

        val cond = compileExpr(node.guard)
        val caseBlocks = node.cases.map({ case (value, _) =>
          val caseBlock = new BasicBlock(s"switchCase$value", fun)
          val caseContBlock = new BasicBlock(s"switchCase${value}Cont", fun)

          val valueImm = emit(new IImmInsn(value, _))
          val cmpEq = emit(new CmpInsn(IrOpcode.CmpIEq, cond, valueImm, _))
          emit(new CondBrInsn(cmpEq, caseBlock, caseContBlock, _))
          appendAndEnterBlock(caseContBlock)
          caseBlock
        }).toIndexedSeq

        if (node.defaultCase.isDefined)
          emit(new BrInsn(defaultBlock, _))
        else
          emit(new BrInsn(contBlock, _))

        node.cases.zipWithIndex.foreach({ case ((_, caseBody), i) =>
          val caseBlock = caseBlocks(i)

          appendAndEnterBlock(caseBlock)
          withBreakTarget(contBlock, {
            compileStmt(caseBody)
          })
          if (i < caseBlocks.size - 1)
            emit(new BrInsn(caseBlocks(i + 1), _))
          else if (node.defaultCase.isDefined)
            emit(new BrInsn(defaultBlock, _))
          else
            emit(new BrInsn(contBlock, _))
        })

        node.defaultCase.foreach(defaultBody => {
          appendAndEnterBlock(defaultBlock)
          withBreakTarget(contBlock, {
            compileStmt(defaultBody)
          })
          emit(new BrInsn(contBlock, _))
        })

        appendAndEnterBlock(contBlock)

      case node: AstContinue => contTargetOption match {
        case Some(contTarget) =>
          emit(new BrInsn(contTarget, _))
          appendAndEnterBlock(new BasicBlock("unreachable", fun))

        case None => throw new TinyCCompilerException(Error, "Unexpected continue stmt outside of loop.", node.loc)
      }

      case node: AstBreak => breakTargetOption match {
        case Some(breakTarget) =>
          emit(new BrInsn(breakTarget, _))
          appendAndEnterBlock(new BasicBlock("unreachable", fun))

        case None => throw new TinyCCompilerException(Error, "Unexpected break stmt outside of loop.", node.loc)
      }

      case node: AstReturn =>
        node.expr match {
          case Some(v) =>
            val retVal = compileExpr(v) // TODO: cast return value
            emit(new RetInsn(retVal, _))

          case None =>
            emit(new RetVoidInsn(bb))
        }
        appendAndEnterBlock(new BasicBlock("unreachable", fun))

      case node: AstWrite =>
        val value = compileExprAndCastTo(node.expr, CharTy)
        emit(new PutCharInsn(value, _))

      case node: AstWriteNum =>
        val value = compileExprAndCastTo(node.expr, IntTy)
        emit(new PutNumInsn(value, _))

      case _: AstSequence | _: AstBlock | _: AstProgram =>
        node.children.foreach(compileStmt)

      case _ => compileExpr(node)
    }

    private def compileVarDecl(node: AstVarDecl): Unit = {
      val varIrTy = compileType(node.varTy.ty)

      allocMap(VarDecl(node)) = funOption match {
        case Some(fun) if fun != entryFun => // local variable
          val alloc = emit(new AllocLInsn(varIrTy, _)).name(node.symbol.name)
          node.value.foreach(value => {
            val compiledValue = compileExprAndCastTo(value, node.varTy.ty)
            emit(new StoreInsn(alloc, compiledValue, _))
          })
          alloc

        case Some(fun) if fun == entryFun =>
          throw new UnsupportedOperationException(s"Cannot declare variable inside entry function.")

        case None => // global variable - append to entryFun
          withEntryFun({
            // globals can have forward declarations - keep track of variable names in globalMap
            val alloc = globalMap.getOrElseUpdate(node.symbol, emit(new AllocGInsn(varIrTy, Seq.empty, _)).name(node.symbol.name))
            node.value.foreach(value => {
              val compiledValue = compileExprAndCastTo(value, node.varTy.ty)
              emit(new StoreInsn(alloc, compiledValue, _))
            })
            alloc
          })
      }
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
        appendAndEnterBlock(new BasicBlock("entry", fun))

        // Load arguments into local variables (they could be mutated later in the body.
        node.args.zip(irFun.argTys).zipWithIndex.foreach({ case (((_, argName), argTy), index) =>
          val alloc = emit(new AllocLInsn(argTy, _)).name(argName.name)
          emit(new StoreInsn(alloc, emit(new LoadArgInsn(index, _)), _))
          allocMap(FunArgDecl(node, index)) = alloc
        })

        node.body.foreach(compileStmt)

        if (funTy.returnTy == Types.VoidTy)
          emit(new RetVoidInsn(bb)) // implicit return
      }
      exitFun()
    }

    private def compileMemberExprPtrHelper(structPtr: Insn, structTy: StructTy, member: Symbol): Insn = {
      val structIrTy = compileType(structTy)
      val fieldIndex = structTy match {
        case StructTy(_, Some(fields)) => fields.indexWhere(f => f._2 == member)
      }
      emit(new GetElementPtrInsn(structPtr, emitIImm(0), structIrTy, fieldIndex, _))
    }

    /** Returns instruction, whose result is a pointer to the value of the expression. */
    private def compileExprPtr(node: AstNode): Insn = node match {
      case node: AstIdentifier => node.decl match {
        case FunDecl(decl) => emit(new GetFunPtrInsn(funMap(decl.symbol), _))
        case decl => allocMap(decl)
      }

      case node: AstDeref => compileExpr(node.expr)

      case node: AstIndex =>
        val expr = compileExpr(node.expr)
        val IndexableTy(baseTy) = node.expr.ty
        val index = compileExpr(node.index)
        emit(new GetElementPtrInsn(expr, index, compileType(baseTy), 0, _))

      case node: AstMember => // .
        compileMemberExprPtrHelper(compileExprPtr(node.expr), node.expr.ty.asInstanceOf[StructTy], node.member)

      case node: AstMemberPtr => // ->
        val structPtr = compileExpr(node.expr)
        val structTy = node.expr.ty match {
          case IndexableTy(structTy: StructTy) => structTy
        }
        compileMemberExprPtrHelper(structPtr, structTy, node.member)

      case node => throw new TinyCCompilerException(Error, s"Expression '$node' is not a l-value.", node.loc)
    }

    private def compileExpr(node: AstNode): Insn = node match {
      case node: AstInteger =>
        emit(new IImmInsn(node.value, _))

      case node: AstChar =>
        emit(new IImmInsn(node.value.toLong, _))

      // TODO: AstString

      case node: AstDouble =>
        emit(new FImmInsn(node.value, _))

      case node: AstUnaryOp =>
        compileUnaryOp(node)

      case node: AstUnaryPostOp =>
        compileUnaryPostOp(node)

      case node: AstBinaryOp =>
        compileBinaryOp(node)

      case node: AstSequence =>
        node.body.map(compileExpr).lastOption
          .getOrElse(throw new TinyCCompilerException(Error, "empty sequence", node.loc))

      case _: AstRead =>
        emit(new GetCharInsn(_))

      case node: AstAssignment =>
        val lvalue = compileExprPtr(node.lvalue)
        val value = compileExprAndCastTo(node.value, node.lvalue.ty)
        emit(new StoreInsn(lvalue, value, _))
        value

      case node: AstIdentifier if node.decl.isInstanceOf[FunDecl] => node.decl match {
        case FunDecl(decl) =>
          emit(new GetFunPtrInsn(funMap(decl.symbol), _))
      }

      case node: AstAddress =>
        compileExprPtr(node.expr)

      case node: AstDeref =>
        val value = compileExpr(node.expr)
        emit(new LoadInsn(compileType(node.ty), value, _))

      case node: AstIndex =>
        val expr = compileExpr(node.expr)
        val index = compileExprAndCastTo(node.index, IntTy)

        node.expr.ty match {
          case exprTy: IndexableTy =>
            val elemTy = compileType(exprTy.baseTy)
            emit(new GetElementPtrInsn(expr, index, elemTy, 0, _))

          case _ => throw new UnsupportedOperationException("Invalid AstIndex expr type.")
        }

      case c: AstCall =>
        c.expr match {
          case id: AstIdentifier => // direct call
            id.decl match {
              case FunDecl(decl) =>
                val funTy = decl.ty.asInstanceOf[FunTy]
                emit(new CallInsn(funMap(decl.symbol), compileCallArgs(funTy.argTys, c.args).toIndexedSeq, _))
              case _ => throw new TinyCCompilerException(Error, "call of non-function", c.loc)
            }

          case fun => // indirect call
            val funTy = fun.ty.asInstanceOf[FunTy]
            val funPtr = compileExpr(fun)
            val funSig = compileFunSignature(funTy)
            emit(new CallPtrInsn(funSig, funPtr, compileCallArgs(funTy.argTys, c.args).toIndexedSeq, _))
        }

      case node: AstCast =>
        val expr = compileExpr(node.expr)
        compileCastFromTo(expr, node.expr.ty, node.ty, node.loc)

      case node =>
        val resultPtr = compileExprPtr(node)
        emit(new LoadInsn(compileType(node.ty), resultPtr, _))
    }

    @tailrec
    private def compileCastFromTo(value: Insn, fromTy: Types.Ty, toTy: Types.Ty, loc: SourceLocation): Insn = (fromTy, toTy) match {
      case (fromTy, toTy) if fromTy == toTy => value

      case (Types.CharTy, Types.IntTy) => value

      case (Types.IntTy, Types.CharTy) =>
        val imm = emitIImm(64 - 8)
        val tmp = emit(new BinaryArithInsn(IrOpcode.IShl, value, imm, _))
        emit(new BinaryArithInsn(IrOpcode.IShr, tmp, imm, _))

      case (_: Types.IntegerTy, Types.DoubleTy) =>
        emit(new CastInsn(IrOpcode.SInt64ToDouble, value, _))

      case (Types.DoubleTy, _: Types.IntegerTy) =>
        val valueInt = emit(new CastInsn(IrOpcode.DoubleToSInt64, value, _))
        compileCastFromTo(valueInt, Types.IntTy, toTy, loc)

      // Types of pointers were already checked by TypeAnalysis.
      case (Types.CharTy | Types.IntTy | _: Types.PtrTy | _: Types.ArrayTy, _: Types.PtrTy) =>
        value

      case (_: Types.PtrTy | _: Types.ArrayTy, _: Types.IntegerTy) =>
        compileCastFromTo(value, Types.IntTy, toTy, loc)

      case (fromTy, toTy) => throw new TinyCCompilerException(Error, s"cannot cast '$fromTy' to '$toTy'", loc)
    }

    private def compileExprAndCastTo(expr: AstNode, toTy: Types.Ty): Insn =
      compileCastFromTo(compileExpr(expr), expr.ty, toTy, expr.loc)

    private def compileFunSignature(ty: FunTy): IrFunSignature =
      IrFunSignature(compileType(ty.returnTy), ty.argTys.map(compileType))

    private def compileCallArgs(argTys: Seq[Ty], args: Seq[AstNode]): Seq[Insn] =
      argTys.zip(args).map({ case (ty, arg) => compileExprAndCastTo(arg, ty) })

    private def compileIncDec(op: Symbol, expr: AstNode, isPostfix: Boolean): Insn = {
      val exprPtr = compileExprPtr(expr)
      val oldValue = emit(new LoadInsn(compileType(expr.ty), exprPtr, _))
      val delta = if (op == Symbols.inc) 1 else -1

      val newValue = expr.ty match {
        case _: IntegerTy =>
          val deltaImm = emitIImm(delta)
          emit(new BinaryArithInsn(IrOpcode.IAdd, oldValue, deltaImm, _))

        case DoubleTy =>
          val deltaImm = emitFImm(delta)
          emit(new BinaryArithInsn(IrOpcode.FAdd, oldValue, deltaImm, _))

        case PtrTy(baseTy) =>
          val deltaImm = emitIImm(delta)
          emit(new GetElementPtrInsn(oldValue, deltaImm, compileType(baseTy), 0, _))
      }

      emit(new StoreInsn(exprPtr, newValue, _))
      if (isPostfix) oldValue else newValue
    }

    private def compileUnaryOp(node: AstUnaryOp): Insn = (node.op, node.expr.ty) match {
      case (Symbols.add, _) =>
        compileExpr(node.expr)

      case (Symbols.sub, _: Types.IntegerTy) =>
        val expr = compileExpr(node.expr)
        val zero = emit(new IImmInsn(0, _))
        val res = emit(new BinaryArithInsn(IrOpcode.ISub, zero, expr, _))
        compileCastFromTo(res, Types.IntTy, node.ty, node.loc)

      case (Symbols.sub, Types.DoubleTy) =>
        val expr = compileExpr(node.expr)
        val zero = emit(new FImmInsn(0, _))
        emit(new BinaryArithInsn(IrOpcode.FSub, zero, expr, _))

      case (Symbols.neg, exprTy: Types.IntegerTy) =>
        val expr = compileExpr(node.expr)
        val maxValue = emit(new IImmInsn(exprTy.maxValueLong, _))
        emit(new BinaryArithInsn(IrOpcode.IXor, expr, maxValue, _))

      case (Symbols.not, _: Types.IntegerTy | _: Types.PtrTy) =>
        val expr = compileExpr(node.expr)
        val zero = emit(new IImmInsn(0, _))
        emit(new CmpInsn(IrOpcode.CmpIEq, expr, zero, _))

      case (Symbols.not, Types.DoubleTy) =>
        val expr = compileExpr(node.expr)
        val zero = emit(new FImmInsn(0, _))
        emit(new CmpInsn(IrOpcode.CmpFEq, expr, zero, _))

      case (Symbols.inc | Symbols.dec, _) => compileIncDec(node.op, node.expr, isPostfix = false)

      case (op, argTy) => throw new NotImplementedError(s"UnaryOp '${op.name}' with '$argTy'.")
    }

    private def compileUnaryPostOp(node: AstUnaryPostOp): Insn = (node.op, node.expr.ty) match {
      case (Symbols.inc | Symbols.dec, _) => compileIncDec(node.op, node.expr, isPostfix = true)

      case (op, argTy) => throw new NotImplementedError(s"UnaryPostOp '${op.name}' with '$argTy'.")
    }

    private def compileBinaryArith(node: AstBinaryOp): Insn = (node.op, node.ty) match {
      case (Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.bitXor, resultTy: IntegerTy) =>
        val leftInt = compileExprAndCastTo(node.left, IntTy)
        val rightInt = compileExprAndCastTo(node.right, IntTy)
        val resultInt = node.op match {
          case Symbols.add => emitBinaryArith(IAdd, leftInt, rightInt)
          case Symbols.sub => emitBinaryArith(ISub, leftInt, rightInt)
          case Symbols.mul => emitBinaryArith(SMul, leftInt, rightInt)
          case Symbols.div => emitBinaryArith(SDiv, leftInt, rightInt)

          case Symbols.mod =>
            val divRes = emitBinaryArith(SDiv, leftInt, rightInt)
            val mulRes = emitBinaryArith(SMul, divRes, rightInt)
            emitBinaryArith(ISub, leftInt, mulRes)

          case Symbols.bitAnd => emitBinaryArith(IAnd, leftInt, rightInt)
          case Symbols.bitOr => emitBinaryArith(IOr, leftInt, rightInt)
          case Symbols.bitXor => emitBinaryArith(IXor, leftInt, rightInt)
        }
        compileCastFromTo(resultInt, IntTy, resultTy, node.loc)

      case (Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.mod, DoubleTy) =>
        val leftDouble = compileExprAndCastTo(node.left, DoubleTy)
        val rightDouble = compileExprAndCastTo(node.right, DoubleTy)
        node.op match {
          case Symbols.add => emitBinaryArith(FAdd, leftDouble, rightDouble)
          case Symbols.sub => emitBinaryArith(FSub, leftDouble, rightDouble)
          case Symbols.mul => emitBinaryArith(FMul, leftDouble, rightDouble)
          case Symbols.div => emitBinaryArith(FDiv, leftDouble, rightDouble)

          case Symbols.mod =>
            val divRes = emitBinaryArith(SDiv, leftDouble, rightDouble)
            val divResInt = emit(new CastInsn(IrOpcode.DoubleToSInt64, divRes, _))
            val divResFloor = emit(new CastInsn(IrOpcode.SInt64ToDouble, divResInt, _))
            val mulRes = emitBinaryArith(SMul, divResFloor, rightDouble)
            emitBinaryArith(FSub, leftDouble, mulRes)
        }

      case (Symbols.add | Symbols.sub, PtrTy(baseTy)) =>
        val left = compileExpr(node.left)
        val rightInt = compileExprAndCastTo(node.right, IntTy)
        val index = node.op match {
          case Symbols.add => rightInt
          case Symbols.sub => emitBinaryArith(ISub, emitIImm(0), rightInt)
        }
        emit(new GetElementPtrInsn(left, index, compileType(baseTy), 0, _))
    }

    private def compileCmpArithmeticHelper(op: Symbol, argTy: Types.Ty, leftPromoted: Insn, rightPromoted: Insn): Insn = (op, argTy) match {
      case (Symbols.eq, _: IntegerTy) => emitCmp(CmpIEq, leftPromoted, rightPromoted)
      case (Symbols.ne, _: IntegerTy) => emitCmp(CmpINe, leftPromoted, rightPromoted)
      case (Symbols.lt, _: IntegerTy) => emitCmp(CmpSLt, leftPromoted, rightPromoted)
      case (Symbols.le, _: IntegerTy) => emitCmp(CmpSLe, leftPromoted, rightPromoted)
      case (Symbols.gt, _: IntegerTy) => emitCmp(CmpSGt, leftPromoted, rightPromoted)
      case (Symbols.ge, _: IntegerTy) => emitCmp(CmpSGe, leftPromoted, rightPromoted)

      case (Symbols.eq, DoubleTy) => emitCmp(CmpFEq, leftPromoted, rightPromoted)
      case (Symbols.ne, DoubleTy) => emitCmp(CmpFNe, leftPromoted, rightPromoted)
      case (Symbols.lt, DoubleTy) => emitCmp(CmpFLt, leftPromoted, rightPromoted)
      case (Symbols.le, DoubleTy) => emitCmp(CmpFLe, leftPromoted, rightPromoted)
      case (Symbols.gt, DoubleTy) => emitCmp(CmpFGt, leftPromoted, rightPromoted)
      case (Symbols.ge, DoubleTy) => emitCmp(CmpFGe, leftPromoted, rightPromoted)

      case _ => throw new NotImplementedError(s"compileCmpArithmeticHelper($op, $argTy)")
    }

    private def compileCmp(node: AstBinaryOp): Insn = (node.op, node.left.ty, node.right.ty) match {
      case (op, leftTy: ArithmeticTy, rightTy: ArithmeticTy) if leftTy <= rightTy =>
        val leftPromoted = compileExprAndCastTo(node.left, rightTy)
        val right = compileExpr(node.right)
        compileCmpArithmeticHelper(op, rightTy, leftPromoted, right)

      case (op, leftTy: ArithmeticTy, rightTy: ArithmeticTy) if leftTy > rightTy =>
        val left = compileExpr(node.left)
        val rightPromoted = compileExprAndCastTo(node.right, leftTy)
        compileCmpArithmeticHelper(op, leftTy, left, rightPromoted)

      case (op, _: PtrTy | _: ArrayTy, _: PtrTy | _: ArrayTy) =>
        val left = compileExpr(node.left)
        val right = compileExpr(node.right)
        compileCmpArithmeticHelper(op, IntTy, left, right)
    }

    private def compileBinaryOp(node: AstBinaryOp): Insn = (node.op, node.left.ty, node.right.ty) match {
      case (Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.bitXor, _, _) =>
        compileBinaryArith(node)

      case (Symbols.eq | Symbols.ne | Symbols.lt | Symbols.le | Symbols.gt | Symbols.ge, _, _) =>
        compileCmp(node)

      case (Symbols.and, _, _) =>
        val leftTrueBlock = new BasicBlock("leftTrue", fun)
        val contBlock = new BasicBlock("cont", fun)

        val resultPtr = emit(new AllocLInsn(IrTy.Int64Ty, _))
        emit(new StoreInsn(resultPtr, emitIImm(0), _))

        val leftInt = compileExprAndCastTo(node.left, IntTy)
        emit(new CondBrInsn(leftInt, leftTrueBlock, contBlock, _))

        appendAndEnterBlock(leftTrueBlock)
        val rightInt = compileExprAndCastTo(node.right, IntTy)
        val tmpResult = emitCmp(CmpINe, rightInt, emitIImm(0))
        emit(new StoreInsn(resultPtr, tmpResult, _))
        emit(new BrInsn(contBlock, _))

        appendAndEnterBlock(contBlock)
        emit(new LoadInsn(IrTy.Int64Ty, resultPtr, _))

      case (Symbols.or, _, _) =>
        val leftFalseBlock = new BasicBlock("leftFalse", fun)
        val contBlock = new BasicBlock("cont", fun)

        val resultPtr = emit(new AllocLInsn(IrTy.Int64Ty, _))
        emit(new StoreInsn(resultPtr, emitIImm(1), _))

        val leftInt = compileExprAndCastTo(node.left, IntTy)
        emit(new CondBrInsn(leftInt, contBlock, leftFalseBlock, _))

        appendAndEnterBlock(leftFalseBlock)
        val rightInt = compileExprAndCastTo(node.right, IntTy)
        val tmpResult = emitCmp(CmpINe, rightInt, emitIImm(0))
        emit(new StoreInsn(resultPtr, tmpResult, _))
        emit(new BrInsn(contBlock, _))

        appendAndEnterBlock(contBlock)
        emit(new LoadInsn(IrTy.Int64Ty, resultPtr, _))
    }
  }

}
