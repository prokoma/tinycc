package tinycc.frontend

import tinycc.ProgramException
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir._
import tinycc.frontend.Types._
import tinycc.frontend.analysis.IdentifierDecl.{FunArgDecl, FunDecl, VarDecl}
import tinycc.frontend.analysis.{IdentifierDecl, SemanticAnalysis, TypeAnalysis}
import tinycc.frontend.ast._
import tinycc.frontend.parser.Symbols
import tinycc.util.Profiler.profile
import tinycc.util.parsing.SourceLocation
import tinycc.util.{ErrorLevel, Reporter}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

final class TinyCCompiler(program: AstProgram, _declarations: Declarations, _typeMap: TypeMap) {

  import TinyCCompiler._

  implicit protected def declarations: Declarations = _declarations

  implicit protected def typeMap: TypeMap = _typeMap

  def result(): IrProgram = new _Impl().compileProgram(program)

  final private class _Impl extends TinyCIrProgramBuilder {

    import ErrorLevel._

    /* Compiler State */

    val allocMap: mutable.Map[IdentifierDecl, AllocInsn] = mutable.Map.empty
    val globalMap: mutable.Map[Symbol, AllocGInsn] = mutable.Map.empty
    val funMap: mutable.Map[Symbol, IrFun] = mutable.Map.empty
    val stringMap: mutable.Map[String, AllocGInsn] = mutable.Map.empty
    var curFunTyOption: Option[FunTy] = None

    def curFunTy: FunTy = curFunTyOption.get

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

        emit(new CallInsn(mainFun, IndexedSeq.empty, bb))
        emit(new HaltInsn(bb))
      })

      program
    }

    /* Visitor Methods */

    /** Compile type for use in normal situations (arguments, load, store). Compiles arrays and structs as pointers. */
    private def compileBasicType(ty: Types.Ty): IrTy = ty match {
      case Types.VoidTy => IrTy.VoidTy
      case Types.CharTy => IrTy.Int64Ty
      case Types.IntTy => IrTy.Int64Ty
      case Types.DoubleTy => IrTy.DoubleTy
      case _: Types.PtrTy => IrTy.PtrTy

      case _: Types.ArrayPtrTy | _: Types.StructTy => IrTy.PtrTy

      case _: Types.FunTy => throw new UnsupportedOperationException(s"Cannot compile FunTy to IR type.")
    }

    /** Compile type for use in Alloc or GetElementPtr. Compiles arrays and structs as the full types. */
    private def compileVarType(ty: Types.Ty): IrTy = ty match {
      case Types.ArrayPtrTy(elemTy, numElem) =>
        IrTy.ArrayTy(compileVarType(elemTy), numElem)

      case Types.StructTy(_, fieldsOption) =>
        val fieldIrTys = fieldsOption match {
          case Some(fields) => fields.map({ case (fieldTy, _) => compileVarType(fieldTy) })
          case None => throw new UnsupportedOperationException(s"Cannot compile incomplete struct type.")
        }
        IrTy.StructTy(fieldIrTys)

      case ty => compileBasicType(ty)
    }

    private def compileStmt(node: AstNode): Unit = node match {
      case node: AstVarDecl => compileVarDecl(node)
      case node: AstFunDecl => compileFunDecl(node)
      case _: AstStructDecl | _: AstFunPtrDecl => // Ignore, handled by TypeAnalysis

      case node: AstIf =>
        val trueBlock = new BasicBlock("ifTrue", fun)
        val falseBlock = new BasicBlock("ifFalse", fun)
        val contBlock = new BasicBlock("ifCont", fun)

        compileBoolExpr(node.guard, trueBlock, falseBlock)

        appendAndEnterBlock(trueBlock)
        compileStmt(node.trueCase)
        emit(new BrInsn(contBlock, bb))

        appendAndEnterBlock(falseBlock)
        node.falseCase.foreach(compileStmt)
        emit(new BrInsn(contBlock, bb))

        appendAndEnterBlock(contBlock)

      case node: AstWhile =>
        val condBlock = new BasicBlock("whileCond", fun)
        val bodyBlock = new BasicBlock("whileBody", fun)
        val contBlock = new BasicBlock("whileCont", fun)

        emit(new BrInsn(condBlock, bb))

        appendAndEnterBlock(condBlock)
        compileBoolExpr(node.guard, bodyBlock, contBlock)

        appendAndEnterBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(node.body)
        })
        emit(new BrInsn(condBlock, bb))

        appendAndEnterBlock(contBlock)

      case node: AstDoWhile =>
        val bodyBlock = new BasicBlock("doWhileBody", fun)
        val condBlock = new BasicBlock("doWhileCond", fun)
        val contBlock = new BasicBlock("doWhileCont", fun)

        emit(new BrInsn(bodyBlock, bb))

        appendAndEnterBlock(bodyBlock)
        withBreakContTarget(contBlock, condBlock, {
          compileStmt(node.body)
        })
        emit(new BrInsn(condBlock, bb))

        appendAndEnterBlock(condBlock)
        compileBoolExpr(node.guard, bodyBlock, contBlock)

        appendAndEnterBlock(contBlock)

      case node: AstFor =>
        val condBlock = new BasicBlock("forCond", fun)
        val bodyBlock = new BasicBlock("forBody", fun)
        val incBlock = new BasicBlock("forInc", fun)
        val contBlock = new BasicBlock("forCont", fun)

        node.init.foreach(compileStmt)
        emit(new BrInsn(condBlock, bb))

        appendAndEnterBlock(condBlock)
        node.guard match {
          case Some(guard) => compileBoolExpr(guard, bodyBlock, contBlock)

          case None =>
            emit(new BrInsn(bodyBlock, bb))
        }

        appendAndEnterBlock(bodyBlock)
        withBreakContTarget(contBlock, incBlock, {
          compileStmt(node.body)
        })
        emit(new BrInsn(incBlock, bb))

        appendAndEnterBlock(incBlock)
        node.increment.foreach(compileExpr)
        emit(new BrInsn(condBlock, bb))

        appendAndEnterBlock(contBlock)

      case node: AstSwitch =>
        val contBlock = new BasicBlock("switchCont", fun)
        val defaultBlock = new BasicBlock("switchDefault", fun)

        val cond = compileExprAndCastTo(node.guard, IntTy)
        val caseBlocks = node.cases.map({ case (value, _) =>
          val caseBlock = new BasicBlock(s"switchCase$value", fun)
          val caseContBlock = new BasicBlock(s"switchCase${value}Cont", fun)

          val cmpEq = emit(new CmpInsn(IrOpcode.CmpIEq, cond, emitIImm(value), bb))
          emit(new CondBrInsn(cmpEq, caseBlock, caseContBlock, bb))
          appendAndEnterBlock(caseContBlock)
          caseBlock
        }).toIndexedSeq

        if (node.defaultCase.isDefined)
          emit(new BrInsn(defaultBlock, bb))
        else
          emit(new BrInsn(contBlock, bb))

        node.cases.zipWithIndex.foreach({ case ((_, caseBody), i) =>
          val caseBlock = caseBlocks(i)

          appendAndEnterBlock(caseBlock)
          withBreakTarget(contBlock, {
            compileStmt(caseBody)
          })
          if (i < caseBlocks.size - 1)
            emit(new BrInsn(caseBlocks(i + 1), bb))
          else if (node.defaultCase.isDefined)
            emit(new BrInsn(defaultBlock, bb))
          else
            emit(new BrInsn(contBlock, bb))
        })

        node.defaultCase.foreach(defaultBody => {
          appendAndEnterBlock(defaultBlock)
          withBreakTarget(contBlock, {
            compileStmt(defaultBody)
          })
          emit(new BrInsn(contBlock, bb))
        })

        appendAndEnterBlock(contBlock)

      case node: AstContinue => contTargetOption match {
        case Some(contTarget) =>
          emit(new BrInsn(contTarget, bb))
          appendAndEnterBlock(new BasicBlock("unreachable", fun))

        case None => throw new TinyCCompilerException(Error, "Unexpected continue stmt outside of loop.", node.loc)
      }

      case node: AstBreak => breakTargetOption match {
        case Some(breakTarget) =>
          emit(new BrInsn(breakTarget, bb))
          appendAndEnterBlock(new BasicBlock("unreachable", fun))

        case None => throw new TinyCCompilerException(Error, "Unexpected break stmt outside of loop.", node.loc)
      }

      case node: AstReturn =>
        curFunTy.returnTy match {
          case VoidTy => emit(new RetVoidInsn(bb))

          case returnTy: StructTy =>
            val retStructDest = emit(new LoadArgInsn(0, bb)) // first argument contains the destination pointer
            val retStructPtr = compileExprAndCastTo(node.expr.get, curFunTy.returnTy)
            compileStructCopy(returnTy, retStructDest, retStructPtr, node.loc)
            emit(new RetInsn(retStructDest, bb))

          case _ =>
            val retVal = compileExprAndCastTo(node.expr.get, curFunTy.returnTy)
            emit(new RetInsn(retVal, bb))
        }
        appendAndEnterBlock(new BasicBlock("unreachable", fun))

      case node: AstWrite =>
        val value = compileExprAndCastTo(node.expr, CharTy)
        emit(new PutCharInsn(value, bb))

      case node: AstWriteNum =>
        val value = compileExprAndCastTo(node.expr, IntTy)
        emit(new PutNumInsn(value, bb))

      case _: AstSequence | _: AstBlock | _: AstProgram =>
        node.children.foreach(compileStmt)

      case _ => compileExpr(node)
    }

    private def compileVarDecl(node: AstVarDecl): Unit = {
      val varTy = node.varTy.ty
      val varIrTy = compileVarType(varTy)

      allocMap(VarDecl(node)) = funOption match {
        case Some(fun) if fun != entryFun => // local variable
          val alloc = emit(new AllocLInsn(varIrTy, bb)).name("local_" + node.symbol.name)
          node.value.foreach(value => compileAssignment(varTy, alloc, value, node.loc))
          alloc

        case Some(fun) if fun == entryFun =>
          throw new UnsupportedOperationException(s"Cannot declare variable inside entry function.")

        case None => // global variable - append to entryFun
          withEntryFun({
            // globals can have forward declarations - keep track of variable names in globalMap
            val alloc = globalMap.getOrElseUpdate(node.symbol, emit(new AllocGInsn(varIrTy, Seq.empty, bb)).name("global_" + node.symbol.name))
            node.value.foreach(value => compileAssignment(varTy, alloc, value, node.loc))
            alloc
          })
      }
    }

    private def compileFunDecl(node: AstFunDecl): Unit = {
      val funTy = node.ty.asInstanceOf[FunTy]
      val irFun = funMap.getOrElseUpdate(node.symbol, {
        new IrFun(node.symbol.name, compileFunType(funTy), program)
      })

      appendAndEnterFun(irFun)
      curFunTyOption = Some(funTy)
      if (node.body.isDefined) {
        appendAndEnterBlock(new BasicBlock("entry", fun))

        val loadArgOffset = funTy.returnTy match {
          case _: StructTy => 1
          case _ => 0
        }

        // Load arguments into local variables (they could be mutated later in the body)
        val argNames = node.args.map(_._2)
        funTy.argTys.zip(argNames).zipWithIndex.foreach({ case ((argTy, argName), index) =>
          val varTy = compileVarType(argTy)
          val alloc = emit(new AllocLInsn(varTy, bb)).name(s"arg_" + argName.name)
          val argValue = emit(new LoadArgInsn(index + loadArgOffset, bb))

          argTy match {
            case argTy: StructTy =>
              // argValue contains pointer to the first field
              compileStructCopy(argTy, alloc, argValue, node.loc)

            case _ => emit(new StoreInsn(alloc, argValue, bb))
          }

          allocMap(FunArgDecl(node, index)) = alloc
        })

        node.body.foreach(compileStmt)

        // TODO: run dfs on the function body and check returns
        if (funTy.returnTy == Types.VoidTy)
          emit(new RetVoidInsn(bb)) // implicit return
      }
      curFunTyOption = None
      exitFun()
    }

    private def compileMemberExprPtrHelper(structPtr: Insn, structTy: StructTy, member: Symbol): Insn = {
      val structIrTy = compileVarType(structTy)
      val fieldIndex = structTy match {
        case StructTy(_, Some(fields)) => fields.indexWhere(f => f._2 == member)
      }
      emit(new GetElementPtrInsn(structPtr, emitIImm(0), structIrTy, fieldIndex, bb))
    }

    /** Returns instruction, whose result is a pointer to the value of the expression. */
    private def compileExprPtr(node: AstNode): Insn = node match {
      case node: AstIdentifier => node.decl match {
        case FunDecl(decl) => emit(new GetFunPtrInsn(funMap(decl.symbol), bb))
        case decl => allocMap(decl)
      }

      case node: AstDeref => compileExpr(node.expr)

      case node: AstIndex =>
        val ptr = compileExpr(node.expr)
        val IndexableTy(baseTy) = node.expr.ty
        val index = compileExpr(node.index)
        emit(new GetElementPtrInsn(ptr, index, compileVarType(baseTy), 0, bb))

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

    /** Compiles the AstExpression and returns the IR instruction, which holds the result.
     * If the result of the expression is an static array or struct, returns address of the first element/field. */
    private def compileExpr(node: AstNode): Insn = node match {
      case node: AstInteger => emitIImm(node.value)

      case node: AstChar => emitIImm(node.value.toLong)

      case node: AstString =>
        stringMap.getOrElseUpdate(node.value, {
          val allocName = "str_" + node.value.replaceAll("[^a-zA-Z0-9_]", "").take(8)
          val initData = (node.value + 0.toChar).map(_.toLong)
          emit(new AllocGInsn(compileVarType(node.ty), initData, bb)).name(allocName)
        })

      case node: AstDouble => emitFImm(node.value)

      case node: AstUnaryOp => compileUnaryOp(node)

      case node: AstUnaryPostOp => compileUnaryPostOp(node)

      case node: AstBinaryOp => compileBinaryOp(node)

      case node: AstSequence =>
        node.body.map(compileExpr).lastOption
          .getOrElse(throw new TinyCCompilerException(Error, "empty sequence", node.loc))

      case _: AstRead => emit(new GetCharInsn(bb))

      case node: AstAssignment =>
        compileAssignment(node.lvalue.ty, compileExprPtr(node.lvalue), node.value, node.loc)

      case node: AstAddress => compileExprPtr(node.expr)

      case c: AstCall =>
        c.expr match {
          case id: AstIdentifier => // direct call
            id.decl match {
              case FunDecl(decl) =>
                val funTy = decl.ty.asInstanceOf[FunTy]
                emit(new CallInsn(funMap(decl.symbol), compileCallArgs(funTy, c.args.toIndexedSeq), bb))
              case _ => throw new TinyCCompilerException(Error, "call of non-function", c.loc)
            }

          case fun => // indirect call
            val funTy = fun.ty.asInstanceOf[FunTy]
            val funPtr = compileExpr(fun)
            emit(new CallPtrInsn(compileFunType(funTy), funPtr, compileCallArgs(funTy, c.args.toIndexedSeq), bb))
        }

      case node: AstCast =>
        val expr = compileExpr(node.expr)
        compileCastFromTo(expr, node.expr.ty, node.ty, node.loc)

      case _: AstIdentifier | _: AstMember | _: AstMemberPtr => node.ty match {
        // for functions, return theirs address
        // for static arrays & structs, return address of the first element/field
        case _: FunTy | _: ArrayPtrTy | _: StructTy => compileExprPtr(node)

        case _ => emit(new LoadInsn(compileBasicType(node.ty), compileExprPtr(node), bb))
      }

      case node => emit(new LoadInsn(compileBasicType(node.ty), compileExprPtr(node), bb))
    }

    private def compileAssignment(destTy: Types.Ty, destPtr: Insn, srcNode: AstNode, loc: SourceLocation): Insn = {
      val value = compileExprAndCastTo(srcNode, destTy)
      destTy match {
        case ty: StructTy => compileStructCopy(ty, destPtr, value, loc)
        case _ => emit(new StoreInsn(destPtr, value, bb))
      }
      value
    }

    private def compileStructCopy(ty: Types.StructTy, destPtr: Insn, srcPtr: Insn, loc: SourceLocation): Unit = {
      def compileSmallValueCopy(irTy: IrTy, destPtr: Insn, srcPtr: Insn): Unit = irTy match {
        case IrTy.VoidTy =>
        case IrTy.Int64Ty | IrTy.DoubleTy =>
          emit(new StoreInsn(destPtr, emit(new LoadInsn(irTy, srcPtr, bb)), bb))

        case IrTy.StructTy(fields) =>
          fields.zipWithIndex.foreach({ case (fieldTy, fieldIndex) =>
            val izero = emitIImm(0)
            val destFieldPtr = emit(new GetElementPtrInsn(destPtr, izero, irTy, fieldIndex, bb))
            val srcFieldPtr = emit(new GetElementPtrInsn(srcPtr, izero, irTy, fieldIndex, bb))
            compileSmallValueCopy(fieldTy, destFieldPtr, srcFieldPtr)
          })

        case IrTy.ArrayTy(baseTy, numElem) =>
          0.until(numElem).foreach(index => {
            val indexImm = emitIImm(index)
            val destFieldPtr = emit(new GetElementPtrInsn(destPtr, indexImm, baseTy, 0, bb))
            val srcFieldPtr = emit(new GetElementPtrInsn(srcPtr, indexImm, baseTy, 0, bb))
            compileSmallValueCopy(baseTy, destFieldPtr, srcFieldPtr)
          })
      }

      val irTy = compileVarType(ty)
      if (irTy.sizeWords <= 3)
        compileSmallValueCopy(irTy, destPtr, srcPtr)
      else {
        val memcpyFun = program.funs.find(_.name == "memcpy").getOrElse(throw new TinyCCompilerException(Error, "an implementation of memcpy is required for large struct support", loc))
        if (memcpyFun.signature != IrFunSignature(IrTy.VoidTy, IndexedSeq(IrTy.PtrTy, IrTy.PtrTy, IrTy.Int64Ty)))
          throw new TinyCCompilerException(Error, "invalid memcpy function signature", loc)
        emit(new CallInsn(memcpyFun, IndexedSeq(destPtr, srcPtr, emit(new SizeOfInsn(irTy, bb))), bb))
      }
    }

    @tailrec
    private def compileCastFromTo(value: Insn, fromTy: Types.Ty, toTy: Types.Ty, loc: SourceLocation): Insn = (fromTy, toTy) match {
      case (fromTy, toTy) if fromTy == toTy => value

      case (Types.CharTy, Types.IntTy) => value

      case (Types.IntTy, Types.CharTy) =>
        val imm = emitIImm(64 - 8)
        val tmp = emit(new BinaryArithInsn(IrOpcode.IShl, value, imm, bb))
        emit(new BinaryArithInsn(IrOpcode.IShr, tmp, imm, bb))

      case (_: Types.IntegerTy, Types.DoubleTy) =>
        emit(new CastInsn(IrOpcode.SInt64ToDouble, value, bb))

      case (Types.DoubleTy, _: Types.IntegerTy) =>
        val valueInt = emit(new CastInsn(IrOpcode.DoubleToSInt64, value, bb))
        compileCastFromTo(valueInt, Types.IntTy, toTy, loc)

      // Types of pointers were already checked by TypeAnalysis.
      case (Types.CharTy | Types.IntTy | _: Types.PtrTy | _: Types.ArrayPtrTy, _: Types.PtrTy) =>
        value

      case (_: Types.PtrTy | _: Types.ArrayPtrTy, _: Types.IntegerTy) =>
        compileCastFromTo(value, Types.IntTy, toTy, loc)

      case (fromTy, toTy) => throw new TinyCCompilerException(Error, s"cannot cast '$fromTy' to '$toTy'", loc)
    }

    private def compileExprAndCastTo(expr: AstNode, toTy: Types.Ty): Insn =
      compileCastFromTo(compileExpr(expr), expr.ty, toTy, expr.loc)

    private def compileExprAndCastToBool(expr: AstNode): Insn = expr.ty match {
      case DoubleTy =>
        emitCmp(CmpFNe, compileExpr(expr), emitFImm(0))

      case _ => compileExprAndCastTo(expr, IntTy)
    }

    private def compileFunType(ty: FunTy): IrFunSignature = {
      val returnIrTy = compileBasicType(ty.returnTy)
      val origArgIrTys = ty.argTys.map(compileBasicType)

      val argIrTys = ty.returnTy match {
        case _: StructTy =>
          // pass space for the returned struct as first argument
          returnIrTy +: origArgIrTys

        case _ => origArgIrTys
      }

      IrFunSignature(returnIrTy, argIrTys)
    }

    private def compileCallArgs(ty: FunTy, args: IndexedSeq[AstNode]): IndexedSeq[Insn] = {
      def compileOrigArgs(): IndexedSeq[Insn] = ty.argTys.zip(args).map({ case (ty, arg) => compileExprAndCastTo(arg, ty) })

      ty.returnTy match {
        case _: StructTy =>
          // allocate space for returned struct and pass it as first argument
          val alloc = emit(new AllocLInsn(compileVarType(ty.returnTy), bb)).name(s"struct_ret")
          alloc +: compileOrigArgs()

        case _ => compileOrigArgs()
      }
    }

    private def compileIncDec(op: Symbol, expr: AstNode, isPostfix: Boolean): Insn = {
      val exprPtr = compileExprPtr(expr)
      val oldValue = emit(new LoadInsn(compileBasicType(expr.ty), exprPtr, bb))
      val delta = if (op == Symbols.inc) 1 else -1

      val newValue = expr.ty match {
        case _: IntegerTy =>
          val deltaImm = emitIImm(delta)
          emit(new BinaryArithInsn(IrOpcode.IAdd, oldValue, deltaImm, bb))

        case DoubleTy =>
          val deltaImm = emitFImm(delta)
          emit(new BinaryArithInsn(IrOpcode.FAdd, oldValue, deltaImm, bb))

        case PtrTy(baseTy) =>
          val deltaImm = emitIImm(delta)
          emit(new GetElementPtrInsn(oldValue, deltaImm, compileVarType(baseTy), 0, bb))
      }

      emit(new StoreInsn(exprPtr, newValue, bb))
      if (isPostfix) oldValue else newValue
    }

    private def compileUnaryOp(node: AstUnaryOp): Insn = (node.op, node.expr.ty) match {
      case (Symbols.add, _) =>
        compileExpr(node.expr)

      case (Symbols.sub, _: Types.IntegerTy) =>
        val expr = compileExpr(node.expr)
        val res = emit(new BinaryArithInsn(IrOpcode.ISub, emitIImm(0), expr, bb))
        compileCastFromTo(res, Types.IntTy, node.ty, node.loc)

      case (Symbols.sub, Types.DoubleTy) =>
        val expr = compileExpr(node.expr)
        emit(new BinaryArithInsn(IrOpcode.FSub, emitFImm(0), expr, bb))

      case (Symbols.neg, exprTy: Types.IntegerTy) =>
        val expr = compileExpr(node.expr)
        emit(new BinaryArithInsn(IrOpcode.IXor, expr, emitIImm(exprTy.longBitmask), bb))

      case (Symbols.not, _: Types.IntegerTy | _: Types.PtrTy) =>
        val expr = compileExpr(node.expr)
        emit(new CmpInsn(IrOpcode.CmpIEq, expr, emitIImm(0), bb))

      case (Symbols.not, Types.DoubleTy) =>
        val expr = compileExpr(node.expr)
        emit(new CmpInsn(IrOpcode.CmpFEq, expr, emitFImm(0), bb))

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
        }

      case (Symbols.add | Symbols.sub, PtrTy(baseTy)) =>
        val left = compileExpr(node.left)
        val rightInt = compileExprAndCastTo(node.right, IntTy)
        val index = node.op match {
          case Symbols.add => rightInt
          case Symbols.sub => emitBinaryArith(ISub, emitIImm(0), rightInt)
        }
        emit(new GetElementPtrInsn(left, index, compileVarType(baseTy), 0, bb))
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

      case (op, _: PtrTy | _: ArrayPtrTy, _: PtrTy | _: ArrayPtrTy) =>
        val left = compileExpr(node.left)
        val right = compileExpr(node.right)
        compileCmpArithmeticHelper(op, IntTy, left, right)
    }

    private def compileBinaryOp(node: AstBinaryOp): Insn = node.op match {
      case Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.bitXor =>
        compileBinaryArith(node)

      case Symbols.eq | Symbols.ne | Symbols.lt | Symbols.le | Symbols.gt | Symbols.ge =>
        compileCmp(node)

      case Symbols.and | Symbols.or =>
        val trueBlock = new BasicBlock("true", fun)
        val falseBlock = new BasicBlock("false", fun)
        val contBlock = new BasicBlock("cont", fun)

        compileBoolExpr(node, trueBlock, falseBlock)
        appendAndEnterBlock(trueBlock)
        val ione = emitIImm(1)
        emit(new BrInsn(contBlock, bb))
        appendAndEnterBlock(falseBlock)
        val izero = emitIImm(0)
        emit(new BrInsn(contBlock, bb))
        appendAndEnterBlock(contBlock)
        emit(PhiInsn(IndexedSeq((ione, trueBlock), (izero, falseBlock)), bb))

      case op => throw new UnsupportedOperationException(s"invalid binary operator $op")
    }

    private def compileBoolExpr(node: AstNode, trueBlock: BasicBlock, falseBlock: BasicBlock): Unit = node match {
      case AstBinaryOp(Symbols.and, left, right, _) =>
        val leftTrueBlock = new BasicBlock("leftTrue", fun)

        compileBoolExpr(left, leftTrueBlock, falseBlock)
        appendAndEnterBlock(leftTrueBlock)
        compileBoolExpr(right, trueBlock, falseBlock)

      case AstBinaryOp(Symbols.or, left, right, _) =>
        val leftFalseBlock = new BasicBlock("leftFalse", fun)

        compileBoolExpr(left, trueBlock, leftFalseBlock)
        appendAndEnterBlock(leftFalseBlock)
        compileBoolExpr(right, trueBlock, falseBlock)

      case AstUnaryOp(Symbols.not, expr, _) =>
        compileBoolExpr(expr, falseBlock, trueBlock)

      case _ =>
        emit(new CondBrInsn(compileExprAndCastToBool(node), trueBlock, falseBlock, bb))
    }
  }
}

object TinyCCompiler {
  class TinyCCompilerException(val level: ErrorLevel, message: String, val loc: SourceLocation) extends ProgramException(message) {
    override def format(reporter: Reporter): String = reporter.formatError(level, message, loc)
  }

  trait TinyCIrProgramBuilder extends IrProgramBuilder {
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

  def apply(program: AstProgram): TinyCCompiler = {
    val declarations = profile("semanticAnalysis", new SemanticAnalysis(program).result())
    val typeMap = profile("typeAnalysis", new TypeAnalysis(program, declarations).result())
    new TinyCCompiler(program, declarations, typeMap)
  }
}