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

        emit(new CallInsn(mainFun, IndexedSeq.empty, _))
        emit(new HaltInsn(_))
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

        val cond = compileExprAndCastToBool(node.guard)
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
        val cond = compileExprAndCastToBool(node.guard)
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
        val cond = compileExprAndCastToBool(node.guard)
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
            val cond = compileExprAndCastToBool(c)
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

        val cond = compileExprAndCastTo(node.guard, IntTy)
        val caseBlocks = node.cases.map({ case (value, _) =>
          val caseBlock = new BasicBlock(s"switchCase$value", fun)
          val caseContBlock = new BasicBlock(s"switchCase${value}Cont", fun)

          val cmpEq = emit(new CmpInsn(IrOpcode.CmpIEq, cond, emitIImm(value), _))
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
        curFunTy.returnTy match {
          case VoidTy => emit(new RetVoidInsn(_))

          case returnTy: StructTy =>
            val retStructDest = emit(new LoadArgInsn(0, _)) // first argument contains the destination pointer
            val retStructPtr = compileExprAndCastTo(node.expr.get, curFunTy.returnTy)
            compileStructCopy(returnTy, retStructDest, retStructPtr, node.loc)
            emit(new RetInsn(retStructDest, _))

          case _ =>
            val retVal = compileExprAndCastTo(node.expr.get, curFunTy.returnTy)
            emit(new RetInsn(retVal, _))
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
      val varTy = node.varTy.ty
      val varIrTy = compileVarType(varTy)

      allocMap(VarDecl(node)) = funOption match {
        case Some(fun) if fun != entryFun => // local variable
          val alloc = emit(new AllocLInsn(varIrTy, _)).name("local_" + node.symbol.name)
          node.value.foreach(value => compileAssignment(varTy, alloc, value, node.loc))
          alloc

        case Some(fun) if fun == entryFun =>
          throw new UnsupportedOperationException(s"Cannot declare variable inside entry function.")

        case None => // global variable - append to entryFun
          withEntryFun({
            // globals can have forward declarations - keep track of variable names in globalMap
            val alloc = globalMap.getOrElseUpdate(node.symbol, emit(new AllocGInsn(varIrTy, Seq.empty, _)).name("global_" + node.symbol.name))
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
          val alloc = emit(new AllocLInsn(varTy, _)).name(s"arg_" + argName.name)
          val argValue = emit(new LoadArgInsn(index + loadArgOffset, _))

          argTy match {
            case argTy: StructTy =>
              // argValue contains pointer to the first field
              compileStructCopy(argTy, alloc, argValue, node.loc)

            case _ => emit(new StoreInsn(alloc, argValue, _))
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
        val ptr = compileExpr(node.expr)
        val IndexableTy(baseTy) = node.expr.ty
        val index = compileExpr(node.index)
        emit(new GetElementPtrInsn(ptr, index, compileVarType(baseTy), 0, _))

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
        val varIrTy = compileVarType(node.ty)
        val initData = (node.value + 0.toChar).map(_.toLong)
        emit(new AllocGInsn(varIrTy, initData, _))

      case node: AstDouble => emitFImm(node.value)

      case node: AstUnaryOp => compileUnaryOp(node)

      case node: AstUnaryPostOp => compileUnaryPostOp(node)

      case node: AstBinaryOp => compileBinaryOp(node)

      case node: AstSequence =>
        node.body.map(compileExpr).lastOption
          .getOrElse(throw new TinyCCompilerException(Error, "empty sequence", node.loc))

      case _: AstRead => emit(new GetCharInsn(_))

      case node: AstAssignment =>
        compileAssignment(node.lvalue.ty, compileExprPtr(node.lvalue), node.value, node.loc)

      case node: AstAddress => compileExprPtr(node.expr)

      case c: AstCall =>
        c.expr match {
          case id: AstIdentifier => // direct call
            id.decl match {
              case FunDecl(decl) =>
                val funTy = decl.ty.asInstanceOf[FunTy]
                emit(new CallInsn(funMap(decl.symbol), compileCallArgs(funTy, c.args.toIndexedSeq), _))
              case _ => throw new TinyCCompilerException(Error, "call of non-function", c.loc)
            }

          case fun => // indirect call
            val funTy = fun.ty.asInstanceOf[FunTy]
            val funPtr = compileExpr(fun)
            emit(new CallPtrInsn(compileFunType(funTy), funPtr, compileCallArgs(funTy, c.args.toIndexedSeq), _))
        }

      case node: AstCast =>
        val expr = compileExpr(node.expr)
        compileCastFromTo(expr, node.expr.ty, node.ty, node.loc)

      case _: AstIdentifier | _: AstMember | _: AstMemberPtr => node.ty match {
        // for functions, return theirs address
        // for static arrays & structs, return address of the first element/field
        case _: FunTy | _: ArrayPtrTy | _: StructTy => compileExprPtr(node)

        case _ => emit(new LoadInsn(compileBasicType(node.ty), compileExprPtr(node), _))
      }

      case node => emit(new LoadInsn(compileBasicType(node.ty), compileExprPtr(node), _))
    }

    private def compileAssignment(destTy: Types.Ty, destPtr: Insn, srcNode: AstNode, loc: SourceLocation): Insn = {
      val value = compileExprAndCastTo(srcNode, destTy)
      destTy match {
        case ty: StructTy => compileStructCopy(ty, destPtr, value, loc)
        case _ => emit(new StoreInsn(destPtr, value, _))
      }
      value
    }

    private def compileStructCopy(ty: Types.StructTy, destPtr: Insn, srcPtr: Insn, loc: SourceLocation): Unit = {
      def compileSmallValueCopy(irTy: IrTy, destPtr: Insn, srcPtr: Insn): Unit = irTy match {
        case IrTy.VoidTy =>
        case IrTy.Int64Ty | IrTy.DoubleTy =>
          emit(new StoreInsn(destPtr, emit(new LoadInsn(irTy, srcPtr, _)), _))

        case IrTy.StructTy(fields) =>
          fields.zipWithIndex.foreach({ case (fieldTy, fieldIndex) =>
            val izero = emitIImm(0)
            val destFieldPtr = emit(new GetElementPtrInsn(destPtr, izero, irTy, fieldIndex, _))
            val srcFieldPtr = emit(new GetElementPtrInsn(srcPtr, izero, irTy, fieldIndex, _))
            compileSmallValueCopy(fieldTy, destFieldPtr, srcFieldPtr)
          })

        case IrTy.ArrayTy(baseTy, numElem) =>
          0.until(numElem).foreach(index => {
            val indexImm = emitIImm(index)
            val destFieldPtr = emit(new GetElementPtrInsn(destPtr, indexImm, baseTy, 0, _))
            val srcFieldPtr = emit(new GetElementPtrInsn(srcPtr, indexImm, baseTy, 0, _))
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
        emit(new CallInsn(memcpyFun, IndexedSeq(destPtr, srcPtr, emit(new SizeOfInsn(irTy, _))), _))
      }
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
          val alloc = emit(new AllocLInsn(compileVarType(ty.returnTy), _)).name(s"struct_ret")
          alloc +: compileOrigArgs()

        case _ => compileOrigArgs()
      }
    }

    private def compileIncDec(op: Symbol, expr: AstNode, isPostfix: Boolean): Insn = {
      val exprPtr = compileExprPtr(expr)
      val oldValue = emit(new LoadInsn(compileBasicType(expr.ty), exprPtr, _))
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
          emit(new GetElementPtrInsn(oldValue, deltaImm, compileVarType(baseTy), 0, _))
      }

      emit(new StoreInsn(exprPtr, newValue, _))
      if (isPostfix) oldValue else newValue
    }

    private def compileUnaryOp(node: AstUnaryOp): Insn = (node.op, node.expr.ty) match {
      case (Symbols.add, _) =>
        compileExpr(node.expr)

      case (Symbols.sub, _: Types.IntegerTy) =>
        val expr = compileExpr(node.expr)
        val res = emit(new BinaryArithInsn(IrOpcode.ISub, emitIImm(0), expr, _))
        compileCastFromTo(res, Types.IntTy, node.ty, node.loc)

      case (Symbols.sub, Types.DoubleTy) =>
        val expr = compileExpr(node.expr)
        emit(new BinaryArithInsn(IrOpcode.FSub, emitFImm(0), expr, _))

      case (Symbols.neg, exprTy: Types.IntegerTy) =>
        val expr = compileExpr(node.expr)
        emit(new BinaryArithInsn(IrOpcode.IXor, expr, emitIImm(exprTy.longBitmask), _))

      case (Symbols.not, _: Types.IntegerTy | _: Types.PtrTy) =>
        val expr = compileExpr(node.expr)
        emit(new CmpInsn(IrOpcode.CmpIEq, expr, emitIImm(0), _))

      case (Symbols.not, Types.DoubleTy) =>
        val expr = compileExpr(node.expr)
        emit(new CmpInsn(IrOpcode.CmpFEq, expr, emitFImm(0), _))

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
        emit(new GetElementPtrInsn(left, index, compileVarType(baseTy), 0, _))
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