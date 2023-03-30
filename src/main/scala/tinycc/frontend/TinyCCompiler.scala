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

final class TinyCCompiler(program: AstProgram, _declarations: Declarations, _typeMap: TypeMap) {

  import TinyCCompiler._

  implicit protected def declarations: Declarations = _declarations

  implicit protected def typeMap: TypeMap = _typeMap

  def result(): IrProgram = new _Impl().compileProgram(program)

  final private class _Impl extends TinyCIrProgramBuilder {

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
        appendAndEnterBlock(new BasicBlock("entry", fun).loc(node.loc))
      })

      compileStmt(node)

      withEntryFun({
        val mainFun = program.funs.find(_.name == "main")
          .getOrElse(throw new TinyCCompilerException("missing main function declaration", node.loc))
        if (mainFun.argTys.nonEmpty)
          throw new TinyCCompilerException("invalid main function signature (expected main())", node.loc)

        emit(new CallInsn(mainFun, IndexedSeq.empty, bb))
        emit(new HaltInsn(bb))
      })

      program
    }

    /* Visitor Methods */

    /** Compile type for use in normal situations (arguments, load, store). Compiles arrays and structs as pointers. */
    private def compileBasicType(ty: Types.Ty): IrTy = ty match {
      case VoidTy => IrTy.VoidTy
      case CharTy => IrTy.Int64Ty
      case IntTy => IrTy.Int64Ty
      case DoubleTy => IrTy.DoubleTy
      case _: PtrTy => IrTy.PtrTy

      case _: ArrayTy | _: StructTy => IrTy.PtrTy

      case _: FunTy => throw new UnsupportedOperationException(s"Cannot compile FunTy to IR type.")
    }

    /** Compile type for use in Alloc or GetElementPtr. Compiles arrays and structs as the full types. */
    private def compileVarType(ty: Ty): IrTy = ty match {
      case ArrayTy(elemTy, numElem) =>
        IrTy.ArrayTy(compileVarType(elemTy), numElem)

      case StructTy(_, fieldsOption) =>
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
        val trueBlock = new BasicBlock("ifTrue", fun).loc(node.trueCase.loc)
        val falseBlock = new BasicBlock("ifFalse", fun).loc(node.loc)
        val contBlock = new BasicBlock("ifCont", fun).loc(node.loc)

        compileBoolExpr(node.guard, trueBlock, falseBlock)

        appendAndEnterBlock(trueBlock)
        compileStmt(node.trueCase)
        emit(new BrInsn(contBlock, bb))

        appendAndEnterBlock(falseBlock)
        node.falseCase.foreach(compileStmt)
        emit(new BrInsn(contBlock, bb))

        appendAndEnterBlock(contBlock)

      case node: AstWhile =>
        val condBlock = new BasicBlock("whileCond", fun).loc(node.guard.loc)
        val bodyBlock = new BasicBlock("whileBody", fun).loc(node.body.loc)
        val contBlock = new BasicBlock("whileCont", fun).loc(node.loc)

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
        val bodyBlock = new BasicBlock("doWhileBody", fun).loc(node.body.loc)
        val condBlock = new BasicBlock("doWhileCond", fun).loc(node.guard.loc)
        val contBlock = new BasicBlock("doWhileCont", fun).loc(node.loc)

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
        val condBlock = new BasicBlock("forCond", fun).loc(node.loc)
        val bodyBlock = new BasicBlock("forBody", fun).loc(node.loc)
        val incBlock = new BasicBlock("forInc", fun).loc(node.loc)
        val contBlock = new BasicBlock("forCont", fun).loc(node.loc)

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
        val contBlock = new BasicBlock("switchCont", fun).loc(node.loc)
        val defaultBlock = new BasicBlock("switchDefault", fun).loc(node.loc)

        val cond = compileExprAndCastTo(node.guard, IntTy)
        val caseBlocks = node.cases.map({ case (value, body) =>
          val caseBlock = new BasicBlock(s"switchCase$value", fun).loc(body.loc)
          val caseContBlock = new BasicBlock(s"switchCase${value}Cont", fun).loc(node.loc)

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
          appendAndEnterBlock(new BasicBlock("unreachable", fun)).loc(node.loc)

        case None => throw new TinyCCompilerException("Unexpected continue stmt outside of loop.", node.loc)
      }

      case node: AstBreak => breakTargetOption match {
        case Some(breakTarget) =>
          emit(new BrInsn(breakTarget, bb))
          appendAndEnterBlock(new BasicBlock("unreachable", fun)).loc(node.loc)

        case None => throw new TinyCCompilerException("Unexpected break stmt outside of loop.", node.loc)
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
        appendAndEnterBlock(new BasicBlock("unreachable", fun)).loc(node.loc)

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
        case Some(fun) if fun == entryFun =>
          throw new UnsupportedOperationException(s"Cannot declare variable inside entry function.")

        case Some(_) => // local variable
          val alloc = emit(new AllocLInsn(varIrTy, bb)).name("local_" + node.symbol.name)
          node.value.foreach(value => compileAssignment(varTy, alloc, value, node.loc))
          alloc

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

      if(node.symbol.name == entryFun.name)
        throw new TinyCCompilerException(s"identifier ${node.symbol.name} is reserved and cannot be used", node.loc)

      appendAndEnterFun(irFun)
      curFunTyOption = Some(funTy)

      node.body.foreach(body => {
        appendAndEnterBlock(new BasicBlock("entry", fun).loc(body.loc))

        val loadArgOffset = funTy.returnTy match {
          case _: StructTy => 1 // first argument is pointer to the returned struct
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

        compileStmt(body)

        if (funTy.returnTy == VoidTy)
          emit(new RetVoidInsn(bb)) // implicit return

        checkUnterminatedBlocks(irFun)
      })
      curFunTyOption = None
      exitFun()
    }

    private def checkUnterminatedBlocks(fun: IrFun): Unit = {
      val visited = mutable.Set.empty[BasicBlock]
      def dfs(bb: BasicBlock): Unit = {
        if(visited.contains(bb))
          return
        visited += bb
        val terminator = bb.terminatorOption.getOrElse(throw new TinyCCompilerException("missing return statement in reachable block", bb.loc))
        terminator.succBlocks.foreach(dfs)
      }
      dfs(fun.entryBlock)
      fun.basicBlocks.foreach(bb => {
        if(!visited.contains(bb) && bb.terminatorOption.isEmpty) // unterminated unreachable block
          bb.append(new HaltInsn(bb)) // append halt, so the IrProgram is valid
      })
    }

    private def compileMemberExprPtrHelper(structPtr: Insn, structTy: StructTy, member: Symbol): Insn = {
      val structIrTy = compileVarType(structTy)
      val StructTy(_, Some(fields)) = structTy
      val fieldIndex = fields.indexWhere(f => f._2 == member)
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
        val PtrTyBase(baseTy) = node.expr.ty
        val index = compileExpr(node.index)
        emit(new GetElementPtrInsn(ptr, index, compileVarType(baseTy), 0, bb))

      case node: AstMember => // .
        compileMemberExprPtrHelper(compileExprPtr(node.expr), node.expr.ty.asInstanceOf[StructTy], node.member)

      case node: AstMemberPtr => // ->
        val structPtr = compileExpr(node.expr)
        val PtrTyBase(structTy: StructTy) = node.expr.ty
        compileMemberExprPtrHelper(structPtr, structTy, node.member)

      case node => throw new TinyCCompilerException(s"expression '$node' is not a l-value", node.loc)
    }

    /** Compiles the AstExpression and returns the IR instruction, which holds the result.
     * If the result of the expression is an static array or struct, returns address of the first element/field. */
    private def compileExpr(node: AstNode): Insn = node match {
      case node: AstInteger => emitIImm(node.value)

      case node: AstChar => emitIImm(node.value.toLong)

      case node: AstString =>
        // allocate global variable for the string (deduplicated using [[stringMap]])
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
          .getOrElse(throw new TinyCCompilerException("empty sequence", node.loc))

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
              case _ => throw new TinyCCompilerException("call of non-function", c.loc)
            }

          case expr => // indirect call
            // the value of expr is address of the function
            val PtrTy(funTy: FunTy) = expr.ty
            val funPtr = compileExpr(expr)
            emit(new CallPtrInsn(compileFunType(funTy), funPtr, compileCallArgs(funTy, c.args.toIndexedSeq), bb))
        }

      case node: AstCast =>
        val expr = compileExpr(node.expr)
        compileCastFromTo(expr, node.expr.ty, node.ty, node.loc)

      case _: AstIdentifier | _: AstMember | _: AstMemberPtr => node.ty match {
        // for functions, return theirs address
        // the type of function identifier is pointer to a function
        // for static arrays & structs, return address of the first element/field
        case _: FunTy | _: ArrayTy | _: StructTy => compileExprPtr(node)

        case _ => emit(new LoadInsn(compileBasicType(node.ty), compileExprPtr(node), bb))
      }

      case node: AstDeref => node.expr.ty match {
        // dereferencing (*) function pointer is noop
        // int foo();
        // *foo == foo == &foo == *(&foo) etc.
        case PtrTy(_: FunTy) => compileExpr(node.expr)

        case _ => emit(new LoadInsn(compileBasicType(node.ty), compileExprPtr(node), bb))
      }

      case node => emit(new LoadInsn(compileBasicType(node.ty), compileExprPtr(node), bb))
    }

    private def compileAssignment(destTy: Ty, destPtr: Insn, srcNode: AstNode, loc: SourceLocation): Insn = {
      val value = compileExprAndCastTo(srcNode, destTy)
      destTy match {
        case ty: StructTy => compileStructCopy(ty, destPtr, value, loc)
        case _ => emit(new StoreInsn(destPtr, value, bb))
      }
      value
    }

    private def compileStructCopy(ty: StructTy, destPtr: Insn, srcPtr: Insn, loc: SourceLocation): Unit = {
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
        val memcpyFun = program.funs.find(_.name == "memcpy").getOrElse(throw new TinyCCompilerException("an implementation of memcpy is required for large struct support", loc))
        if (memcpyFun.signature != IrFunSignature(IrTy.VoidTy, IndexedSeq(IrTy.PtrTy, IrTy.PtrTy, IrTy.Int64Ty)))
          throw new TinyCCompilerException("invalid memcpy function signature (expected 'void memcpy(void* dest, void* src, int n)' )", loc)
        emit(new CallInsn(memcpyFun, IndexedSeq(destPtr, srcPtr, emit(new SizeOfInsn(irTy, bb))), bb))
      }
    }

    @tailrec
    private def compileCastFromTo(value: Insn, fromTy: Ty, toTy: Ty, loc: SourceLocation): Insn = (fromTy, toTy) match {
      case (fromTy, toTy) if fromTy == toTy => value

      case (CharTy, IntTy) => value

      case (IntTy, CharTy) =>
        val imm = emitIImm(64 - 8)
        val tmp = emit(new BinaryArithInsn(IrOpcode.IShl, value, imm, bb))
        emit(new BinaryArithInsn(IrOpcode.IShr, tmp, imm, bb))

      case (_: IntegerTy, DoubleTy) =>
        emit(new CastInsn(IrOpcode.SInt64ToDouble, value, bb))

      case (DoubleTy, _: IntegerTy) =>
        val valueInt = emit(new CastInsn(IrOpcode.DoubleToSInt64, value, bb))
        compileCastFromTo(valueInt, IntTy, toTy, loc)

      // Types of pointers were already checked by TypeAnalysis.
      case (CharTy | IntTy | _: PtrTy | _: ArrayTy, _: PtrTyBase) =>
        value

      case (_: PtrTyBase, _: IntegerTy) =>
        compileCastFromTo(value, IntTy, toTy, loc)

      case (fromTy, toTy) => throw new TinyCCompilerException(s"cannot cast '$fromTy' to '$toTy'", loc)
    }

    private def compileExprAndCastTo(expr: AstNode, toTy: Ty): Insn =
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

        case PtrTyBase(baseTy) =>
          val deltaImm = emitIImm(delta)
          emit(new GetElementPtrInsn(oldValue, deltaImm, compileVarType(baseTy), 0, bb))

        case exprTy => throw new TinyCCompilerException(s"cannot compile unary $op with $exprTy", expr.loc)
      }

      emit(new StoreInsn(exprPtr, newValue, bb))
      if (isPostfix) oldValue else newValue
    }

    private def compileUnaryOp(node: AstUnaryOp): Insn = (node.op, node.expr.ty) match {
      case (Symbols.add, _) =>
        compileExpr(node.expr)

      case (Symbols.sub, _: IntegerTy) =>
        val expr = compileExpr(node.expr)
        val res = emit(new BinaryArithInsn(IrOpcode.ISub, emitIImm(0), expr, bb))
        compileCastFromTo(res, IntTy, node.ty, node.loc)

      case (Symbols.sub, DoubleTy) =>
        val expr = compileExpr(node.expr)
        emit(new BinaryArithInsn(IrOpcode.FSub, emitFImm(0), expr, bb))

      case (Symbols.neg, exprTy: IntegerTy) =>
        val expr = compileExpr(node.expr)
        emit(new BinaryArithInsn(IrOpcode.IXor, expr, emitIImm(exprTy.longBitmask), bb))

      case (Symbols.not, DoubleTy) =>
        val expr = compileExpr(node.expr)
        emit(new CmpInsn(IrOpcode.CmpFEq, expr, emitFImm(0), bb))

      case (Symbols.not, _: ScalarTy) => // int, char, pointer
        val expr = compileExpr(node.expr)
        emit(new CmpInsn(IrOpcode.CmpIEq, expr, emitIImm(0), bb))

      case (Symbols.inc | Symbols.dec, _) => compileIncDec(node.op, node.expr, isPostfix = false)

      case (op, argTy) => throw new TinyCCompilerException(s"cannot compile unary op $op with $argTy", node.loc)
    }

    private def compileUnaryPostOp(node: AstUnaryPostOp): Insn = (node.op, node.expr.ty) match {
      case (Symbols.inc | Symbols.dec, _) => compileIncDec(node.op, node.expr, isPostfix = true)

      case (op, argTy) => throw new TinyCCompilerException(s"cannot compile unary post op $op with $argTy", node.loc)
    }

    private def compileBinaryArith(node: AstBinaryOp): Insn = (node.op, node.ty) match {
      // subtracting two pointers from each other
      case (Symbols.sub, IntTy) if node.left.ty.isInstanceOf[PtrTyBase] && node.right.ty.isInstanceOf[PtrTyBase] =>
        val left = compileExpr(node.left)
        val right = compileExpr(node.right)
        val deltaWords = emitBinaryArith(ISub, left, right)
        val PtrTyBase(baseTy) = node.left.ty
        val baseSize = emit(new SizeOfInsn(compileVarType(baseTy), bb))
        emitBinaryArith(UDiv, deltaWords, baseSize)

      case (Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.bitXor | Symbols.shiftLeft | Symbols.shiftRight, resultTy: IntegerTy) =>
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
          case Symbols.shiftLeft => emitBinaryArith(IShl, leftInt, rightInt)
          case Symbols.shiftRight => emitBinaryArith(IShr, leftInt, rightInt)
        }
        compileCastFromTo(resultInt, IntTy, resultTy, node.loc)

      case (Symbols.add | Symbols.sub | Symbols.mul | Symbols.div, DoubleTy) =>
        val leftDouble = compileExprAndCastTo(node.left, DoubleTy)
        val rightDouble = compileExprAndCastTo(node.right, DoubleTy)
        node.op match {
          case Symbols.add => emitBinaryArith(FAdd, leftDouble, rightDouble)
          case Symbols.sub => emitBinaryArith(FSub, leftDouble, rightDouble)
          case Symbols.mul => emitBinaryArith(FMul, leftDouble, rightDouble)
          case Symbols.div => emitBinaryArith(FDiv, leftDouble, rightDouble)
        }

      case (Symbols.add | Symbols.sub, PtrTyBase(baseTy)) => // add/sub offset from pointer
        val left = compileExpr(node.left)
        val rightInt = compileExprAndCastTo(node.right, IntTy)
        val index = node.op match {
          case Symbols.add => rightInt
          case Symbols.sub => emitBinaryArith(ISub, emitIImm(0), rightInt)
        }
        emit(new GetElementPtrInsn(left, index, compileVarType(baseTy), 0, bb))

      case (op, resultTy) => throw new TinyCCompilerException(s"cannot compile binary op ${op.name} as $resultTy", node.loc)
    }

    private def compileCmpArithmeticHelper(op: Symbol, argTy: Ty, leftPromoted: Insn, rightPromoted: Insn): Insn = (op, argTy) match {
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

      case _ => throw new UnsupportedOperationException(s"cannot compile arithmetic cmp ${op.name} with $argTy")
    }

    private def compileCmp(node: AstBinaryOp): Insn = (node.op, node.left.ty, node.right.ty) match {
      case (op, leftTy: ArithmeticTy, rightTy: ArithmeticTy) =>
        val resultTy = ArithmeticTy.getResultTy(leftTy, rightTy)
        val left = compileExprAndCastTo(node.left, resultTy)
        val right = compileExprAndCastTo(node.right, resultTy)
        compileCmpArithmeticHelper(op, resultTy, left, right)

      case (op, _: PtrTyBase, _: PtrTyBase) => // compare addresses
        val left = compileExpr(node.left)
        val right = compileExpr(node.right)
        compileCmpArithmeticHelper(op, IntTy, left, right)

      case (op, leftTy, rightTy) => throw new TinyCCompilerException(s"cannot compile cmp ${op.name} with $leftTy and $rightTy", node.loc)
    }

    private def compileBinaryOp(node: AstBinaryOp): Insn = node.op match {
      case Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.bitXor | Symbols.shiftLeft | Symbols.shiftRight =>
        compileBinaryArith(node)

      case Symbols.eq | Symbols.ne | Symbols.lt | Symbols.le | Symbols.gt | Symbols.ge =>
        compileCmp(node)

      case Symbols.and | Symbols.or =>
        val trueBlock = new BasicBlock("true", fun).loc(node.loc)
        val falseBlock = new BasicBlock("false", fun).loc(node.loc)
        val contBlock = new BasicBlock("cont", fun).loc(node.loc)

        compileBoolExpr(node, trueBlock, falseBlock)
        appendAndEnterBlock(trueBlock)
        val ione = emitIImm(1)
        emit(new BrInsn(contBlock, bb))
        appendAndEnterBlock(falseBlock)
        val izero = emitIImm(0)
        emit(new BrInsn(contBlock, bb))
        appendAndEnterBlock(contBlock)
        emit(PhiInsn(IndexedSeq((ione, trueBlock), (izero, falseBlock)), bb))

      case op => throw new TinyCCompilerException(s"invalid binary operator ${op.name}", node.loc)
    }

    private def compileBoolExpr(node: AstNode, trueBlock: BasicBlock, falseBlock: BasicBlock): Unit = node match {
      case AstBinaryOp(Symbols.and, left, right, _) =>
        val leftTrueBlock = new BasicBlock("leftTrue", fun).loc(node.loc)

        compileBoolExpr(left, leftTrueBlock, falseBlock)
        appendAndEnterBlock(leftTrueBlock)
        compileBoolExpr(right, trueBlock, falseBlock)

      case AstBinaryOp(Symbols.or, left, right, _) =>
        val leftFalseBlock = new BasicBlock("leftFalse", fun).loc(node.loc)

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
  class TinyCCompilerException(message: String, val loc: SourceLocation) extends ProgramException("compiler: " + message) {
    override def format(reporter: Reporter): String = reporter.formatError(ErrorLevel.Error, getMessage, loc)
  }

  trait TinyCIrProgramBuilder extends IrProgramBuilder {
    val program: IrProgram = new IrProgram
    val entryFun: IrFun = program.appendEntryFun()

    var breakTargetOption: Option[BasicBlock] = None
    var contTargetOption: Option[BasicBlock] = None

    val blockLocations: mutable.Map[BasicBlock, SourceLocation] = mutable.Map.empty

    implicit class BasicBlockLoc(that: BasicBlock) {
      def loc(loc: SourceLocation): BasicBlock = {
        blockLocations(that) = loc
        that
      }

      def loc: SourceLocation = blockLocations.getOrElse(that, SourceLocation(1, 1, 0))
    }

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