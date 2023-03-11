package tinycc.common.ir.parser

import tinycc.common.ir._
import tinycc.common.ir.parser.Lexer.Token
import tinycc.util.parsing.ParserException
import tinycc.util.parsing.combinator._

import scala.collection.mutable
import scala.language.implicitConversions

object IrParser extends Parsers {

  import IrOpcode._
  import IrTy._
  import Symbols._
  import Token._

  type Input = Reader[Token]

  def elem[R](message: String, fn: PartialFunction[Token, R]): Parser[R] = (in: Input) => in.headOption match {
    case Some(tok) => fn.andThen(Accept(_, in.tail)).applyOrElse(tok, (_: Token) => Reject(message, in))
    case None => Reject(message, in)
  }

  type Context = IrProgramBuilder with RefPatcher

  implicit def symbol2parser(symbol: Symbol): Parser[Symbol] = elem(s"'${symbol.name}'", { case Special(value) if value == symbol => value })

  implicit def opcode2parser[T <: IrOpcode](op: T): Parser[T] = elem(s"'${op}'", { case Identifier(value) if value.name == op.toString => op })

  lazy val identifier: Parser[Symbol] = elem("identifier", { case Identifier(value) => value })
  lazy val register: Parser[Option[Symbol]] = elem("register", { case Register(value) => value })
  lazy val integer: Parser[Long] = elem("integer literal", { case IntLiteral(value) => value })
  lazy val double: Parser[Double] = elem("double literal", { case DoubleLiteral(value) => value })

  lazy val nonEmptyRegister: Parser[Symbol] = register.filter(_.nonEmpty).map(_.get) described "not-null register"

  trait RefPatcher {
    private val _insnRefs: mutable.Map[(IrFun, Symbol), List[Option[Insn] => Unit]] = mutable.Map.empty.withDefaultValue(Nil)
    private val _basicBlockRefs: mutable.Map[(IrFun, Symbol), List[Option[BasicBlock] => Unit]] = mutable.Map.empty.withDefaultValue(Nil)
    private val _funRefs: mutable.Map[Symbol, List[Option[IrFun] => Unit]] = mutable.Map.empty.withDefaultValue(Nil)

    def registerInsnRef(fun: IrFun, name: Symbol, resolve: Option[Insn] => Unit): Unit = _insnRefs((fun, name)) ::= resolve

    def registerBasicBlockRef(fun: IrFun, name: Symbol, resolve: Option[BasicBlock] => Unit): Unit = _basicBlockRefs((fun, name)) ::= resolve

    def registerFunRef(name: Symbol, resolve: Option[IrFun] => Unit): Unit = _funRefs(name) ::= resolve

    /** Scan the whole program and resolve registered references. */
    def patchProgram(program: IrProgram): Unit = {
      program.funs.foreach(fun => {
        _funRefs.remove(Symbol(fun.name)).foreach(list =>
          list.foreach(resolve => resolve(Some(fun))))

        fun.basicBlocks.foreach(basicBlock => {
          _basicBlockRefs.remove((fun, Symbol(basicBlock.name))).foreach(list =>
            list.foreach(resolve => resolve(Some(basicBlock))))

          basicBlock.body.foreach(insn => {
            _insnRefs.remove((fun, Symbol(insn.name))).foreach(list =>
              list.foreach(resolve => resolve(Some(insn))))
          })
        })
      })

      _insnRefs.values.flatten.foreach(resolve => resolve(None))
      _basicBlockRefs.values.flatten.foreach(resolve => resolve(None))
      _funRefs.values.flatten.foreach(resolve => resolve(None))
    }
  }

  /** Because instructions can reference not yet encountered objects, this provides means to register a callback, which will be called in the future, when a value for the reference is available. */
  type RefFuture[T] = (T => Unit) => Unit

  lazy val insnRef: Parser[Context => RefFuture[Insn]] = loc ~ register ^^ { case loc ~ name =>
    (ctx: Context) =>
      (resolve: Insn => Unit) =>
        name.foreach(name => ctx.registerInsnRef(ctx.fun, name, insnOption =>
          resolve(insnOption.getOrElse(throw new ParserException(s"undeclared instruction '$name'", loc)))))
  }

  lazy val basicBlockRef: Parser[Context => RefFuture[BasicBlock]] = loc ~ register ^^ { case loc ~ name =>
    (ctx: Context) =>
      (resolve: BasicBlock => Unit) =>
        name.foreach(name => ctx.registerBasicBlockRef(ctx.fun, name, basicBlockOption =>
          resolve(basicBlockOption.getOrElse(throw new ParserException(s"undeclared basic block '$name'", loc)))))
  }

  lazy val funRef: Parser[Context => RefFuture[IrFun]] = loc ~ identifier ^^ { case loc ~ name =>
    (ctx: Context) =>
      (resolve: IrFun => Unit) =>
        ctx.registerFunRef(name, basicBlockOption =>
          resolve(basicBlockOption.getOrElse(throw new ParserException(s"undeclared function '$name'", loc))))
  }

  lazy val PROGRAM: Parser[Context => Any] = rep(FUN_DECL) ^^ { case funGens =>
    (ctx: Context) => funGens.foreach(_(ctx))
  }

  lazy val FUN_DECL: Parser[Context => Any] =
    (kwFn ~> RET_TYPE) ~ identifier ~ (parOpen ~> repsep(ARG_TYPE, comma)) ~ (parClose ~> curlyOpen ~> FUN_BODY) <~ curlyClose ^^ {
      case returnTy ~ name ~ argTys ~ bodyGen => (ctx: Context) => {
        ctx.appendAndEnterFun(new IrFun(name.name, returnTy, argTys.toIndexedSeq, _))
        bodyGen(ctx)
      }
    } described "function declaration"

  lazy val FUN_BODY: Parser[Context => Any] = rep1(BASIC_BLOCK) ^^ { case blockGens =>
    (ctx: Context) => blockGens.foreach(_(ctx))
  } described "function body"

  lazy val BASIC_BLOCK: Parser[Context => Any] = identifier ~ (colon ~> rep(INSN)) ^^ { case name ~ bodyGens =>
    (ctx: Context) => {
      ctx.appendAndEnterBlock(name.name)
      bodyGens.foreach(_(ctx))
    }
  } described "basic block"

  lazy val INSN: Parser[Context => Any] = nonEmptyRegister ~ (assign ~> (
    IIMM | FIMM | BINARY_ARITH | CMP | ALLOCG | ALLOCL | LOAD | STORE | GETELEMENTPTR | GETFUNPTR | LOADARG | CALL | CALLPTR | PUTCHAR | PUTNUM | GETCHAR | CAST | RET | RETVOID | HALT | BR | CONDBR
    )) ^^ { case name ~ insnGen =>
    (ctx: Context) => {
      val insn = insnGen(ctx).name(name.name)
      assert(insn.name == name.name)
    }
  } described "instruction"

  lazy val IIMM: Parser[Context => IImmInsn] = IImm ~> integer ^^ { value => _.emitIImm(value) }

  lazy val FIMM: Parser[Context => FImmInsn] = FImm ~> (integer ^^ (_.toDouble) | double) ^^ { value => _.emitFImm(value) }

  lazy val BINARY_ARITH: Parser[Context => BinaryArithInsn] =
    (IAdd | ISub | IAnd | IOr | IXor | IShl | IShr | UMul | SMul | SDiv | FAdd | FSub | FMul | FDiv) ~ insnRef ~ (comma ~> insnRef) ^^ { case op ~ leftFuture ~ rightFuture =>
      (ctx: Context) => {
        val insn = ctx.emit(new BinaryArithInsn(op, None, None, _))
        leftFuture(ctx)(insn.leftRef.apply)
        rightFuture(ctx)(insn.rightRef.apply)
        insn
      }
    }

  lazy val CMP: Parser[Context => CmpInsn] =
    (CmpIEq | CmpINe | CmpULt | CmpULe | CmpUGt | CmpUGe | CmpSLt | CmpSLe | CmpSGe | CmpFEq | CmpFNe | CmpFLt | CmpFGt | CmpFGe) ~ insnRef ~ (comma ~> insnRef) ^^ { case op ~ leftFuture ~ rightFuture =>
      (ctx: Context) => {
        val insn = ctx.emit(new CmpInsn(op, None, None, _))
        leftFuture(ctx)(insn.leftRef.apply)
        rightFuture(ctx)(insn.rightRef.apply)
        insn
      }
    }

  lazy val ALLOCL: Parser[Context => AllocLInsn] = AllocL ~> VAR_TYPE ^^ { varTy => _.emit(new AllocLInsn(varTy, _)) }

  lazy val ALLOCG: Parser[Context => AllocGInsn] = AllocG ~> VAR_TYPE ^^ { varTy => _.emit(new AllocGInsn(varTy, Seq.empty, _)) }

  lazy val LOAD: Parser[Context => LoadInsn] = (Load ~> SCALAR_TYPE) ~ insnRef ^^ { case valueTy ~ ptrFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new LoadInsn(valueTy, None, _))
      ptrFuture(ctx)(insn.ptrRef.apply)
      insn
    }
  }

  lazy val LOADARG: Parser[Context => LoadArgInsn] = LoadArg ~> integer ^^ { index => _.emit(new LoadArgInsn(index.toInt, _)) }

  lazy val STORE: Parser[Context => StoreInsn] = (Store ~> insnRef) ~ (comma ~> insnRef) ^^ { case ptrFuture ~ valueFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new StoreInsn(None, None, _))
      ptrFuture(ctx)(insn.ptrRef.apply)
      valueFuture(ctx)(insn.valueRef.apply)
      insn
    }
  }

  lazy val GETELEMENTPTR: Parser[Context => GetElementPtrInsn] = GetElementPtr ^^ (_ => ???) // TODO

  lazy val GETFUNPTR: Parser[Context => GetFunPtrInsn] = (GetFunPtr ~> funRef) ^^ { targetFunFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new GetFunPtrInsn(None, _))
      targetFunFuture(ctx)(insn.targetFunRef.apply)
      insn
    }
  }

  lazy val CALL: Parser[Context => CallInsn] = (Call ~> RET_TYPE) ~ funRef ~ (parOpen ~> CALL_ARGS) <~ parClose ^^ { case returnTy ~ targetFunFuture ~ args =>
    (ctx: Context) => {
      val insn = ctx.emit(new CallInsn(None, args.map(_ => None), _))
      targetFunFuture(ctx)(insn.targetFunRef.apply)
      args.zip(insn.argRefs).foreach({ case ((argTy, argFuture), argRef) => argFuture(ctx)(argRef.apply) })
      insn
    }
  }

  lazy val CALLPTR: Parser[Context => CallPtrInsn] = (CallPtr ~> RET_TYPE) ~ insnRef ~ (parOpen ~> CALL_ARGS) <~ parClose ^^ { case returnTy ~ funPtrFuture ~ args =>
    (ctx: Context) => {
      val insn = ctx.emit(new CallPtrInsn(buildIrFunSignature(returnTy, args), None, args.map(_ => None), _))
      funPtrFuture(ctx)(insn.funPtrRef.apply)
      args.zip(insn.argRefs).foreach({ case ((argTy, argFuture), argRef) => argFuture(ctx)(argRef.apply) })
      insn
    }
  }

  def buildIrFunSignature(returnTy: IrTy, args: IndexedSeq[(IrTy, _)]): IrFunSignature =
    IrFunSignature(returnTy, args.map(_._1))

  lazy val CALL_ARGS: Parser[IndexedSeq[(IrTy, Context => RefFuture[Insn])]] = repsep(ARG_TYPE ~ insnRef, comma) ^^ { list =>
    list.map({ case argTy ~ argFuture => (argTy, argFuture) }).toIndexedSeq
  }

  lazy val PUTCHAR: Parser[Context => PutCharInsn] = (PutChar ~> insnRef) ^^ { argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new PutCharInsn(None, _))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  lazy val PUTNUM: Parser[Context => PutNumInsn] = (PutNum ~> insnRef) ^^ { argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new PutNumInsn(None, _))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  lazy val GETCHAR: Parser[Context => GetCharInsn] = GetChar ^^ { _ => _.emit(new GetCharInsn(_)) }

  lazy val PHI: Parser[Context => Insn] = Phi ^^ (_ => ???) // TODO

  lazy val RET: Parser[Context => RetInsn] = (Ret ~> insnRef) ^^ { argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new RetInsn(None, _))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  lazy val RETVOID: Parser[Context => RetVoidInsn] = RetVoid ^^ { _ => _.emit(new RetVoidInsn(_)) }

  lazy val HALT: Parser[Context => HaltInsn] = Halt ^^ { _ => _.emit(new HaltInsn(_)) }

  lazy val BR: Parser[Context => BrInsn] = (Br ~> kwLabel ~> basicBlockRef) ^^ { succBlockFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new BrInsn(None, _))
      succBlockFuture(ctx)(insn.succBlockRef.apply)
      insn
    }
  }

  lazy val CONDBR: Parser[Context => CondBrInsn] = (CondBr ~> insnRef) ~ (kwLabel ~> basicBlockRef) ~ (comma ~> kwLabel ~> basicBlockRef) ^^ { case argFuture ~ trueBlockFuture ~ falseBlockFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new CondBrInsn(None, None, None, _))
      argFuture(ctx)(insn.argRef.apply)
      trueBlockFuture(ctx)(insn.trueBlockRef.apply)
      falseBlockFuture(ctx)(insn.falseBlockRef.apply)
      insn
    }
  }

  lazy val CAST: Parser[Context => CastInsn] = (BitcastInt64ToDouble | SInt64ToDouble | BitcastDoubleToInt64 | DoubleToSInt64) ~ insnRef ^^ { case op ~ argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new CastInsn(op, None, _))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  // Types

  lazy val ARG_TYPE: Parser[IrTy] = SCALAR_TYPE

  lazy val RET_TYPE: Parser[IrTy] = SCALAR_TYPE | (kwVoid ^^ { _ => VoidTy }) described "return type"


  lazy val VAR_TYPE: Parser[IrTy] = SCALAR_TYPE | STRUCT_TYPE

  lazy val SCALAR_TYPE: Parser[IrTy] = (
    (kwI64 ^^ { _ => Int64Ty }) | (kwDouble ^^ { _ => DoubleTy }) | (kwPtr ^^ { _ => PtrTy })
    ) described "scalar type"

  lazy val STRUCT_TYPE: Parser[IrTy] = (kwStruct ~> curlyOpen ~> rep1sep(VAR_TYPE, comma)) <~ curlyClose ^^ (fieldTys => StructTy(fieldTys.toIndexedSeq)) described "struct"

  override def remainderToString(in: Input): String = in.headOption.map({
    case Special(value) => s"'${value.name}'"
    case Identifier(value) => s"identifier '${value.name}'"
    case IntLiteral(value) => s"integer literal $value"
    case DoubleLiteral(value) => s"double literal $value"
    case Register(value) => s"register %${value.map(_.name).getOrElse("<null>")}"
  }).getOrElse("end of input")

  def buildProgram(gen: Context => Any): IrProgram = {
    val _program = new IrProgram
    val ctx = new IrProgramBuilder with RefPatcher {
      override def program: IrProgram = _program
    }
    gen(ctx)
    ctx.patchProgram(_program)
    _program.entryFunRef(_program.funs.find(fun => fun.name == IrProgram.entryFunName))
    _program
  }

  def parseProgram(in: Reader[Token]): IrProgram =
    parse(PROGRAM <~ EOI, in) match {
      case Accept(value, _, _) => buildProgram(value)
      case Reject(expectation, remainder, _) =>
        throw new ParserException(formatErrorMessage(expectation, remainder), remainder.loc)
    }

  def parseProgram(s: String): IrProgram = parseProgram(Lexer.Scanner(CharReader(s)))
}