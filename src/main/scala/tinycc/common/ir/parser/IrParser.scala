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
  lazy val register: Parser[Symbol] = elem("register", { case Register(value) => value })
  lazy val integer: Parser[Long] = elem("integer literal", { case IntLiteral(value) => value })
  lazy val double: Parser[Double] = elem("double literal", { case DoubleLiteral(value) => value })

  trait RefPatcher {
    private val _insnRefs: mutable.Map[(IrFun, Symbol), List[Option[Insn] => Unit]] = mutable.Map.empty.withDefaultValue(Nil)
    private val _basicBlockRefs: mutable.Map[(IrFun, Symbol), List[Option[BasicBlock] => Unit]] = mutable.Map.empty.withDefaultValue(Nil)
    private val _funRefs: mutable.Map[Symbol, List[Option[IrFun] => Unit]] = mutable.Map.empty.withDefaultValue(Nil)

    def registerInsnRef(fun: IrFun, name: Symbol, resolve: Option[Insn] => Unit): Unit = _insnRefs((fun, name)) ::= resolve

    def registerBasicBlockRef(fun: IrFun, name: Symbol, resolve: Option[BasicBlock] => Unit): Unit = _basicBlockRefs((fun, name)) ::= resolve

    def registerFunRef(name: Symbol, resolve: Option[IrFun] => Unit): Unit = _funRefs(name) ::= resolve

    /** Scan the whole program and resolve registered references. */
    def patchProgram(program: IrProgram): Unit = {
      val globals = program.globals
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

        // link refs to globals from this function
        globals.foreach(insn => {
          _insnRefs.remove((fun, Symbol(insn.name))).foreach(list =>
            list.foreach(resolve => resolve(Some(insn))))
        })
      })

      _insnRefs.values.flatten.foreach(resolve => resolve(None))
      _basicBlockRefs.values.flatten.foreach(resolve => resolve(None))
      _funRefs.values.flatten.foreach(resolve => resolve(None))
    }
  }

  /** Because instructions can reference not yet encountered objects, this provides means to register a callback, which will be called in the future, when a value for the reference is available. */
  type RefFuture[T] = (T => Unit) => Unit

  /** Universal handler for null references. */
  def emptyRef[T]: Parser[Context => RefFuture[T]] = kwNull ^^ { _ => (ctx: Context) => (resolve: T => Unit) => () }

  /** insnRef := 'null' | register */
  lazy val insnRef: Parser[Context => RefFuture[Insn]] = emptyRef[Insn] | (loc ~ register ^^ { case loc ~ name =>
    (ctx: Context) =>
      (resolve: Insn => Unit) =>
        ctx.registerInsnRef(ctx.fun, name, insnOption =>
          resolve(insnOption.getOrElse(throw new ParserException(s"undeclared instruction '$name'", loc))))
  }) described "instruction reference"

  /** basicBlockRef := 'null' | ( 'label' register ) */
  lazy val basicBlockRef: Parser[Context => RefFuture[BasicBlock]] = emptyRef[BasicBlock] | (loc ~ (kwLabel ~> register) ^^ { case loc ~ name =>
    (ctx: Context) =>
      (resolve: BasicBlock => Unit) =>
        ctx.registerBasicBlockRef(ctx.fun, name, basicBlockOption =>
          resolve(basicBlockOption.getOrElse(throw new ParserException(s"undeclared basic block '$name'", loc))))
  }) described "basic block reference"

  /** funRef := 'null' | identifier */
  lazy val funRef: Parser[Context => RefFuture[IrFun]] = emptyRef[IrFun] | (loc ~ identifier ^^ { case loc ~ name =>
    (ctx: Context) =>
      (resolve: IrFun => Unit) =>
        ctx.registerFunRef(name, basicBlockOption =>
          resolve(basicBlockOption.getOrElse(throw new ParserException(s"undeclared function '$name'", loc))))
  }) described "function reference"

  /** PROGRAM := { FUN_DECL } */
  lazy val PROGRAM: Parser[Context => Any] = rep(FUN_DECL) ^^ { case funGens =>
    (ctx: Context) => funGens.foreach(_(ctx))
  }

  /** FUN_DECL := 'fn' RET_TYPE identifier '(' ARG_TYPE { ',' ARG_TYPE } ')' '{' FUN_BODY '}' */
  lazy val FUN_DECL: Parser[Context => Any] =
    (kwFn ~> RET_TYPE) ~ identifier ~ (parOpen ~> repsep(ARG_TYPE, comma)) ~ (parClose ~> curlyOpen ~> FUN_BODY) <~ curlyClose ^^ {
      case returnTy ~ name ~ argTys ~ bodyGen => (ctx: Context) => {
        ctx.appendAndEnterFun(new IrFun(name.name, returnTy, argTys.toIndexedSeq, ctx.program))
        bodyGen(ctx)
      }
    } described "function declaration"

  /** FUN_BODY := BASIC_BLOCK { BASIC_BLOCK } */
  lazy val FUN_BODY: Parser[Context => Any] = rep1(BASIC_BLOCK) ^^ { case blockGens =>
    (ctx: Context) => blockGens.foreach(_(ctx))
  } described "function body"

  /** BASIC_BLOCK := identifier ':' { INSN } */
  lazy val BASIC_BLOCK: Parser[Context => Any] = identifier ~ (colon ~> rep(INSN)) ^^ { case name ~ bodyGens =>
    (ctx: Context) => {
      ctx.appendAndEnterBlock(new BasicBlock(name.name, ctx.fun))
      bodyGens.foreach(_(ctx))
    }
  } described "basic block"

  /** INSN := register '=' ( IIMM | FIMM | BINARY_ARITH | CMP | ALLOCG | ALLOCL | LOAD | STORE | GETELEMENTPTR | SIZEOF | GETFUNPTR | LOADARG | CALL | CALLPTR | PUTCHAR | PUTNUM | GETCHAR | PHI | CAST | RET | RETVOID | HALT | BR | CONDBR ) */
  lazy val INSN: Parser[Context => Any] = register ~ (assign ~> (
    IIMM | FIMM | BINARY_ARITH | CMP | ALLOCG | ALLOCL | LOAD | STORE | GETELEMENTPTR | SIZEOF | GETFUNPTR | LOADARG | CALL | CALLPTR | PUTCHAR | PUTNUM | GETCHAR | PHI | CAST | RET | RETVOID | HALT | BR | CONDBR
    )) ^^ { case name ~ insnGen =>
    (ctx: Context) => {
      val insn = insnGen(ctx).name(name.name)
      assert(insn.name == name.name)
    }
  } described "instruction"

  /** IIMM := 'iimm' integer */
  lazy val IIMM: Parser[Context => IImmInsn] = IImm ~> integer ^^ { value => _.emitIImm(value) }

  /** FIMM := 'fimm' ( integer | double ) */
  lazy val FIMM: Parser[Context => FImmInsn] = FImm ~> (integer ^^ (_.toDouble) | double) ^^ { value => _.emitFImm(value) }

  /** BINARY_ARITH := ( 'iadd' | 'isub' | 'iand' | 'ior' | 'ixor' | 'ishl' | 'ishr' | 'umul' | 'smul' | 'udiv' | 'sdiv' | 'fadd' | 'fsub' | 'fmul' | 'fdiv' ) insnRef ',' insnRef */
  lazy val BINARY_ARITH: Parser[Context => BinaryArithInsn] =
    (IAdd | ISub | IAnd | IOr | IXor | IShl | IShr | UMul | SMul | UDiv | SDiv | FAdd | FSub | FMul | FDiv) ~ insnRef ~ (comma ~> insnRef) ^^ { case op ~ leftFuture ~ rightFuture =>
      (ctx: Context) => {
        val insn = ctx.emit(new BinaryArithInsn(op, None, None, ctx.bb))
        leftFuture(ctx)(insn.leftRef.apply)
        rightFuture(ctx)(insn.rightRef.apply)
        insn
      }
    }

  /** CMP := ( 'cmpieq' | 'cmpine' | 'cmpult' | 'cmpule' | 'cmpugt' | 'cmpuge' | 'cmpslt' | 'cmpsle' | 'cmpsgt' | 'cmpsge' | 'cmpfeq' | 'cmpfne' | 'cmpflt' | 'cmpfle' | 'cmpfgt' | 'cmpfge' ) insnRef ',' insnRef */
  lazy val CMP: Parser[Context => CmpInsn] =
    (CmpIEq | CmpINe | CmpULt | CmpULe | CmpUGt | CmpUGe | CmpSLt | CmpSLe | CmpSGt | CmpSGe | CmpFEq | CmpFNe | CmpFLt | CmpFLe | CmpFGt | CmpFGe) ~ insnRef ~ (comma ~> insnRef) ^^ { case op ~ leftFuture ~ rightFuture =>
      (ctx: Context) => {
        val insn = ctx.emit(new CmpInsn(op, None, None, ctx.bb))
        leftFuture(ctx)(insn.leftRef.apply)
        rightFuture(ctx)(insn.rightRef.apply)
        insn
      }
    }

  /** ALLOCL := 'allocl' VAR_TYPE */
  lazy val ALLOCL: Parser[Context => AllocLInsn] = AllocL ~> VAR_TYPE ^^ { varTy => (ctx: Context) => ctx.emit(new AllocLInsn(varTy, ctx.bb)) }

  /** ALLOCG := 'allocg' VAR_TYPE [ ',' integer { integer } ] */
  lazy val ALLOCG: Parser[Context => AllocGInsn] = AllocG ~> VAR_TYPE ~ opt(comma ~> rep1(integer)) ^^ { case varTy ~ initData =>
    (ctx: Context) => ctx.emit(new AllocGInsn(varTy, initData.getOrElse(Seq.empty), ctx.bb))
  }

  /** STORE := 'store' SCALAR_TYPE insnRef */
  lazy val LOAD: Parser[Context => LoadInsn] = (Load ~> SCALAR_TYPE) ~ insnRef ^^ { case valueTy ~ ptrFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new LoadInsn(valueTy, None, ctx.bb))
      ptrFuture(ctx)(insn.ptrRef.apply)
      insn
    }
  }

  /** STORE := 'loadarg' integer */
  lazy val LOADARG: Parser[Context => LoadArgInsn] = LoadArg ~> integer ^^ { index => (ctx: Context) => ctx.emit(new LoadArgInsn(index.toInt, ctx.bb)) }

  /** STORE := 'store' insnRef ',' insnRef */
  lazy val STORE: Parser[Context => StoreInsn] = (Store ~> insnRef) ~ (comma ~> insnRef) ^^ { case ptrFuture ~ valueFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new StoreInsn(None, None, ctx.bb))
      ptrFuture(ctx)(insn.ptrRef.apply)
      valueFuture(ctx)(insn.valueRef.apply)
      insn
    }
  }

  /** GETELEMENTPTR := 'getelementptr' VAR_TYPE ',' 'ptr' insnRef ',' '[' insnRef ']' '.' integer */
  lazy val GETELEMENTPTR: Parser[Context => GetElementPtrInsn] =
    (GetElementPtr ~> VAR_TYPE) ~ (comma ~> kwPtr ~> insnRef) ~ (comma ~> squareOpen ~> insnRef) ~ (squareClose ~> dot ~> integer) ^^ { case elemTy ~ ptrFuture ~ indexFuture ~ fieldIndex =>
      (ctx: Context) => {
        val insn = ctx.emit(new GetElementPtrInsn(None, None, elemTy, fieldIndex.toInt, ctx.bb))
        ptrFuture(ctx)(insn.ptrRef.apply)
        indexFuture(ctx)(insn.indexRef.apply)
        insn
      }
    }

  /** SIZEOF := 'sizeof' VAR_TYPE */
  lazy val SIZEOF: Parser[Context => SizeOfInsn] = SizeOf ~> VAR_TYPE ^^ { varTy =>
    (ctx: Context) => ctx.emit(new SizeOfInsn(varTy, ctx.bb))
  }

  /** GETFUNPTR := 'getfunptr' funRef */
  lazy val GETFUNPTR: Parser[Context => GetFunPtrInsn] = (GetFunPtr ~> funRef) ^^ { targetFunFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new GetFunPtrInsn(None, ctx.bb))
      targetFunFuture(ctx)(insn.targetFunRef.apply)
      insn
    }
  }

  /** CALL := 'call' RET_TYPE funRef '(' CALL_ARGS ')' */
  lazy val CALL: Parser[Context => CallInsn] = (Call ~> RET_TYPE) ~ funRef ~ (parOpen ~> CALL_ARGS) <~ parClose ^^ { case returnTy ~ targetFunFuture ~ args =>
    (ctx: Context) => {
      val insn = ctx.emit(new CallInsn(None, args.map(_ => None), ctx.bb))
      targetFunFuture(ctx)(insn.targetFunRef.apply)
      args.zip(insn.argRefs).foreach({ case ((argTy, argFuture), argRef) => argFuture(ctx)(argRef.apply) })
      insn
    }
  }

  /** CALLPTR := 'callptr' RET_TYPE insnRef '(' CALL_ARGS ')' */
  lazy val CALLPTR: Parser[Context => CallPtrInsn] = (CallPtr ~> RET_TYPE) ~ insnRef ~ (parOpen ~> CALL_ARGS) <~ parClose ^^ { case returnTy ~ funPtrFuture ~ args =>
    (ctx: Context) => {
      val insn = ctx.emit(new CallPtrInsn(buildIrFunSignature(returnTy, args), None, args.map(_ => None), ctx.bb))
      funPtrFuture(ctx)(insn.funPtrRef.apply)
      args.zip(insn.argRefs).foreach({ case ((argTy, argFuture), argRef) => argFuture(ctx)(argRef.apply) })
      insn
    }
  }

  private def buildIrFunSignature(returnTy: IrTy, args: IndexedSeq[(IrTy, _)]): IrFunSignature =
    IrFunSignature(returnTy, args.map(_._1))

  /** CALL_ARGS := [ ARG_TYPE insnRef { ',' ARG_TYPE insnRef } ] */
  lazy val CALL_ARGS: Parser[IndexedSeq[(IrTy, Context => RefFuture[Insn])]] = repsep(ARG_TYPE ~ insnRef, comma) ^^ { list =>
    list.map({ case argTy ~ argFuture => (argTy, argFuture) }).toIndexedSeq
  }

  /** PUTCHAR := 'putchar' insnRef */
  lazy val PUTCHAR: Parser[Context => PutCharInsn] = (PutChar ~> insnRef) ^^ { argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new PutCharInsn(None, ctx.bb))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  /** PUTNUM := 'putnum' insnRef */
  lazy val PUTNUM: Parser[Context => PutNumInsn] = (PutNum ~> insnRef) ^^ { argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new PutNumInsn(None, ctx.bb))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  /** GETCHAR := 'getchar' */
  lazy val GETCHAR: Parser[Context => GetCharInsn] = GetChar ^^ { _ => (ctx: Context) => ctx.emit(new GetCharInsn(ctx.bb)) }

  /** PHI := 'phi' '[' insnRef ',' basicBlockRef ']' { ',' '[' insnRef ',' basicBlockRef ']' } */
  lazy val PHI: Parser[Context => PhiInsn] = (Phi ~> rep1sep((squareOpen ~> insnRef) ~ (comma ~> basicBlockRef) <~ squareClose, comma)) ^^ { case args =>
    (ctx: Context) => {
      val insn = ctx.emit(new PhiInsn(args.map(_ => (None, None)).toIndexedSeq, ctx.bb))
      args.zip(insn.argRefs).foreach({ case (insnFuture ~ bbFuture, argRef) =>
        insnFuture(ctx)(argRef._1.apply)
        bbFuture(ctx)(argRef._2.apply)
      })
      insn
    }
  }

  /** RET := 'ret' insnRef */
  lazy val RET: Parser[Context => RetInsn] = (Ret ~> insnRef) ^^ { argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new RetInsn(None, ctx.bb))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  /** RETVOID := 'retvoid' */
  lazy val RETVOID: Parser[Context => RetVoidInsn] = RetVoid ^^ { _ => (ctx: Context) => ctx.emit(new RetVoidInsn(ctx.bb)) }

  /** HALT := 'halt' */
  lazy val HALT: Parser[Context => HaltInsn] = Halt ^^ { _ => (ctx: Context) => ctx.emit(new HaltInsn(ctx.bb)) }

  /** BR := 'br' basicBlockRef */
  lazy val BR: Parser[Context => BrInsn] = (Br ~> basicBlockRef) ^^ { succBlockFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new BrInsn(None, ctx.bb))
      succBlockFuture(ctx)(insn.succBlockRef.apply)
      insn
    }
  }

  /** CONDBR := 'condbr' insnRef basicBlockRef basicBlockRef */
  lazy val CONDBR: Parser[Context => CondBrInsn] = (CondBr ~> insnRef) ~ (comma ~> basicBlockRef) ~ (comma ~> basicBlockRef) ^^ { case argFuture ~ trueBlockFuture ~ falseBlockFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new CondBrInsn(None, None, None, ctx.bb))
      argFuture(ctx)(insn.argRef.apply)
      trueBlockFuture(ctx)(insn.trueBlockRef.apply)
      falseBlockFuture(ctx)(insn.falseBlockRef.apply)
      insn
    }
  }

  /** CAST := ( 'bitcastint64todouble' | 'sint64todouble' | 'bitcastdoubletoint64' | 'doubletosint64' ) insnRef */
  lazy val CAST: Parser[Context => CastInsn] = (BitcastInt64ToDouble | SInt64ToDouble | BitcastDoubleToInt64 | DoubleToSInt64) ~ insnRef ^^ { case op ~ argFuture =>
    (ctx: Context) => {
      val insn = ctx.emit(new CastInsn(op, None, ctx.bb))
      argFuture(ctx)(insn.argRef.apply)
      insn
    }
  }

  // Types

  /** ARG_TYPE := SCALAR_TYPE */
  lazy val ARG_TYPE: Parser[IrTy] = SCALAR_TYPE

  /** RET_TYPE := SCALAR_TYPE | 'void' */
  lazy val RET_TYPE: Parser[IrTy] = SCALAR_TYPE | (kwVoid ^^ { _ => VoidTy }) described "return type"

  /** VAR_TYPE := ( SCALAR_TYPE | STRUCT_TYPE ) { '[' integer ']' } */
  lazy val VAR_TYPE: Parser[IrTy] = (SCALAR_TYPE | STRUCT_TYPE) ~ rep((squareOpen ~> integer) <~ squareClose ^^ { case numElem =>
    (baseTy: IrTy) => ArrayTy(baseTy, numElem.toInt)
  }) ^^ { case baseTy ~ postfix => postfix.foldLeft(baseTy)((baseTy, f) => f(baseTy)) }

  /**
   * SCALAR_TYPE := 'i64' | 'double' | 'ptr'
   *
   * Pointer is a scalar type in IR (alias to Int64).
   */
  lazy val SCALAR_TYPE: Parser[IrTy] = (
    (kwI64 ^^ { _ => Int64Ty }) | (kwDouble ^^ { _ => DoubleTy }) | (kwPtr ^^ { _ => PtrTy })
    ) described "scalar type"

  /** STRUCT_TYPE := 'struct' '{' VAR_TYPE { ',' VAR_TYPE } '}' */
  lazy val STRUCT_TYPE: Parser[IrTy] = (kwStruct ~> curlyOpen ~> rep1sep(VAR_TYPE, comma)) <~ curlyClose ^^ (fieldTys => StructTy(fieldTys.toIndexedSeq)) described "struct"

  override def remainderToString(in: Input): String = in.headOption.map({
    case Special(value) => s"'${value.name}'"
    case Identifier(value) => s"identifier '${value.name}'"
    case IntLiteral(value) => s"integer literal $value"
    case DoubleLiteral(value) => s"double literal $value"
    case Register(value) => s"register %${value.name}"
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