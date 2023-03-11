package tinycc.common.ir.parser

import tinycc.common.ir.IrOpcode
import tinycc.common.ir.parser.Lexer.Token
import tinycc.util.parsing.ParserException
import tinycc.util.parsing.combinator._

object IrParser extends Parsers {

  import IrOpcode._
  import tinycc.common.ir.parser.Lexer.Token._
  import tinycc.common.ir.parser.Symbols._

  type Elem = Token
  type Input = Reader[Elem]

  def elem[R](message: String, fn: PartialFunction[Elem, R]): Parser[R] = (in: Input) => in.headOption match {
    case Some(tok) => fn.andThen(Accept(_, in.tail)).applyOrElse(tok, (_: Elem) => Reject(message, in))
    case None => Reject(message, in)
  }

  implicit def symbol2parser(symbol: Symbol): Parser[Symbol] = elem(s"'${symbol.name}'", { case Special(value) if value == symbol => value })

  implicit def opcode2parser[T <: IrOpcode](op: T): Parser[T] = elem(s"'${op}'", { case Identifier(value) if value.name == op.toString => op })

  lazy val identifier: Parser[Symbol] = elem("identifier", { case Identifier(value) => value })
  lazy val register: Parser[Symbol] = elem("register", { case Register(Some(value)) => value })
  lazy val integer: Parser[Long] = elem("integer literal", { case IntLiteral(value) => value })
  lazy val double: Parser[Double] = elem("double literal", { case DoubleLiteral(value) => value })

  lazy val PROGRAM: Parser[Any] = rep(FUN_DECL)

  lazy val FUN_DECL: Parser[Any] = kwFn ~ RET_TYPE ~ parOpen ~ repsep(ARG_TYPE, comma) ~ parClose ~ curlyOpen ~ FUN_BODY ~ curlyClose described "function declaration"

  lazy val FUN_BODY: Parser[Any] = rep1(BASIC_BLOCK) described "function body"

  lazy val BASIC_BLOCK: Parser[Any] = identifier ~ colon ~ rep(INSN) described "basic block"

  lazy val INSN: Parser[Any] = register ~ assign ~ (
    IIMM | FIMM | BINARY_ARITH | CMP | ALLOCG | ALLOCL | LOAD | STORE | GETELEMENTPTR | GETFUNPTR | LOADARG | CALL | CALLPTR | PUTCHAR | PUTNUM | GETCHAR | CAST | RET | RETVOID | HALT | BR | CONDBR
    ) described "instruction"

  lazy val IIMM: Parser[Any] = IImm ~ integer

  lazy val FIMM: Parser[Any] = FImm ~ (integer | double)

  lazy val BINARY_ARITH: Parser[Any] = (IAdd | ISub | IAnd | IOr | IXor | IShl | IShr | UMul | SMul | SDiv | FAdd | FSub | FMul | FDiv) ~ register ~ comma ~ register

  lazy val CMP: Parser[Any] = (CmpIEq | CmpINe | CmpULt | CmpULe | CmpUGt | CmpUGe | CmpSLt | CmpSLe | CmpSGe | CmpFEq | CmpFNe | CmpFLt | CmpFGt | CmpFGe) ~ register ~ comma ~ register

  lazy val ALLOCL: Parser[Any] = AllocL ~ VAR_TYPE

  lazy val ALLOCG: Parser[Any] = AllocG ~ VAR_TYPE

  lazy val LOAD: Parser[Any] = Load ~ SCALAR_TYPE ~ register

  lazy val LOADARG: Parser[Any] = LoadArg ~ integer

  lazy val STORE: Parser[Any] = Store ~ register ~ comma ~ register

  lazy val GETELEMENTPTR: Parser[Any] = GetElementPtr // TODO

  lazy val GETFUNPTR: Parser[Any] = GetFunPtr ~ identifier

  lazy val CALL: Parser[Any] = Call ~ RET_TYPE ~ identifier ~ parOpen ~ CALL_ARGS ~ parClose

  lazy val CALLPTR: Parser[Any] = CallPtr ~ RET_TYPE ~ register ~ parOpen ~ CALL_ARGS ~ parClose

  lazy val CALL_ARGS: Parser[Any] = repsep(ARG_TYPE ~ register, comma)

  lazy val PUTCHAR: Parser[Any] = PutChar ~ register

  lazy val PUTNUM: Parser[Any] = PutNum ~ register

  lazy val GETCHAR: Parser[Any] = GetChar

  lazy val PHI: Parser[Any] = Phi // TODO

  lazy val RET: Parser[Any] = Ret ~ register

  lazy val RETVOID: Parser[Any] = RetVoid

  lazy val HALT: Parser[Any] = Halt

  lazy val BR: Parser[Any] = Br ~ kwLabel ~ register

  lazy val CONDBR: Parser[Any] = CondBr ~ register ~ kwLabel ~ register ~ comma ~ kwLabel ~ register

  lazy val CAST: Parser[Any] = (BitcastInt64ToDouble | SInt64ToDouble | BitcastDoubleToInt64 | DoubleToSInt64) ~ register

  // Types

  lazy val ARG_TYPE: Parser[Any] = SCALAR_TYPE

  lazy val RET_TYPE: Parser[Any] = SCALAR_TYPE | kwVoid described "return type"


  lazy val VAR_TYPE: Parser[Any] = SCALAR_TYPE | STRUCT_TYPE

  lazy val SCALAR_TYPE: Parser[Any] = kwI64 | kwDouble | kwPtr described "scalar type"

  lazy val STRUCT_TYPE: Parser[Any] = kwStruct ~ curlyOpen ~ rep1sep(VAR_TYPE, comma) ~ curlyClose described "struct"

  override def remainderToString(in: Input): String = in.headOption.map({
    case Special(value) => s"'${value.name}'"
    case Identifier(value) => s"identifier '${value.name}'"
    case IntLiteral(value) => s"integer literal $value"
    case DoubleLiteral(value) => s"double literal $value"
    case Register(value) => s"register %${value.map(_.name).getOrElse("<null>")}"
  }).getOrElse("end of input")

  def parseProgram(in: Reader[Elem]): Any =
    parse(PROGRAM <~ EOI, in) match {
      case Accept(value, _, _) => value
      case Reject(expectation, remainder, _) =>
        throw new ParserException(formatErrorMessage(expectation, remainder), remainder.loc)
    }

  def parseProgram(s: String): Any = parseProgram(Lexer.Scanner(CharReader(s)))
}