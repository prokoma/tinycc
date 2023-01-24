package tinycc.frontend

import tinycc.frontend.Symbols._
import tinycc.frontend.ast._
import tinycc.util.parsing.SourceLocation
import tinycc.util.parsing.combinator.{CharReader, Parsers, Reader}

import scala.language.implicitConversions

object TinyCParser extends Parsers {
  type Elem = Token
  type Input = ScannerAdapter

  def unexpectedEndOfInput(in: Input): Reject = Reject("unexpected end of input", in)

  lazy val loc: Parser[SourceLocation] = in => Accept(in.loc, in)

  def elem[R](fn: PartialFunction[Elem, R], msgFn: Elem => String): Parser[R] =
    in => in.headOption match {
      case None => unexpectedEndOfInput(in)
      case Some(tok) => fn.andThen(Accept(_, in.tail)).applyOrElse(tok, (tok: Elem) => Reject(msgFn(tok), in))
    }

  def tokenToString(tok: Token): String = tok.toString

  implicit def symbolParser(symbol: Symbol): Parser[Symbol] = elem({ case t: Special if t.value == symbol => t.value }, tok => s"expected $symbol, got ${tokenToString(tok)}")

  lazy val identifier: Parser[Symbol] = elem({ case Identifier(value) => value }, tok => s"expected identifier, got ${tokenToString(tok)}")
  lazy val integer: Parser[Long] = elem({ case IntLiteral(value) => value }, tok => s"expected int literal, got ${tokenToString(tok)}")
  lazy val double: Parser[Double] = elem({ case DoubleLiteral(value) => value }, tok => s"expected double literal, got ${tokenToString(tok)}")
  lazy val char: Parser[Char] = elem({ case StringLiteral(value, '\'') if value.length == 1 => value(0) }, tok => s"expected char literal, got ${tokenToString(tok)}")
  lazy val string: Parser[String] = elem({ case StringLiteral(value, '"') => value }, tok => s"expected string literal, got ${tokenToString(tok)}")

  /** PROGRAM := { FUN_DECL | VAR_DECLS ';' | STRUCT_DECL | FUNPTR_DECL } */
  lazy val PROGRAM: Parser[AstBlock] = loc ~ rep[AstNode](FUN_DECL | (VAR_DECLS <~ semicolon) | STRUCT_DECL | FUNPTR_DECL) ^^ {
    case loc ~ decls => new AstBlock(decls, loc)
  }

  /** FUN_DECL := TYPE_FUN_RET identifier '(' [ FUN_ARG { ',' FUN_ARG } ] ')' [ BLOCK_STMT ] */
  lazy val FUN_DECL: Parser[AstFunDecl] = loc ~ TYPE_FUN_RET ~ identifier ~ (parOpen ~> repsep(FUN_ARG, comma)) ~ (parClose ~> opt(BLOCK_STMT)) ^^ {
    case loc ~ returnTy ~ name ~ args ~ body => new AstFunDecl(name, returnTy, args, body, loc)
  }

  /** FUN_ARG := TYPE identifier */
  lazy val FUN_ARG: Parser[(AstType, Symbol)] = TYPE ~ identifier ^^ { case argTy ~ name => (argTy, name) }

  /** STATEMENT := BLOCK_STMT | IF_STMT | SWITCH_STMT | WHILE_STMT | DO_WHILE_STMT | FOR_STMT | BREAK_STMT | CONTINUE_STMT | RETURN_STMT | EXPR_STMT */
  lazy val STATEMENT: Parser[AstNode] = BLOCK_STMT | IF_STMT | SWITCH_STMT | WHILE_STMT | DO_WHILE_STMT | FOR_STMT | BREAK_STMT | CONTINUE_STMT | RETURN_STMT | EXPR_STMT

  /** BLOCK_STMT := '{' { STATEMENT } '}' */
  lazy val BLOCK_STMT: Parser[AstBlock] = loc ~ (curlyOpen ~> rep(STATEMENT)) <~ curlyClose ^^ {
    case loc ~ body => new AstBlock(body, loc)
  }

  /** IF_STMT := if '(' EXPR ')' STATEMENT [ else STATEMENT ] */
  lazy val IF_STMT: Parser[AstIf] = loc ~ (kwIf ~> parOpen ~> EXPR) ~ (parClose ~> STATEMENT) ~ opt(kwElse ~> STATEMENT) ^^ {
    case loc ~ guard ~ trueCase ~ falseCase => new AstIf(guard, trueCase, falseCase, loc)
  }

  /** SWITCH_STMT := switch '(' EXPR ')' '{' { CASE_STMT } [ default ':' CASE_BODY } ] '}' */
  lazy val SWITCH_STMT: Parser[AstSwitch] = loc ~ (kwSwitch ~> parOpen ~> EXPR) ~ (parClose ~> curlyOpen ~> rep(CASE_STMT)) ~ opt(kwDefault ~> colon ~> CASE_BODY) <~ curlyClose ^^ {
    case loc ~ guard ~ cases ~ defaultCase => new AstSwitch(guard, cases, defaultCase, loc)
  }

  /** CASE_STMT := case integer_literal ':' CASE_BODY */
  lazy val CASE_STMT: Parser[(Long, AstNode)] = (kwCase ~> integer) ~ (colon ~> CASE_BODY) ^^ {
    case value ~ body => (value, body)
  }

  /** CASE_BODY := { STATEMENT } */
  lazy val CASE_BODY: Parser[AstBlock] = loc ~ rep(STATEMENT) ^^ { case loc ~ body => new AstBlock(body, loc) }

  /** WHILE_STMT := while '(' EXPR ')' STATEMENT */
  lazy val WHILE_STMT: Parser[AstWhile] = loc ~ (kwWhile ~> parOpen ~> EXPR) ~ (parClose ~> STATEMENT) ^^ {
    case loc ~ guard ~ body => new AstWhile(guard, body, loc)
  }

  /** DO_WHILE_STMT := do STATEMENT while '(' EXPR ')' ';' */
  lazy val DO_WHILE_STMT: Parser[AstDoWhile] = loc ~ (kwDo ~> STATEMENT) ~ (kwWhile ~> parOpen ~> EXPR) <~ (parClose ~ semicolon) ^^ {
    case loc ~ body ~ guard => new AstDoWhile(body, guard, loc)
  }

  /** FOR_STMT := for '(' [ EXPRS_OR_VAR_DECLS ] ';' [ EXPR ] ';' [ EXPR ] ')' STATEMENT */
  lazy val FOR_STMT: Parser[AstFor] = loc ~ (kwFor ~> parOpen ~> opt(EXPRS_OR_VAR_DECLS)) ~ (semicolon ~> opt(EXPR)) ~ (semicolon ~> opt(EXPR)) ~ STATEMENT ^^ {
    case loc ~ init ~ guard ~ increment ~ body => new AstFor(init, guard, increment, body, loc)
  }

  /** BREAK_STMT := break ';' */
  lazy val BREAK_STMT: Parser[AstBreak] = loc <~ (kwBreak ~ semicolon) ^^ { loc => new AstBreak(loc) }

  /** CONTINUE_STMT := continue ';' */
  lazy val CONTINUE_STMT: Parser[AstBreak] = loc <~ (kwContinue ~ semicolon) ^^ { loc => new AstBreak(loc) }

  /** RETURN_STMT := return [ EXPR ] ';' */
  lazy val RETURN_STMT: Parser[AstReturn] = loc ~ (kwReturn ~> opt(EXPR)) <~ semicolon ^^ { case loc ~ expr => new AstReturn(expr, loc) }

  /** EXPR_STMT := EXPRS_OR_VAR_DECLS ';' */
  lazy val EXPR_STMT: Parser[AstNode] = EXPRS_OR_VAR_DECLS <~ semicolon

  /** TYPE := (int | double | char | identifier) { * }
   * |= void * { * } */
  lazy val TYPE: Parser[AstType] = (
    (loc ~ (kwInt | kwDouble | kwChar | isNamedType(identifier)) ^^ { case loc ~ name => new AstNamedType(name, loc) })
      | (loc ~ kwVoid ~ loc <~ mul ^^ { case loc1 ~ name ~ loc2 => new AstPointerType(new AstNamedType(name, loc1), loc2) })
    ) ~ rep(loc <~ mul ^^ buildPointerType) ^^ applyPostModifiers

  /** TYPE_FUN_RET := TYPE | void */
  lazy val TYPE_FUN_RET: Parser[AstType] = TYPE | (loc ~ kwVoid ^^ { case loc ~ name => new AstNamedType(name, loc) })

  /** STRUCT_DECL := struct identifier [ '{' { TYPE identifier ';' } '}' ] ';' */
  lazy val STRUCT_DECL: Parser[AstStructDecl] =
    loc ~ (kwStruct ~> declareNamedType(identifier)) ~ opt((curlyOpen ~> rep(TYPE ~ identifier <~ semicolon ^^ { case fieldTy ~ name => (fieldTy, name) })) <~ curlyClose) <~ semicolon ^^ {
      case loc ~ name ~ fields => new AstStructDecl(name, fields.getOrElse(Nil), loc)
    }

  /** FUNPTR_DECL := typedef TYPE_FUN_RET '(' '*' identifier ')' '(' [ TYPE { ',' TYPE } ] ')' ';' */
  lazy val FUNPTR_DECL: Parser[AstFunPtrDecl] =
    loc ~ (kwTypedef ~> TYPE_FUN_RET) ~ (parOpen ~> mul ~> declareNamedType(identifier)) ~ (parClose ~> parOpen ~> repsep(TYPE, comma)) <~ (parClose ~ semicolon) ^^ {
      case loc ~ returnTy ~ name ~ argTys => new AstFunPtrDecl(name, returnTy, argTys, loc)
    }

  /** F := integer | double | char | string | identifier | '(' EXPR ')' | E_CAST | scan '(' ')' | print '(' EXPR ')' */
  lazy val F: Parser[AstNode] = (
    (loc ~ integer ^^ { case loc ~ value => new AstInteger(value, loc) })
      | (loc ~ double ^^ { case loc ~ value => new AstDouble(value, loc) })
      | (loc ~ char ^^ { case loc ~ value => new AstChar(value, loc) })
      | (loc ~ string ^^ { case loc ~ value => new AstString(value, loc) })
      | (loc ~ identifier ^^ { case loc ~ value => new AstIdentifier(value, loc) })
      | (parOpen ~> EXPR <~ parClose)
      | E_CAST
      | (loc <~ (kwScan ~ parOpen ~ parClose) ^^ { case loc => new AstRead(loc) })
      | (loc ~ (kwPrint ~> parOpen ~> EXPR) <~ parClose ^^ { case loc ~ expr => new AstWrite(expr, loc) })
    )

  /** E_CAST := cast '<' TYPE '>' '(' EXPR ')' */
  lazy val E_CAST: Parser[AstNode] = loc ~ (kwCast ~> lt ~> TYPE) ~ (gt ~> parOpen ~> EXPR) <~ parClose ^^ { case loc ~ newTy ~ expr => new AstCast(expr, newTy, loc) }

  /** E_CALL_INDEX_MEMBER_POST := F { E_CALL | E_INDEX | E_MEMBER | E_POST } */
  lazy val E_CALL_INDEX_MEMBER_POST: Parser[AstNode] = F ~ rep(E_CALL | E_INDEX | E_MEMBER | E_POST) ^^ applyPostModifiers

  /** E_CALL := '(' [ EXPR { ',' EXPR } ] ')' */
  lazy val E_CALL: Parser[AstNode => AstCall] = loc ~ (parOpen ~> repsep(EXPR, comma)) <~ parClose ^^ { case loc ~ args => expr => new AstCall(expr, args, loc) }

  /** E_INDEX := '[' EXPR ']' */
  lazy val E_INDEX: Parser[AstNode => AstIndex] = loc ~ (squareOpen ~> EXPR) <~ squareClose ^^ { case loc ~ index => base => new AstIndex(base, index, loc) }

  /** E_MEMBER := ('.' | '->') identifier */
  lazy val E_MEMBER: Parser[AstNode => AstNode] = loc ~ (dot | arrowR) ~ identifier ^^ {
    case loc ~ op ~ identifier if op == dot => base => new AstMember(base, identifier, loc)
    case loc ~ op ~ identifier => base => new AstMemberPtr(base, identifier, loc)
  }

  /** E_POST := '++' | '--' */
  lazy val E_POST: Parser[AstNode => AstUnaryPostOp] = loc ~ (inc | dec) ^^ { case loc ~ op => expr => new AstUnaryPostOp(op, expr, loc) }

  /** E_UNARY_PRE := { '+' | '-' | '!' | '~' | '++' | '--' | '*' | '&' } E_CALL_INDEX_MEMBER_POST */
  lazy val E_UNARY_PRE: Parser[AstNode] = rep(loc ~ (add | sub | Symbols.not | neg | inc | dec | mul | bitAnd) ^^ buildUnaryOp) ~ E_CALL_INDEX_MEMBER_POST ^^ applyPreModifiers

  /** E1 := E_UNARY_PRE { ('*' | '/' | '%' ) E_UNARY_PRE } */
  lazy val E1: Parser[AstNode] = E_UNARY_PRE ~ rep(loc ~ (mul | div | mod) ~ E_UNARY_PRE ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E2 := E1 { ('+' | '-') E1 } */
  lazy val E2: Parser[AstNode] = E1 ~ rep(loc ~ (add | sub) ~ E1 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E3 := E2 { ('<<' | '>>') E2 } */
  lazy val E3: Parser[AstNode] = E2 ~ rep(loc ~ (shiftLeft | shiftRight) ~ E2 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E4 := E3 { ('<' | '<=' | '>' | '>=') E3 } */
  lazy val E4: Parser[AstNode] = E3 ~ rep(loc ~ (lt | lte | gt | gte) ~ E3 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E5 := E4 { ('==' | '!=') E4 } */
  lazy val E5: Parser[AstNode] = E4 ~ rep(loc ~ (Symbols.eq | nEq) ~ E4 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E6 := E5 { '&' E5 } */
  lazy val E6: Parser[AstNode] = E5 ~ rep(loc ~ bitAnd ~ E5 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E7 := E6 { '|' E6 } */
  lazy val E7: Parser[AstNode] = E6 ~ rep(loc ~ bitOr ~ E6 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E8 := E7 { '&&' E7 } */
  lazy val E8: Parser[AstNode] = E7 ~ rep(loc ~ and ~ E7 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E9 := E8 { '||' E8 } */
  lazy val E9: Parser[AstNode] = E8 ~ rep(loc ~ Symbols.or ~ E8 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** EXPR := E9 [ '=' EXPR ] */
  lazy val EXPR: Parser[AstNode] = E9 ~ opt((loc <~ assign) ~ EXPR) ^^ {
    case lvalue ~ Some(loc ~ value) => new AstAssignment(lvalue, value, loc)
    case expr ~ None => expr
  }

  /** EXPRS := EXPR { ',' EXPR } */
  lazy val EXPRS: Parser[AstSequence] = loc ~ rep1sep(EXPR, comma) ^^ { case loc ~ exprs => new AstSequence(exprs, loc) }

  /** VAR_DECL := TYPE identifier [ '[' E9 ']' ] [ '=' EXPR ] */
  lazy val VAR_DECL: Parser[AstVarDecl] = loc ~ TYPE ~ identifier ~ opt((squareOpen ~> E9) <~ squareClose) ~ opt(assign ~> EXPR) ^^ {
    case loc ~ varTy ~ name ~ arrayLen ~ value => new AstVarDecl(name, varTy, value, loc) // TODO: array length
  }

  /** VAR_DECLS := VAR_DECL { ',' VAR_DECL } */
  lazy val VAR_DECLS: Parser[AstSequence] = loc ~ rep1sep(VAR_DECL, comma) ^^ { case loc ~ decls => new AstSequence(decls, loc) }

  /** EXPRS_OR_VAR_DECLS := VAR_DECLS | EXPRS */
  lazy val EXPRS_OR_VAR_DECLS: Parser[AstSequence] = VAR_DECLS | EXPRS

  private def buildPointerType(loc: SourceLocation): AstType => AstPointerType = base => new AstPointerType(base, loc)

  private def buildUnaryOp(a: SourceLocation ~ Symbol): AstNode => AstUnaryOp = a match {
    case loc ~ op => expr: AstNode => new AstUnaryOp(op, expr, loc)
  }

  private def buildBinaryOp(a: SourceLocation ~ Symbol ~ AstNode): AstNode => AstBinaryOp = a match {
    case loc ~ op ~ right => left: AstNode => new AstBinaryOp(op, left, right, loc)
  }

  private def applyPreModifiers[T <: AstNode](a: List[T => T] ~ T): T = a match {
    case mods ~ expr => mods.foldRight(expr)((fn, expr) => fn(expr))
  }

  private def applyPostModifiers[T <: AstNode](a: T ~ List[T => T]): T = a match {
    case expr ~ mods => mods.foldLeft(expr)((expr, fn) => fn(expr))
  }

  private def declareNamedType(ty: Parser[Symbol]): Parser[Symbol] =
    in => ty(in) match {
      case Accept(value, reminding) => Accept(value, reminding.withDeclaredType(value))
      case r: Reject => r
    }

  private def isNamedType(ty: Parser[Symbol]): Parser[Symbol] =
    in => ty(in) match {
      case Accept(value, reminding) if in.declaredNamedTypes.contains(value) => Accept(value, reminding)
      case Accept(value, _) => Reject(s"expected named type, got $value", in)
      case r: Reject => r
    }

  case class ScannerAdapter(inner: Reader[Elem], declaredNamedTypes: Set[Symbol] = Set.empty) extends Reader[Elem] {
    override def headOption: Option[Elem] = inner.headOption

    override def tail: ScannerAdapter = ScannerAdapter(inner.tail, declaredNamedTypes)

    override def loc: SourceLocation = inner.loc

    def withDeclaredType(ty: Symbol): ScannerAdapter = ScannerAdapter(this, declaredNamedTypes + ty)
  }

  def parseProgram(in: Reader[Elem]): AstBlock =
    PROGRAM(ScannerAdapter(in)) match {
      case Accept(value, reminding) if reminding.isEmpty => value
      case Accept(_, reminding) => throw new RuntimeException(s"expected end of input at ${reminding.loc} ${reminding.headOption}")
      case Reject(message, reminding, _) => throw new RuntimeException(s"$message at ${reminding.loc}")
    }

  def parseProgram(s: String): AstBlock =
    parseProgram(Lexer.TokenReader(CharReader(s)))
}
