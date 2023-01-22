package tinycc.frontend

import tinycc.frontend.ast._
import tinycc.util.parsing.combinator.Parsers
import tinycc.frontend.Symbols._

import scala.language.implicitConversions

class Parser extends Parsers {

  implicit def symbolToken(symbol: Symbol): Parser[Symbol] = ???

  lazy val loc: Parser[SourceLocation] = ???

  lazy val identifier: Parser[Identifier] = ???
  lazy val integer: Parser[Int] = ???
  lazy val double: Parser[Double] = ???
  lazy val char: Parser[Char] = ???
  lazy val string: Parser[String] = ???

  /** PROGRAM := { FUN_DECL | VAR_DECLS ';' | STRUCT_DECL | FUNPTR_DECL } */
  lazy val PROGRAM: Parser[AstBlock] = loc ~ rep[AstNode](STRUCT_DECL | FUNPTR_DECL | FUN_DECL | (VAR_DECLS <~ semicolon)) ^^ { case loc ~ decls => new AstBlock(decls, loc) }

  /** FUN_DECL := TYPE_FUN_RET identifier '(' [ FUN_ARG { ',' FUN_ARG } ] ')' [ BLOCK_STMT ] */
  lazy val FUN_DECL: Parser[AstFunDecl] = TYPE_FUN_RET ~ identifier <~ parOpen ~ rep1sep(FUN_ARG, comma) <~ parClose ~> opt(BLOCK_STMT)

  /** FUN_ARG := TYPE identifier */
  lazy val FUN_ARG: Parser[Any] = TYPE ~ identifier

  /** STATEMENT := BLOCK_STMT | IF_STMT | SWITCH_STMT | WHILE_STMT | DO_WHILE_STMT | FOR_STMT | BREAK_STMT | CONTINUE_STMT | RETURN_STMT | EXPR_STMT */
  lazy val STATEMENT: Parser[AstNode] = BLOCK_STMT | IF_STMT | SWITCH_STMT | WHILE_STMT | DO_WHILE_STMT | FOR_STMT | BREAK_STMT | CONTINUE_STMT | RETURN_STMT | EXPR_STMT

  /** BLOCK_STMT := '{' { STATEMENT } '}' */
  lazy val BLOCK_STMT: Parser[AstBlock] = curlyOpen ~> opt(STATEMENT) <~ curlyClose

  /** IF_STMT := if '(' EXPR ')' STATEMENT [ else STATEMENT ] */
  lazy val IF_STMT: Parser[AstBlock] = kwIf ~ parOpen ~> EXPR <~ parClose ~ STATEMENT ~ opt(kwElse ~> STATEMENT)

  /** SWITCH_STMT := switch '(' EXPR ')' '{' { CASE_STMT } [ default ':' CASE_BODY } ] { CASE_STMT } '}' */
  lazy val SWITCH_STMT: Parser[AstSwitch] = kwSwitch ~ parOpen ~ EXPR ~ parClose ~ curlyOpen ~ rep(CASE_STMT) ~ opt(kwDefault ~ colon ~ CASE_BODY) ~ rep(CASE_STMT) ~ curlyClose

  /** CASE_STMT := case integer_literal ':' CASE_BODY */
  lazy val CASE_STMT: Parser[Any] = kwCase ~ integer ~ colon ~ CASE_BODY

  /** CASE_BODY := { STATEMENT } */
  lazy val CASE_BODY: Parser[Any] = rep(STATEMENT)

  /** WHILE_STMT := while '(' EXPR ')' STATEMENT */
  lazy val WHILE_STMT: Parser[AstWhile] = kwWhile ~ parOpen ~ EXPR ~ parClose ~ STATEMENT

  /** DO_WHILE_STMT := do STATEMENT while '(' EXPR ')' ';' */
  lazy val DO_WHILE_STMT: Parser[AstDoWhile] = kwDo ~ STATEMENT ~ kwWhile ~ parOpen ~ EXPR ~ parClose ~ semicolon

  /** FOR_STMT := for '(' [ EXPRS_OR_VAR_DECLS ] ';' [ EXPR ] ';' [ EXPR ] ')' STATEMENT */
  lazy val FOR_STMT: Parser[AstFor] = kwFor ~ parOpen ~ opt(EXPRS_OR_VAR_DECLS) ~ semicolon ~ opt(EXPR) ~ semicolon ~ opt(EXPR) ~ STATEMENT

  /** BREAK_STMT := break ';' */
  lazy val BREAK_STMT: Parser[AstBreak] = loc <~ kwBreak <~ semicolon ^^ { loc => new AstBreak(loc) }

  /** CONTINUE_STMT := continue ';' */
  lazy val CONTINUE_STMT: Parser[AstBreak] = loc <~ kwContinue <~ semicolon ^^ { loc => new AstBreak(loc) }

  /** RETURN_STMT := return [ EXPR ] ';' */
  lazy val RETURN_STMT: Parser[AstReturn] = loc <~ kwReturn ~ opt(EXPR) <~ semicolon ^^ { case loc ~ expr => new AstReturn(expr, loc)}

  /** EXPR_STMT := EXPRS_OR_VAR_DECLS ';' */
  lazy val EXPR_STMT: Parser[AstNode] = EXPRS_OR_VAR_DECLS <~ semicolon

  /** TYPE := (int | double | char | identifier) { * }
       |= void * { * } */
  lazy val TYPE: Parser[AstType] = ((kwInt | kwDouble | kwChar | identifier) ~ rep(mul)) | (kwVoid ~ mul ~ rep(mul))

  /** TYPE_FUN_RET := TYPE | void */
  lazy val TYPE_FUN_RET: Parser[AstType] = TYPE | kwVoid

  /** STRUCT_DECL := struct identifier [ '{' { TYPE identifier ';' } '}' ] ';' */
  lazy val STRUCT_DECL: Parser[AstStructDecl] = kwStruct ~ identifier ~ opt(curlyOpen ~ rep(TYPE ~ identifier ~ semicolon) ~ curlyClose) ~ semicolon

  /** FUNPTR_DECL := typedef TYPE_FUN_RET '(' '*' identifier ')' '(' [ TYPE { ',' TYPE } ] ')' ';' */
  lazy val FUNPTR_DECL: Parser[AstFunPtrDecl] = kwTypedef ~ TYPE_FUN_RET ~ parOpen ~ mul ~ identifier ~ parClose ~ parOpen ~ repsep(TYPE, comma) ~ parClose ~ semicolon

  /** F := integer | double | char | string | identifier | '(' EXPR ')' | E_CAST */
  lazy val F: Parser[AstNode] = integer | double | char | string | identifier | (parOpen ~ EXPR ~ parClose) | E_CAST

  /** E_CAST := cast '<' TYPE '>' '(' EXPR ')' */
  lazy val E_CAST: Parser[AstNode] = kwCast ~ lt ~ TYPE ~ gt ~ parOpen ~ EXPR ~ parClose

  /** E_CALL_INDEX_MEMBER_POST := F { E_CALL | E_INDEX | E_MEMBER | E_POST } */
  lazy val E_CALL_INDEX_MEMBER_POST: Parser[AstNode] = F ~ rep(E_CALL | E_INDEX | E_MEMBER | E_POST)

  /** E_CALL := '(' [ EXPR { ',' EXPR } ] ')' */
  lazy val E_CALL: Parser[AstNode => AstCall] = parOpen ~ repsep(EXPR, comma) ~ parClose

  /** E_INDEX := '[' EXPR ']' */
  lazy val E_INDEX: Parser[AstNode => AstNode] = squareOpen ~ EXPR ~ squareClose

  /** E_MEMBER := ('.' | '->') identifier */
  lazy val E_MEMBER: Parser[AstNode => AstNode] = (dot | arrowR) ~ identifier

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
  lazy val E5: Parser[AstNode] = E4 ~ rep(loc ~ (eq | nEq) ~ E4 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E6 := E5 { '&' E5 } */
  lazy val E6: Parser[AstNode] = E5 ~ rep(loc ~ bitAnd ~ E5 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E7 := E6 { '|' E6 } */
  lazy val E7: Parser[AstNode] = E6 ~ rep(loc ~ bitOr ~ E6 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E8 := E7 { '&&' E7 } */
  lazy val E8: Parser[AstNode] = E7 ~ rep(loc ~ and ~ E7 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** E9 := E8 { '||' E8 } */
  lazy val E9: Parser[AstNode] = E8 ~ rep(loc ~ or ~ E8 ^^ buildBinaryOp) ^^ applyPostModifiers

  /** EXPR := E9 [ '=' EXPR ] */
  lazy val EXPR: Parser[AstNode] = E9 ~ opt((loc <~ assign) ~ EXPR) ^^ {
    case lvalue ~ Some(loc ~ value) => new AstAssignment(lvalue, value, loc)
    case expr ~ None => expr
  }

  /** EXPRS := EXPR { ',' EXPR } */
  lazy val EXPRS: Parser[AstSequence] = loc ~ rep1sep(EXPR, comma) ^^ { case loc ~ exprs => new AstSequence(exprs, loc) }

  /** VAR_DECL := TYPE identifier [ '[' E9 ']' ] [ '=' EXPR ] */
  lazy val VAR_DECL: Parser[AstVarDecl] = TYPE ~ identifier ~ opt(squareOpen ~> E9 <~ squareClose) ~ opt(assign ~> EXPR)

  /** VAR_DECLS := VAR_DECL { ',' VAR_DECL } */
  lazy val VAR_DECLS: Parser[AstSequence] = loc ~ rep1sep(VAR_DECL, comma) ^^ { case loc ~ decls => new AstSequence(decls, loc) }

  /** EXPRS_OR_VAR_DECLS := VAR_DECLS | EXPRS */
  lazy val EXPRS_OR_VAR_DECLS: Parser[AstSequence] = VAR_DECLS | EXPRS

  private def buildUnaryOp(a: SourceLocation ~ Symbol): AstNode => AstUnaryOp = a match {
    case loc ~ op => expr: AstNode => new AstUnaryOp(op, expr, loc)
  }

  private def buildBinaryOp(a: SourceLocation ~ Symbol ~ AstNode): AstNode => AstBinaryOp = a match {
    case loc ~ op ~ right => left: AstNode => new AstBinaryOp(op, left, right, loc)
  }

  private def applyPreModifiers(a: List[AstNode => AstNode] ~ AstNode): AstNode = a match {
    case mods ~ expr => mods.foldRight(expr)((fn, expr) => fn(expr))
  }

  private def applyPostModifiers(a: AstNode ~ List[AstNode => AstNode]): AstNode = a match {
    case expr ~ mods => mods.foldLeft(expr)((expr, fn) => fn(expr))
  }
}
