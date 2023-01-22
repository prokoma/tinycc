package tinycc.frontend

object Symbols {
  val inc: Symbol = Symbol("++")
  val dec: Symbol = Symbol("--")
  val add: Symbol = Symbol("+")
  val sub: Symbol = Symbol("-")
  val mul: Symbol = Symbol("*")
  val div: Symbol = Symbol("/")
  val mod: Symbol = Symbol("%")
  val shiftLeft: Symbol = Symbol("<<")
  val shiftRight: Symbol = Symbol(">>")
  val eq: Symbol = Symbol("==")
  val nEq: Symbol = Symbol("!=")
  val lt: Symbol = Symbol("<")
  val gt: Symbol = Symbol(">")
  val lte: Symbol = Symbol("<=")
  val gte: Symbol = Symbol(">=")
  val bitAnd: Symbol = Symbol("&")
  val bitOr: Symbol = Symbol("|")
  val and: Symbol = Symbol("&&")
  val or: Symbol = Symbol("||")
  val not: Symbol = Symbol("!")
  val neg: Symbol = Symbol("~")
  val xor: Symbol = Symbol("^")
  val dot: Symbol = Symbol(".")
  val semicolon: Symbol = Symbol(";")
  val colon: Symbol = Symbol(":")
  val arrowR: Symbol = Symbol("->")
  val comma: Symbol = Symbol(",")
  val parOpen: Symbol = Symbol("(")
  val parClose: Symbol = Symbol(")")
  val squareOpen: Symbol = Symbol("[")
  val squareClose: Symbol = Symbol("]")
  val curlyOpen: Symbol = Symbol("{")
  val curlyClose: Symbol = Symbol("}")
  val assign: Symbol = Symbol("=")
  val backtick: Symbol = Symbol("`")

  val kwBreak: Symbol = Symbol("break")
  val kwCase: Symbol = Symbol("case")
  val kwCast: Symbol = Symbol("cast")
  val kwChar: Symbol = Symbol("char")
  val kwContinue: Symbol = Symbol("continue")
  val kwDefault: Symbol = Symbol("default")
  val kwDefine: Symbol = Symbol("define")
  val kwDefmacro: Symbol = Symbol("defmacro")
  val kwDo: Symbol = Symbol("do")
  val kwDouble: Symbol = Symbol("double")
  val kwElse: Symbol = Symbol("else")
  val kwFor: Symbol = Symbol("for")
  val kwIf: Symbol = Symbol("if")
  val kwInt: Symbol = Symbol("int")
  val kwReturn: Symbol = Symbol("return")
  val kwStruct: Symbol = Symbol("struct")
  val kwSwitch: Symbol = Symbol("switch")
  val kwTypedef: Symbol = Symbol("typedef")
  val kwVoid: Symbol = Symbol("void")
  val kwWhile: Symbol = Symbol("while")
  val kwScan: Symbol = Symbol("scan")
  val kwPrint: Symbol = Symbol("print")
}
