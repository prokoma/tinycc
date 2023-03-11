package tinycc.common.ir.parser

object Symbols {
  val colon: Symbol = Symbol(":")
  val comma: Symbol = Symbol(",")
  val parOpen: Symbol = Symbol("(")
  val parClose: Symbol = Symbol(")")
  val squareOpen: Symbol = Symbol("[")
  val squareClose: Symbol = Symbol("]")
  val curlyOpen: Symbol = Symbol("{")
  val curlyClose: Symbol = Symbol("}")
  val assign: Symbol = Symbol("=")

  val kwFn: Symbol = Symbol("fn")
  val kwLabel: Symbol = Symbol("label")
  val kwI64: Symbol = Symbol("i64")
  val kwDouble: Symbol = Symbol("double")
  val kwVoid: Symbol = Symbol("void")
  val kwPtr: Symbol = Symbol("ptr")
  val kwStruct: Symbol = Symbol("struct")
  val kwNull: Symbol = Symbol("null")
}
