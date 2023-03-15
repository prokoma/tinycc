package tinycc.frontend.parser

import org.scalatest.funsuite.AnyFunSuite
import tinycc.util.parsing.ParserException

class LexerTest extends AnyFunSuite {

  import Lexer.Token._
  import Lexer.tokenize

  test("empty") {
    val tokens = tokenize("")
    assert(tokens == Seq())
  }

  test("whitespace") {
    val tokens = tokenize("    /* test */ // test\r\n   //test\n/********bla**/    \t\r\n")
    assert(tokens == Seq())
  }

  test("unclosed comment") {
    assertThrows[ParserException](tokenize(" test /* another test"))
    assertThrows[ParserException](tokenize(" test /* "))
    assertThrows[ParserException](tokenize(" test /*"))
  }

  test("int literal") {
    val tokens = tokenize(" 100 010 10000 9999 123")
    assert(tokens == Seq(IntLiteral(100), IntLiteral(10), IntLiteral(10000), IntLiteral(9999), IntLiteral(123)))
  }

  test("double literal") {
    val tokens = tokenize(" 1.012 32.12")
    assert(tokens == Seq(DoubleLiteral(1.012), DoubleLiteral(32.12)))
  }

  test("string literals") {
    val singleQuote = '\''
    val doubleQuote = '\"'

    val tokens = tokenize(
      """'a' '\rbc' '\'"\\' // comment
        |"fdafda\"rest'd"
        |""".stripMargin)

    assert(tokens == Seq(
      StringLiteral("a", singleQuote),
      StringLiteral("\rbc", singleQuote),
      StringLiteral("'\"\\", singleQuote),
      StringLiteral("fdafda\"rest'd", doubleQuote)
    ))
  }

  test("mixed") {
    val tokens = tokenize(
      """
        |/** function which adds two numbers */
        |int add(int a, int b) {
        | return a + b; // the result
        |}
        |int main() {
        | int i = 1, j = 10;
        | int k = add(i, j);
        | print(k);
        | return 0;
        |}
        |""".stripMargin)

    assert(tokens == Seq(
      Special(Symbols.kwInt),
      Identifier(Symbol("add")),
      Special(Symbols.parOpen),
      Special(Symbols.kwInt),
      Identifier(Symbol("a")),
      Special(Symbols.comma),
      Special(Symbols.kwInt),
      Identifier(Symbol("b")),
      Special(Symbols.parClose),
      Special(Symbols.curlyOpen),
      Special(Symbols.kwReturn),
      Identifier(Symbol("a")),
      Special(Symbols.add),
      Identifier(Symbol("b")),
      Special(Symbols.semicolon),
      Special(Symbols.curlyClose),
      Special(Symbols.kwInt),
      Identifier(Symbol("main")),
      Special(Symbols.parOpen),
      Special(Symbols.parClose),
      Special(Symbols.curlyOpen),
      Special(Symbols.kwInt),
      Identifier(Symbol("i")),
      Special(Symbols.assign),
      IntLiteral(1),
      Special(Symbols.comma),
      Identifier(Symbol("j")),
      Special(Symbols.assign),
      IntLiteral(10),
      Special(Symbols.semicolon),
      Special(Symbols.kwInt),
      Identifier(Symbol("k")),
      Special(Symbols.assign),
      Identifier(Symbol("add")),
      Special(Symbols.parOpen),
      Identifier(Symbol("i")),
      Special(Symbols.comma),
      Identifier(Symbol("j")),
      Special(Symbols.parClose),
      Special(Symbols.semicolon),
      Special(Symbol("print")),
      Special(Symbols.parOpen),
      Identifier(Symbol("k")),
      Special(Symbols.parClose),
      Special(Symbols.semicolon),
      Special(Symbol("return")),
      IntLiteral(0),
      Special(Symbols.semicolon),
      Special(Symbols.curlyClose),
    ))
  }
}
