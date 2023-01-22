package tinycc.frontend

class LexerTest extends munit.FunSuite {
//  test("empty") {
//    val ti = new Lexer.TokenInput("")
//    println(ti)
//    assert(ti.isEmpty)
//  }
//
//  test("whitespace") {
//    val ti = new Lexer.TokenInput("    /* test */ // test\r\n   //test\n/********bla**/    \t\r\n")
//    println(ti)
//    println(ti.pos)
//    assert(ti.isEmpty)
//  }
//
//  test("int literal") {
//    val ti = new Lexer.TokenInput(" 100 010 10000 9999 123")
//    assertEquals(ti.asInstanceOf[Iterable[Token]], Iterable(IntLiteral(100), IntLiteral(10), IntLiteral(10000), IntLiteral(9999), IntLiteral(123)))
//  }

  test("string literals") {
    val singleQuote = '\''
    val doubleQuote = '\"'
    var ti = new Lexer.TokenInput("""'a' '\rbc' '\'"\\'""")
    while(!ti.isEmpty) {
      println(ti.head)
      ti = ti.tail
    }
//    assertEquals(ti.asInstanceOf[Iterable[Token]], Iterable(StringLiteral("a", singleQuote), StringLiteral("\rbc", singleQuote), StringLiteral("'\"\\", singleQuote)))
  }

//  test("lexer") {
//    val scanner = new Lexer.TokenInput("int main() { return 12; }")
//
//    println(scanner)
//  }
}
