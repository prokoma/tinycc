package tinycc.frontend.analysis

import org.scalatest.funsuite.AnyFunSuite
import tinycc.frontend.analysis.IdentifierDecl.VarDecl
import tinycc.frontend.parser.TinyCParser

class SemanticAnalysisTest extends AnyFunSuite {
  test("basic") {
    val ast = TinyCParser.parseProgram(
      """
        |int a = 10;
        |int main() {
        |  int b = a + a * 2;
        |  return 0;
        |}
        |""".stripMargin)

    val decls = new SemanticAnalysis(ast).result()
    assert(decls.size == 2)
  }

  test("undeclared identifier") {
    val ast = TinyCParser.parseProgram(
      """
        |int a = 10;
        |int main() {
        |  int b = a + c * 2;
        |  return 0;
        |}
        |""".stripMargin)

    val caught = intercept[SemanticAnalysisException]({
      new SemanticAnalysis(ast).result()
    })
    assert(caught.messages.size == 1)
    assert(caught.messages.head.message == "semantic analysis: identifier 'c' undeclared")
  }

  test("scopes") {
    val ast = TinyCParser.parseProgram(
      """
        |int a = 10;
        |int foo() {
        |  return a;
        |}
        |int main() {
        |  int b = a + a * 2;
        |  int foo = 10;
        |  int c = foo;
        |  return 0;
        |}
        |""".stripMargin)

    val decls = new SemanticAnalysis(ast).result()
    assert(decls.size == 4)

    val fooDecl = decls.collectFirst({ case (id, decl) if id.symbol.name == "foo" => decl })
    assert(fooDecl.isDefined)
    assert(fooDecl.get.isInstanceOf[VarDecl])
  }

  test("forward variable decl") {
    val ast = TinyCParser.parseProgram(
      """
        |int a;
        |int a = 1;
        |int a;
        |""".stripMargin)

    new SemanticAnalysis(ast).result()
  }

  test("incompatible forward variable decl") {
    val ast = TinyCParser.parseProgram(
      """
        |int a;
        |int a = 1;
        |void a();
        |""".stripMargin)

    assertThrows[SemanticAnalysisException](new SemanticAnalysis(ast).result())
  }

  test("forward fun decl") {
    val ast = TinyCParser.parseProgram(
      """
        |void a();
        |void a() {}
        |""".stripMargin)

    new SemanticAnalysis(ast).result()
  }

  test("incompatible forward fun decl") {
    val ast = TinyCParser.parseProgram(
      """
        |void a();
        |void a() {}
        |int a;
        |""".stripMargin)

    assertThrows[SemanticAnalysisException](new SemanticAnalysis(ast).result())
  }
}
