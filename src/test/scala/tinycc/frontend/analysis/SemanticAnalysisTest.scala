package tinycc.frontend.analysis

import munit.FunSuite
import tinycc.frontend.{Symbols, TinyCParser}
import tinycc.frontend.ast.{AstBlock, AstFunDecl, AstNamedType, AstVarDecl}
import tinycc.util.parsing.SourceLocation

class SemanticAnalysisTest extends FunSuite {
  test("basic") {
    val ast = TinyCParser.parseProgram(
      """
        |int a = 10;
        |int main() {
        |  int b = a + a * 2;
        |  return 0;
        |}
        |""".stripMargin)

    val res = new SemanticAnalysis(ast).result
    val decls = res.getOrElse(Map.empty)
    val errors = res.left.getOrElse(Seq.empty)

    assert(errors.isEmpty)
    assertEquals(decls.size, 2)
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

    val res = new SemanticAnalysis(ast).result
    val decls = res.getOrElse(Map.empty)
    val errors = res.left.getOrElse(Seq.empty)

    assert(decls.isEmpty)
    assertEquals(errors.size, 1)
    assertEquals(errors.head.getMessage, "identifier 'c' undeclared")
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

    val res = new SemanticAnalysis(ast).result
    val decls = res.getOrElse(Map.empty)
    val errors = res.left.getOrElse(Seq.empty)

    assert(errors.isEmpty)
    assertEquals(decls.size, 4)

    val fooDecl = decls.collectFirst({ case (id, decl) if id.symbol.name == "foo" => decl })
    assert(fooDecl.isDefined)
    assert(fooDecl.get.isInstanceOf[VarDecl])
  }
}
