package tinycc.frontend.tinyc.analysis

import org.scalatest.funsuite.AnyFunSuite
import tinycc.frontend.tinyc.TypeMap
import tinycc.frontend.tinyc.analysis.TypeAnalysis.TypeAnalysisException
import tinycc.frontend.tinyc.parser.TinyCParser

class TypeAnalysisTest extends AnyFunSuite {
  private def analyze(s: String): TypeMap = {
    val ast = TinyCParser.parseProgram(s)
    val decls = new SemanticAnalysis(ast).result()
    new TypeAnalysis(ast, decls).result()
  }

  test("literals") {
    analyze(
      """
        |int main() {
        |   int x = 1;
        |   char y = 'a';
        |   double z = 2.0;
        |   char * s = "hello";
        |}
        |""".stripMargin)
  }

  test("recursive struct with ptr") {
    assertThrows[TypeAnalysisException](analyze(
      """
        |struct x {
        | x next;
        |};
        |""".stripMargin))

    analyze(
      """
        |struct x {
        | x * next;
        |};
        |""".stripMargin)
  }

  test("array") {
    analyze(
      """
        |int a[10];
        |struct s {
        | int a;
        | int b[10];
        |};
        |s c[10];
        |""".stripMargin)
  }

  test("invalid array size") {
    assertThrows[TypeAnalysisException](analyze(
      """
        |int a[10 + 1];
        |""".stripMargin))

    assertThrows[TypeAnalysisException](analyze(
      """
        |int a[-5];
        |""".stripMargin))

    assertThrows[TypeAnalysisException](analyze(
      """
        |int a[0];
        |""".stripMargin))
  }

  test("fun ptr") {
    analyze(
      """
        |typedef void (*foo)(int);
        |""".stripMargin)
  }

  test("recursive fun ptr") {
    assertThrows[TypeAnalysisException](analyze(
      """
        |typedef void (*foo)(int, foo);
        |""".stripMargin))
  }
}
