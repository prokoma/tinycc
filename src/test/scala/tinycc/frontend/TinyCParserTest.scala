package tinycc.frontend

import munit.FunSuite
import tinycc.frontend.parser.TinyCParser

class TinyCParserTest extends FunSuite {
  test("empty") {
    val ast = TinyCParser.parseProgram("")
    println(ast)
  }

  test("simple") {
    val ast = TinyCParser.parseProgram(
      """
        |int main() {
        |  return 0;
        |}
        |int main() {
        |  return 0;
        |}
        |""".stripMargin)

    println(ast, ast.body)

  }

  test("multiple decls") {
    val ast = TinyCParser.parseProgram(
      """
        |/** function which adds two numbers */
        |int add(int a, int b) {
        | return a + b; // the result
        |}
        |int main() {
        |  int i = 1, int j = 10;
        |  int k = add(i, j);
        |  print(k);
        |  return 0;
        |}
        |""".stripMargin)

    println(ast)
  }

}

