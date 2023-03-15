package tinycc.frontend.parser

import org.scalatest.funsuite.AnyFunSuite

class TinyCParserTest extends AnyFunSuite {

  import TinyCParser.parseProgram

  test("empty") {
    val ast = parseProgram("")
    assert(ast.body.isEmpty)
  }

  test("simple") {
    parseProgram(
      """
        |int main() {
        |  return 0;
        |}
        |int main() {
        |  return 0;
        |}
        |""".stripMargin)
  }

  test("multiple decls") {
    parseProgram(
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
  }
}

