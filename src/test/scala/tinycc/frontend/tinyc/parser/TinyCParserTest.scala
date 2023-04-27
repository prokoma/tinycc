package tinycc.frontend.tinyc.parser

import org.scalatest.funsuite.AnyFunSuite
import tinycc.frontend.tinyc.ast.AstPrinter
import tinycc.util.Testing.exampleSources

import java.nio.file.Files

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

  exampleSources.foreach(file => {
    val name = file.getFileName.toString
    val source = Files.readString(file)

    test(s"parse $name") {
      TinyCParser.parseProgram(source)
    }

    test(s"parse, print and parse again $name") {
      val ast = TinyCParser.parseProgram(source)
      val source2 = new AstPrinter().printToString(ast)
      val ast2 = TinyCParser.parseProgram(source2)
      val source3 = new AstPrinter().printToString(ast2)

      assert(source2 == source3)
    }
  })
}

