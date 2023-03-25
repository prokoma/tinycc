package tinycc.frontend

import org.scalatest.funsuite.AnyFunSuite
import tinycc.common.ir.IrProgram
import tinycc.common.transform.BasicBlockScheduling
import tinycc.frontend.TinyCCompiler.TinyCCompilerException
import tinycc.frontend.analysis.TypeAnalysis.TypeAnalysisException
import tinycc.frontend.ast.AstPrinter
import tinycc.frontend.parser.TinyCParser
import tinycc.util.Testing.exampleSources

import java.nio.file.Files

class FrontendTest extends AnyFunSuite {
  exampleSources.foreach(file => {
    val name = file.getFileName.toString
    val source = Files.readString(file)

    test(s"compile $name") {
      val ast = TinyCParser.parseProgram(source)
      TinyCCompiler(ast).result()
    }

    test(s"compile and validate $name") {
      val ast = TinyCParser.parseProgram(source)
      val irProgram = TinyCCompiler(ast).result()
      new BasicBlockScheduling().transformProgram(irProgram)
      irProgram.validate()
    }
  })

  private def compile(s: String): IrProgram =
    TinyCCompiler(TinyCParser.parseProgram(s)).result()

  test("missing main") {
    assertThrows[TinyCCompilerException](compile(
      """
        |int foo() {
        | return 0;
        |}
        |""".stripMargin))
  }

  test("missing return in main") {
    assertThrows[TinyCCompilerException](compile(
      """
        |int main() {
        | int x = 5;
        |}
        |""".stripMargin))
  }

  test("missing return in unreachable block") {
    compile(
      """
        |int main() {
        | if(1)
        |  return 1;
        | else
        |  return 0;
        |
        | int x = 5; // this is unreachable, so missing return is ok
        |}
        |""".stripMargin)
  }

  test("missing return in void fun") {
    compile(
      """
        |void foo() {
        | int x = 5;
        |}
        |int main() {
        | foo();
        | return 0;
        |}
        |""".stripMargin)
  }
}
