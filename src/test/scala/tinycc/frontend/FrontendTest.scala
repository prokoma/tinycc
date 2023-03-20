package tinycc.frontend

import org.scalatest.funsuite.AnyFunSuite
import tinycc.common.transform.BasicBlockScheduling
import tinycc.frontend.ast.AstPrinter
import tinycc.frontend.parser.TinyCParser
import tinycc.util.Testing.exampleSources

import java.nio.file.Files

class FrontendTest extends AnyFunSuite {
  exampleSources.foreach(file => {
    val name = file.getFileName.toString
    val source = Files.readString(file)

    test(s"parse, print and parse again $name") {
      val ast = TinyCParser.parseProgram(source)
      val source2 = new AstPrinter().printToString(ast)
      val ast2 = TinyCParser.parseProgram(source2)
      val source3 = new AstPrinter().printToString(ast2)

      assert(source2 == source3)
    }

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
}
