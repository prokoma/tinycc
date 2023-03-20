package tinycc.common.ir.parser

import org.scalatest.funsuite.AnyFunSuite
import tinycc.common.ir.IrPrinter
import tinycc.common.transform.BasicBlockScheduling
import tinycc.frontend.TinyCCompiler
import tinycc.frontend.parser.TinyCParser
import tinycc.util.Reporter
import tinycc.util.Testing.exampleSources
import tinycc.util.parsing.ParserException

import java.nio.file.Files

class IrParserTest extends AnyFunSuite {
  exampleSources.foreach(file => {
    val name = file.getFileName.toString
    val source = Files.readString(file)

    test(s"print and parse $name") {
      val ast = TinyCParser.parseProgram(source)
      val irProgram = TinyCCompiler(ast).result()

      new BasicBlockScheduling().transformProgram(irProgram)
      irProgram.validate()

      val irSource = new IrPrinter().printToString(irProgram)
      val irReporter = new Reporter(irSource)
      val irProgram2 = try IrParser.parseProgram(irSource) catch {
        case err: ParserException => throw new RuntimeException(err.format(irReporter))
      }

      irProgram2.validate()
    }
  })

}
