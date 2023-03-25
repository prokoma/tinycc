package tinycc

import org.scalatest.ParallelTestExecution
import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import tinycc.backend.t86.{T86Backend, T86Utils}
import tinycc.common.Optimizer
import tinycc.frontend.TinyCCompiler
import tinycc.frontend.parser.TinyCParser
import tinycc.util.Testing.{exampleSources, extractInData, extractOutData}

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.language.postfixOps
import scala.sys.process.{Process, ProcessLogger}

class End2EndTest extends AnyFunSuite with TimeLimits with ParallelTestExecution {
  private def compile(source: String, enableOptionalOptimizations: Boolean = false): String = {
    val ast = TinyCParser.parseProgram(source)
    val irProgram = TinyCCompiler(ast).result()
    new Optimizer(enableOptionalOptimizations).transformProgram(irProgram)
    new T86Backend(irProgram, T86Utils.defaultMachineRegCount, T86Utils.defaultMachineFRegCount).resultAsString()
  }

  private def runT86(asm: String, inData: String): String = {
    val tmpFile = Files.createTempFile(s"tinycc", ".t86")
    try {
      Files.writeString(tmpFile, asm)

      failAfter(1 minute) {
        val inStream = new ByteArrayInputStream(inData.getBytes(StandardCharsets.UTF_8))
        val devNull = ProcessLogger(_ => ())
        // Yeah, i know, i know, wtf is #< and !!. I tried to find something better, but couldn't.
        // #< set stdin of the process, !! executes the process, waits for completion (throws on non-zero exit code) and returns stdout as string.
        (Process("t86/build/t86-cli/t86-cli", Seq("run", tmpFile.toString)) #< inStream !! devNull)
      }
    } finally {
      Files.deleteIfExists(tmpFile)
    }
  }

  exampleSources.foreach(file => {
    val name = file.getFileName.toString
    val source = Files.readString(file)

    val inData = extractInData(source, file.getParent)
    val refData = extractOutData(source, file.getParent)

    test(s"compile to asm $name") {
      compile(source)
    }

    test(s"compile to asm and execute $name") {
      val asm = compile(source)
      assert(runT86(asm, inData) == refData)
    }

    test(s"compile to asm with optimizations $name") {
      compile(source, true)
    }

    test(s"compile to asm with optimizations and execute $name") {
      val asm = compile(source, true)
      assert(runT86(asm, inData) == refData)
    }
  })

}
