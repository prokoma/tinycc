package tinycc.cli

import tinycc.backend.BasicBlockScheduling
import tinycc.backend.t86.insel.T86InstructionSelection
import tinycc.backend.t86.regalloc.T86RegisterAllocator
import tinycc.backend.t86.{T86AsmPrinter, T86FunProcessor, T86LabelProcessor}
import tinycc.common.ir.IrPrinter
import tinycc.frontend.analysis.{SemanticAnalysis, TypeAnalysis}
import tinycc.frontend.ast.{AstPrinter, AstPrinterC}
import tinycc.frontend.{TinyCCompiler, TinyCParser}

import java.io.PrintStream
import java.nio.file.{Files, Path}

trait Action extends Product with Serializable {
  def execute(): Unit
}

trait StdoutOrFileOutput {
  def outFile: Option[Path]

  def withPrintStream(f: PrintStream => Any): Unit = outFile match {
    case Some(path) =>
      util.Using(new PrintStream(Files.newOutputStream(path)))(f).get

    case None =>
      f(Console.out)
  }
}

object Action {
  case class TranspileToC(file: Path, outFile: Option[Path] = None, prefix: Seq[String] = Seq.empty) extends Action with StdoutOrFileOutput {
    override def execute(): Unit = {
      val str = Files.readString(file)
      val reporter = new Reporter(str, Some(file.getFileName.toString))

      withPrintStream(out => {
        val chain = for {
          ast <- TinyCParser.parseProgram(str)
        } yield {
          prefix.foreach(out.println)
          out.println(new AstPrinterC().printToString(ast))
        }

        chain match {
          case Left(ex) =>
            Console.err.println(ex.format(reporter))
            sys.exit(1)

          case Right(_) =>
        }
      })
    }
  }

  case class Compile(file: Path, outFile: Option[Path] = None) extends Action with StdoutOrFileOutput {
    override def execute(): Unit = {
      val str = Files.readString(file)
      val reporter = new Reporter(str, Some(file.getFileName.toString))

      withPrintStream(out => {
        val chain = for {
          ast <- TinyCParser.parseProgram(str)
          _ = Console.out.println(new AstPrinter().printToString(ast))

          decls <- new SemanticAnalysis(ast).result
          typeMap <- new TypeAnalysis(ast, decls).result
          irProg <- new TinyCCompiler(ast, decls, typeMap).result
          _ = Console.out.println(new IrPrinter().printToString(irProg))

          _ = (new BasicBlockScheduling).transformProgram(irProg)
          _ = irProg.validate()
          t86Prog <- T86InstructionSelection(irProg).result()
          _ = Console.out.println(new T86AsmPrinter().printToString(t86Prog.flatten))

          _ = T86RegisterAllocator(t86Prog).result()
          _ = new T86FunProcessor(t86Prog).result()
          t86Listing <- new T86LabelProcessor(t86Prog.flatten).result()
        } yield {
          out.print(new T86AsmPrinter().printToString(t86Listing))
        }

        chain match {
          case Left(ex) =>
            Console.err.println(ex.format(reporter))
            sys.exit(1)

          case Right(_) =>
        }
      })
    }
  }
}
