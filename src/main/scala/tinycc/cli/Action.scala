package tinycc.cli

import tinycc.ProgramException
import tinycc.backend.BasicBlockScheduling
import tinycc.backend.t86.insel.T86InstructionSelection
import tinycc.backend.t86.regalloc.T86RegisterAllocator
import tinycc.backend.t86.{T86AsmPrinter, T86FunProcessor, T86LabelProcessor}
import tinycc.common.ir.parser.IrParser
import tinycc.common.ir.{IrPrinter, IrProgram}
import tinycc.frontend.TinyCCompiler
import tinycc.frontend.analysis.{SemanticAnalysis, TypeAnalysis}
import tinycc.frontend.ast.{AstPrinterC, AstProgram}
import tinycc.frontend.parser.TinyCParser
import tinycc.util.{IndentWriter, Reporter}

import java.io.{IOException, PrintStream, PrintWriter}
import java.nio.file.{Files, Path}
import scala.util.Using

trait Action extends Product with Serializable {
  def execute(): Unit
}

trait ActionInfo {
  def synopsis: String

  def description: String
}

trait StdoutOrFileOutput {
  def outFile: Option[Path]

  def withPrintStream[T](f: PrintStream => T): T = outFile match {
    case Some(path) =>
      util.Using(new PrintStream(Files.newOutputStream(path)))(f).get

    case None =>
      f(Console.out)
  }
}

trait FileInput {
  def inFile: Path

  def getSource(): String = {
    try
      Files.readString(inFile)
    catch {
      case ex: IOException => throw new CliException("failed to read input file", ex)
    }
  }

  def withSource[T](f: String => T): T = {
    val source = getSource()
    val reporter = new Reporter(source, Some(inFile.getFileName.toString))
    try
      f(source)
    catch {
      case ex: ProgramException => throw new CliException(ex.format(reporter))
    }
  }
}

trait TinyCSourceFileInput extends FileInput {
  def withParsedProgram[T](f: AstProgram => T): T =
    withSource(source => f(TinyCParser.parseProgram(source)))
}

trait IrFileInput extends FileInput {
  def withParsedProgram[T](f: IrProgram => T): T =
    withSource(source => f(IrParser.parseProgram(source)))
}

object Action {

  import Console.{BOLD, RESET}

  private def bold(s: String): String = BOLD + s + RESET

  case class Format(inFile: Path, outFile: Option[Path] = None) extends Action with TinyCSourceFileInput with StdoutOrFileOutput {
    override def execute(): Unit = withPrintStream(out => withParsedProgram(ast => {
      out.println(new AstPrinterC().printToString(ast))
    }))
  }

  object Format extends ActionInfo {
    val synopsis: String = s"${bold("tinycc format")} [-o <file> | --output=<file>] <file>"

    val description: String = "Parse the given TinyC source <file> and print it again, this time formatted. The result is either written to a file, or printed to the standard output."
  }

  case class TranspileToC(inFile: Path, outFile: Option[Path] = None, prefix: Seq[String] = Seq.empty) extends Action with TinyCSourceFileInput with StdoutOrFileOutput {
    override def execute(): Unit = withPrintStream(out => withParsedProgram(ast => {
      prefix.foreach(out.println)
      out.println(new AstPrinterC().printToString(ast))
    }))
  }

  object TranspileToC extends ActionInfo {
    val synopsis: String = s"${bold("tinycc transpile-to-c")} [-p <string> | --prefix=<string>] [-o <file> | --output=<file>] <file>"

    val description: String = "Transpile the given TinyC source <file> to C66 source code with optional prefix. The result is either written to a file, or printed to the standard output."
  }

  case class CompileToIr(inFile: Path, outFile: Option[Path] = None) extends Action with TinyCSourceFileInput with StdoutOrFileOutput {
    override def execute(): Unit = withPrintStream(out => withParsedProgram(ast => {
      val declarations = new SemanticAnalysis(ast).result()
      val typeMap = new TypeAnalysis(ast, declarations).result()
      val irProgram = new TinyCCompiler(ast, declarations, typeMap).result()
      out.println(new IrPrinter().printToString(irProgram))
    }))
  }

  object CompileToIr extends ActionInfo {
    val synopsis: String = s"${bold("tinycc compile-to-ir")} [-o <file> | --output=<file>] <file>"

    val description: String = s"Compile the given TinyC source <file> to intermediate representation. The result is either written to a file, or printed to the standard output."
  }

  case class Codegen(inFile: Path, outFile: Option[Path] = None) extends Action with IrFileInput with StdoutOrFileOutput {
    override def execute(): Unit = withPrintStream(out => withParsedProgram(irProgram => {
      Console.err.println(new IrPrinter().printToString(irProgram))

      new BasicBlockScheduling().transformProgram(irProgram)
      irProgram.validate()
      val t86Program = T86InstructionSelection(irProgram).result()
      Console.err.println(new T86AsmPrinter().printToString(t86Program.flatten))

      T86RegisterAllocator().transformProgram(t86Program)
      new T86FunProcessor().transformProgram(t86Program)
      val t86Listing = new T86LabelProcessor(t86Program.flatten).result()

      out.print(new T86AsmPrinter().printToString(t86Listing))
    }))
  }

  object Codegen extends ActionInfo {
    val synopsis: String = s"${bold("tinycc codegen")} [-o <file> | --output=<file>] <file>"

    val description: String = s"Compile the given intermediate code stored in <file> to Tiny86 assembly listing. The result is either written to a file, or printed to the standard output."
  }

  case class Compile(inFile: Path, outFile: Option[Path] = None) extends Action with TinyCSourceFileInput with StdoutOrFileOutput {
    override def execute(): Unit = withPrintStream(out => withParsedProgram(ast => {
      val declarations = new SemanticAnalysis(ast).result()
      val typeMap = new TypeAnalysis(ast, declarations).result()
      val irProgram = new TinyCCompiler(ast, declarations, typeMap).result()
      Console.err.println(new IrPrinter().printToString(irProgram))

      new BasicBlockScheduling().transformProgram(irProgram)
      irProgram.validate()
      val t86Program = T86InstructionSelection(irProgram).result()
      Console.err.println(new T86AsmPrinter().printToString(t86Program.flatten))

      T86RegisterAllocator().transformProgram(t86Program)
      new T86FunProcessor().transformProgram(t86Program)
      val t86Listing = new T86LabelProcessor(t86Program.flatten).result()

      out.print(new T86AsmPrinter().printToString(t86Listing))
    }))
  }

  object Compile extends ActionInfo {
    val synopsis: String = s"${bold("tinycc compile")} [-o <file> | --output=<file>] <file>"

    val description: String = s"Compile the given TinyC source <file> to Tiny86 assembly listing. Combines the ${bold("compile-to-ir")} and ${bold("codegen")} actions. The result is either written to a file, or printed to the standard output."
  }

  case object Help extends Action with ActionInfo {
    override def execute(): Unit = {
      Using(new IndentWriter(new PrintWriter(Console.err), " " * 3))(out => {
        out.write(bold("NAME"))
        out.withIndent({
          out.write(bold("tinycc") + "- Modular TinyC compiler written in Scala")
          out.nl()
        })
        out.nl()
        out.write(bold("SYNOPSIS"))
        out.withIndent({
          actions.foreach(info => {
            out.write(info.synopsis)
            out.nl()
          })
        })
        out.nl()
        out.write(bold("DESCRIPTION"))
        out.withIndent({
          actions.foreach(info => {
            out.write(info.synopsis)
            out.nl()
            out.withIndent({
              out.write(info.description) // TODO: word wrap
            })
            out.nl()
          })
        })
      }).get
    }

    val synopsis: String = bold("tinycc help")

    val description: String = "Print this help and exit."
  }

  val actions: Seq[ActionInfo] = Seq(Format, TranspileToC, CompileToIr, Codegen, Compile, Help)
}


