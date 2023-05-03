package tinycc.cli

import tinycc.ProgramException
import tinycc.backend.t86.{T86Backend, T86Utils}
import tinycc.common.Optimizer
import tinycc.common.ir.parser.IrParser
import tinycc.common.ir.{IrPrinter, IrProgram}
import tinycc.frontend.tinyc.TinyCCompiler
import tinycc.frontend.tinyc.ast.{AstPrinter, AstPrinterC, AstProgram}
import tinycc.frontend.tinyc.parser.TinyCParser
import tinycc.util.Profiler.{instance, profile}
import tinycc.util.{IndentWriter, Logging, Profiler, Reporter}

import java.io.{IOException, PrintStream, PrintWriter}
import java.nio.file.{Files, Path}
import scala.util.Using

trait Action extends Product with Serializable {
  def execute(): Unit
}

object Action {

  import Console.{BOLD, RESET, UNDERLINED}

  private def bold(s: String): String = BOLD + s + RESET

  private def underlined(s: String): String = UNDERLINED + s + RESET

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

    def readSource(): String = {
      try
        Files.readString(inFile)
      catch {
        case ex: IOException => throw new CliException("failed to read input file", ex)
      }
    }

    def withSource[T](f: String => T): T = {
      val source = readSource()
      val reporter = new Reporter(source, Some(inFile.getFileName.toString))
      try
        f(source)
      catch {
        case ex: ProgramException => throw new CliException(ex.format(reporter))
      }
    }
  }

  trait IrProgramInput {
    def withIrProgram[T](f: IrProgram => T): T
  }

  trait TinyCSourceFileInput extends FileInput with IrProgramInput {
    def withParsedProgram[T](f: AstProgram => T): T =
      withSource(source => f(profile("tinyCParser", TinyCParser.parseProgram(source))))

    def withIrProgram[T](f: IrProgram => T): T =
      withParsedProgram[T](ast => {
        val irProgram = profile("frontend", TinyCCompiler(ast).result())
        irProgram.validate()
        f(irProgram)
      })
  }

  trait IrFileInput extends FileInput with IrProgramInput {
    def withParsedProgram[T](f: IrProgram => T): T =
      withSource(source => {
        val irProgram = profile("irParser", IrParser.parseProgram(source))
        irProgram.validate()
        f(irProgram)
      })

    def withIrProgram[T](f: IrProgram => T): T = withParsedProgram(f)
  }

  trait VerboseLogging {
    def verbose: Boolean

    def withVerboseLogging[T](f: => T): T = {
      Logging.enableLogging = verbose
      f
    }
  }

  trait Profiling {
    def profile: Boolean

    def withProfiling[T](f: => T): T = {
      if (profile) {
        val result = Profiler.profile("program", f)
        Profiler.instance.printReport(Console.err)
        result
      } else f
    }
  }

  trait CommonTransformAction extends Action with FileInput with StdoutOrFileOutput with Profiling with VerboseLogging

  case class Format(inFile: Path,
                    profile: Boolean = false,
                    verbose: Boolean = false,
                    outFile: Option[Path] = None) extends CommonTransformAction with TinyCSourceFileInput {

    override def execute(): Unit = withVerboseLogging(withPrintStream(out => withParsedProgram(ast => {
      out.println(new AstPrinter().printToString(ast))
    })))
  }

  object Format extends ActionInfo {
    val synopsis: String = s"${bold("tinycc format")} [-v | --verbose] [--profile] [-o <file> | --output=<file>] <file>"

    val description: String = s"Parse the given TinyC source ${underlined("<file>")} and print it again, this time formatted. The result is either written to a file, or printed to the standard output."
  }

  case class TranspileToC(inFile: Path,
                          profile: Boolean = false,
                          verbose: Boolean = false,
                          outFile: Option[Path] = None,
                          prefix: Seq[String] = Seq.empty) extends CommonTransformAction with TinyCSourceFileInput {

    override def execute(): Unit = withVerboseLogging(withPrintStream(out => withParsedProgram(ast => {
      prefix.foreach(out.println)
      out.println(new AstPrinterC().printToString(ast))
    })))
  }

  object TranspileToC extends ActionInfo {
    val synopsis: String = s"${bold("tinycc transpile-to-c")} [-v | --verbose] [--profile] [-p <string> | --prefix=<string>] [-o <file> | --output=<file>] <file>"

    val description: String = s"Transpile the given TinyC source ${underlined("<file>")} to C66 source code with optional prefix. The result is either written to a file, or printed to the standard output."
  }

  case class CompileToIr(inFile: Path,
                         profile: Boolean = false,
                         verbose: Boolean = false,
                         optimize: Boolean = false,
                         outFile: Option[Path] = None) extends CommonTransformAction with TinyCSourceFileInput {

    override def execute(): Unit = withVerboseLogging(withProfiling(withPrintStream(out => withIrProgram(irProgram => {
      if (optimize)
        Profiler.profile("middleend", new Optimizer(true).transformProgram(irProgram))

      out.println(new IrPrinter().printToString(irProgram))
    }))))
  }

  object CompileToIr extends ActionInfo {
    val synopsis: String = s"${bold("tinycc compile-to-ir")} [-v | --verbose] [--profile] [--optimize] [-o <file> | --output=<file>] <file>"

    val description: String = s"Compile the given TinyC source ${underlined("<file>")} to intermediate representation. If ${underlined("--optimize")} is specified, apply MemToReg and SingleFunExit transforms. The result is either written to a file, or printed to the standard output."
  }

  case class Optimize(inFile: Path,
                      profile: Boolean = false,
                      verbose: Boolean = false,
                      outFile: Option[Path] = None) extends CommonTransformAction with IrFileInput {

    override def execute(): Unit = withVerboseLogging(withProfiling(withPrintStream(out => withIrProgram(irProgram => {
      Profiler.profile("middleend", new Optimizer(true).transformProgram(irProgram))

      out.println(new IrPrinter().printToString(irProgram))
    }))))
  }

  object Optimize extends ActionInfo {
    val synopsis: String = s"${bold("tinycc optimize")} [-v | --verbose] [--profile] [-o <file> | --output=<file>] <file>"

    val description: String = s"Optimize the given intermediate code stored in ${underlined("<file>")}. The result is either written to a file, or printed to the standard output."
  }

  case class Codegen(inFile: Path,
                     profile: Boolean = false,
                     verbose: Boolean = false,
                     optimize: Boolean = false,
                     registerCnt: Int = T86Utils.defaultMachineRegCount,
                     floatRegisterCnt: Int = T86Utils.defaultMachineFRegCount,
                     outFile: Option[Path] = None) extends Action with IrFileInput with StdoutOrFileOutput with Profiling with VerboseLogging {

    override def execute(): Unit = withVerboseLogging(withProfiling(withPrintStream(out => withIrProgram(irProgram => {
      Profiler.profile("middleend", new Optimizer(optimize).transformProgram(irProgram))

      val asmString = Profiler.profile("backend", new T86Backend(irProgram, registerCnt, floatRegisterCnt).resultAsString())
      out.print(asmString)
    }))))
  }

  object Codegen extends ActionInfo {
    val synopsis: String = s"${bold("tinycc codegen")} [-v | --verbose] [--profile] [--optimize] [--register-cnt=<int>] [--float-register-cnt=<int>] [-o <file> | --output=<file>] <file>"

    val description: String = s"Compile the given intermediate code stored in ${underlined("<file>")} to Tiny86 assembly listing. If ${underlined("--optimize")} is specified, apply MemToReg and SingleFunExit transforms. The result is either written to a file, or printed to the standard output."
  }

  case class Compile(inFile: Path,
                     profile: Boolean = false,
                     verbose: Boolean = false,
                     optimize: Boolean = false,
                     registerCnt: Int = T86Utils.defaultMachineRegCount,
                     floatRegisterCnt: Int = T86Utils.defaultMachineFRegCount,
                     outFile: Option[Path] = None) extends Action with TinyCSourceFileInput with StdoutOrFileOutput with Profiling with VerboseLogging {

    override def execute(): Unit = withVerboseLogging(withProfiling(withPrintStream(out => withIrProgram(irProgram => {
      Profiler.profile("middleend", new Optimizer(optimize).transformProgram(irProgram))

      val asmString = Profiler.profile("backend", new T86Backend(irProgram, registerCnt, floatRegisterCnt).resultAsString())
      out.print(asmString)
    }))))
  }

  object Compile extends ActionInfo {
    val synopsis: String = s"${bold("tinycc compile")} [-v | --verbose] [--profile] [--optimize] [--register-cnt=<int>] [--float-register-cnt=<int>] [-o <file> | --output=<file>] <file>"

    val description: String = s"Compile the given TinyC source ${underlined("<file>")} to Tiny86 assembly listing. If ${underlined("--optimize")} is specified, apply MemToReg and SingleFunExit transforms. Combines the ${bold("compile-to-ir")} and ${bold("codegen")} actions. The result is either written to a file, or printed to the standard output."
  }

  case object Help extends Action with ActionInfo {

    override def execute(): Unit = {
      Using(new IndentWriter(new PrintWriter(Console.err), " " * 3))(out => {
        out.write(bold("NAME"))
        out.withIndent({
          out.write(bold("tinycc") + " - Modular TinyC compiler written in Scala")
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

  val actions: Seq[ActionInfo] = Seq(Format, TranspileToC, CompileToIr, Optimize, Codegen, Compile, Help)
}


