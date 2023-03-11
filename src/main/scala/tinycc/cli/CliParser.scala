package tinycc.cli

import tinycc.util.parsing.combinator.{CliParsers, SeqReader}

import java.nio.file.Path
import scala.language.implicitConversions

class CliException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

object CliParser extends CliParsers {

  import Action._

  lazy val output: Parser[Path] = (shortOpt("o") | longOpt("output")) ~> path

  lazy val prefix: Parser[String] = (shortOpt("p") | longOpt("prefix")) ~> elem

  lazy val inFile: Parser[Path] = path.described("input file")

  lazy val FORMAT: Parser[Format] = {
    val optOutFile = output ^^ { outFile => (a: Format) => a.copy(outFile = Some(outFile)) }

    "format" ~> commit(rep(optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(Format(inFile), opts) } described "format"
  }

  lazy val TRANSPILE_TO_C: Parser[TranspileToC] = {
    val optOutFile = output ^^ { outFile => (a: TranspileToC) => a.copy(outFile = Some(outFile)) }
    val optPrefix = prefix ^^ { prefix => (a: TranspileToC) => a.copy(prefix = a.prefix :+ prefix) }

    "transpile-to-c" ~> commit(rep(optOutFile | optPrefix) ~ inFile) ^^ { case opts ~ inFile => applyOpts(TranspileToC(inFile), opts) } described "transpile-to-c"
  }

  lazy val COMPILE_TO_IR: Parser[CompileToIr] = {
    val optOutFile = output ^^ { outFile => (a: CompileToIr) => a.copy(outFile = Some(outFile)) }

    "compile-to-ir" ~> commit(rep(optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(CompileToIr(inFile), opts) } described "compile-to-ir"
  }

  lazy val CODEGEN: Parser[Codegen] = {
    val optOutFile = output ^^ { outFile => (a: Codegen) => a.copy(outFile = Some(outFile)) }

    "codegen" ~> commit(rep(optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(Codegen(inFile), opts) } described "codegen"
  }

  lazy val COMPILE: Parser[Compile] = {
    val optOutFile = output ^^ { outFile => (a: Compile) => a.copy(outFile = Some(outFile)) }

    "compile" ~> commit(rep(optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(Compile(inFile), opts) } described "compile"
  }

  lazy val HELP: Parser[Action] = {
    "help" ^^ (_ => Help) described "help"
  }

  lazy val ACTION: Parser[Action] = (FORMAT | TRANSPILE_TO_C | COMPILE_TO_IR | CODEGEN | COMPILE | HELP)

  def parseArgs(args: Seq[String]): Action = parse(ACTION <~ EOI, SeqReader(args)) match {
    case Accept(action, _, _) => action
    case Reject(expectation, reminding, _) => throw new CliException(s"cli error: " + formatErrorMessage(expectation, reminding))
  }
}
