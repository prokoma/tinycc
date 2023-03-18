package tinycc.cli

import tinycc.util.parsing.combinator.{CliParsers, SeqReader}

import java.nio.file.Path
import scala.language.implicitConversions

class CliException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

object CliParser extends CliParsers {

  import Action._

  lazy val output: Parser[Path] = (shortOpt("o") | longOptWithValue("output")) ~> commit(path)

  lazy val prefix: Parser[String] = (shortOpt("p") | longOptWithValue("prefix")) ~> commit(elem)

  lazy val registerCnt: Parser[Int] = longOptWithValue("register-cnt") ~> commit(positiveInteger)

  lazy val floatRegisterCnt: Parser[Int] = longOptWithValue("float-register-cnt") ~> commit(positiveInteger)

  lazy val profile: Parser[Any] = longOpt("profile")

  lazy val optimize: Parser[Any] = shortOpt("O") | longOpt("optimize")

  lazy val verbose: Parser[Any] = shortOpt("v") | longOpt("verbose")

  lazy val inFile: Parser[Path] = path.described("input file")

  lazy val FORMAT: Parser[Format] = {
    val optVerbose = verbose ^^ (_ => (a: Format) => a.copy(verbose = true))
    val optOutFile = output ^^ { outFile => (a: Format) => a.copy(outFile = Some(outFile)) }

    "format" ~> commit(rep(optVerbose | optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(Format(inFile), opts) } described "format"
  }

  lazy val TRANSPILE_TO_C: Parser[TranspileToC] = {
    val optVerbose = verbose ^^ (_ => (a: TranspileToC) => a.copy(verbose = true))
    val optPrefix = prefix ^^ { prefix => (a: TranspileToC) => a.copy(prefix = a.prefix :+ prefix) }
    val optOutFile = output ^^ { outFile => (a: TranspileToC) => a.copy(outFile = Some(outFile)) }

    "transpile-to-c" ~> commit(rep(optVerbose | optPrefix | optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(TranspileToC(inFile), opts) } described "transpile-to-c"
  }

  lazy val COMPILE_TO_IR: Parser[CompileToIr] = {
    val optVerbose = verbose ^^ (_ => (a: CompileToIr) => a.copy(verbose = true))
    val optProfile = profile ^^ (_ => (a: CompileToIr) => a.copy(profile = true))
    val optOptimize = optimize ^^ (_ => (a: CompileToIr) => a.copy(optimize = true))
    val optOutFile = output ^^ { outFile => (a: CompileToIr) => a.copy(outFile = Some(outFile)) }

    "compile-to-ir" ~> commit(rep(optVerbose | optProfile | optOptimize | optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(CompileToIr(inFile), opts) } described "compile-to-ir"
  }

  lazy val CODEGEN: Parser[Codegen] = {
    val optVerbose = verbose ^^ (_ => (a: Codegen) => a.copy(verbose = true))
    val optProfile = profile ^^ (_ => (a: Codegen) => a.copy(profile = true))
    val optOptimize = optimize ^^ (_ => (a: Codegen) => a.copy(optimize = true))
    val optRegisterCnt = registerCnt ^^ { registerCnt => (a: Codegen) => a.copy(registerCnt = registerCnt) }
    val optFloatRegisterCnt = floatRegisterCnt ^^ { floatRegisterCnt => (a: Codegen) => a.copy(floatRegisterCnt = floatRegisterCnt) }
    val optOutFile = output ^^ { outFile => (a: Codegen) => a.copy(outFile = Some(outFile)) }

    "codegen" ~> commit(rep(optVerbose | optProfile | optOptimize | optRegisterCnt | optFloatRegisterCnt | optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(Codegen(inFile), opts) } described "codegen"
  }

  lazy val COMPILE: Parser[Compile] = {
    val optVerbose = verbose ^^ (_ => (a: Compile) => a.copy(verbose = true))
    val optProfile = profile ^^ (_ => (a: Compile) => a.copy(profile = true))
    val optOptimize = optimize ^^ (_ => (a: Compile) => a.copy(optimize = true))
    val optRegisterCnt = registerCnt ^^ { registerCnt => (a: Compile) => a.copy(registerCnt = registerCnt) }
    val optFloatRegisterCnt = floatRegisterCnt ^^ { floatRegisterCnt => (a: Compile) => a.copy(floatRegisterCnt = floatRegisterCnt) }
    val optOutFile = output ^^ { outFile => (a: Compile) => a.copy(outFile = Some(outFile)) }

    "compile" ~> commit(rep(optVerbose | optProfile | optOptimize | optRegisterCnt | optFloatRegisterCnt | optOutFile) ~ inFile) ^^ { case opts ~ inFile => applyOpts(Compile(inFile), opts) } described "compile"
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
