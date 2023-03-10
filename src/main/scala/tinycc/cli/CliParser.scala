package tinycc.cli

import tinycc.util.parsing.combinator.{CliParsers, SeqReader}

import java.nio.file.Path
import scala.language.implicitConversions

class CliException(message: String) extends RuntimeException(message)

object CliParser extends CliParsers {

  import Action._

  lazy val output: Parser[Path] = (shortOpt("o") | longOpt("output")) ~> path

  lazy val prefix: Parser[String] = (shortOpt("p") | longOpt("prefix")) ~> elem

  lazy val TRANSPILE_TO_C: Parser[Action.TranspileToC] = {
    val optOutFile = output ^^ { outFile => (a: TranspileToC) => a.copy(outFile = Some(outFile)) }
    val optPrefix = prefix ^^ { prefix => (a: TranspileToC) => a.copy(prefix = a.prefix :+ prefix) }

    "transpile-to-c" ~> commit(rep(optOutFile | optPrefix) ~ path) ^^ { case opts ~ file => applyOpts(TranspileToC(file), opts) } described "transpile-to-c action"
  }

  lazy val COMPILE: Parser[Action.Compile] = {
    val optOutFile = output ^^ { outFile => (a: Compile) => a.copy(outFile = Some(outFile)) }

    "compile" ~> commit(rep(optOutFile) ~ path) ^^ { case opts ~ file => applyOpts(Compile(file), opts) } described "compile action"
  }

  lazy val HELP: Parser[Action] = {
    "help" ^^ (_ => Help)
  }

  lazy val ACTION: Parser[Action] = (TRANSPILE_TO_C | COMPILE | HELP) described "action"

  def parseArgs(args: Seq[String]): Action = parse(ACTION <~ EOI, SeqReader(args)) match {
    case Accept(action, _, _) => action
    case Reject(expectation, reminding, _) => throw new CliException(s"Cli error: " + formatErrorMessage(expectation, reminding))
  }
}
