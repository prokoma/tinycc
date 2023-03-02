package tinycc.cli

import tinycc.util.parsing.SourceLocation
import tinycc.util.parsing.combinator.{Parsers, Reader}

import java.nio.file.Path
import scala.language.implicitConversions
import scala.util.Try

case class SeqReader[T](seq: Seq[T]) extends Reader[T] {
  override def headOption: Option[T] = seq.headOption

  override def isEmpty: Boolean = seq.isEmpty

  override def tail: SeqReader[T] = SeqReader(seq.tail)

  override def loc: SourceLocation = SourceLocation(0, 0, 0)

  override def iterator: Iterator[T] = seq.iterator
}

object CliParser extends Parsers {

  import Action._

  type Input = SeqReader[Elem]
  type Elem = String

  def unexpectedEndOfInput(in: Input): Reject = Reject("unexpected end of input", in)

  lazy val elem: Parser[Elem] = in => in.headOption match {
    case None => unexpectedEndOfInput(in)
    case Some(tok) => Accept(tok, in.tail)
  }

  def longOpt(name: String): Parser[Elem] = in => in.headOption match {
    case None => unexpectedEndOfInput(in)
    case Some(tok) if tok == s"--$name" => Accept(tok, in.tail)
    case Some(tok) if tok.startsWith(s"--$name=") => Accept(s"--$name=", SeqReader(tok.substring(s"--$name=".length) +: in.seq.tail))
    case Some(tok) => Reject(s"unexpected $tok", in)
  }

  lazy val EOF: Parser[Unit] = in => if (in.isEmpty) Accept((), in) else Reject("expected end of input", in)

  implicit def stringParser(s: String): Parser[String] = elem.filter(_ == s).label(s)

  lazy val file: Parser[Path] = elem.map(arg => Try(Path.of(arg)))
    .filter(_.isSuccess)
    .map(_.get).label("valid file")

  type OptParser[T] = Parser[T => T]

  def applyOpts[T](action: T, opts: Seq[T => T]): T = opts.foldLeft(action)((action, opt) => opt(action))

  lazy val outputFile: Parser[Path] = ("-o" | longOpt("output")) ~> file

  lazy val prefix: Parser[String] = longOpt("prefix") ~> elem

  lazy val TRANSPILE_TO_C: Parser[Action.TranspileToC] = "transpile-to-c" ~> rep(
    (outputFile ^^ { outFile => (a: TranspileToC) => a.copy(outFile = Some(outFile)) }) |
      (prefix ^^ { prefix => (a: TranspileToC) => a.copy(prefix = a.prefix :+ prefix) })
  ) ~ file ^^ { case opts ~ file => applyOpts(TranspileToC(file), opts) }

  lazy val COMPILE: Parser[Action.Compile] = "compile" ~> rep(
    outputFile ^^ { outFile => (a: Compile) => a.copy(outFile = Some(outFile)) }
  ) ~ file ^^ { case opts ~ file => applyOpts(Compile(file), opts) }

  lazy val ACTION: Parser[Action] = (
    TRANSPILE_TO_C | COMPILE
    ) | failure("unknown action")

  def parseArgs(args: Seq[String]): Action = (ACTION <~ EOF)(SeqReader(args)) match {
    case Accept(action, _) => action
    case Reject(msg, _, _) => throw new RuntimeException(msg)
  }

}
