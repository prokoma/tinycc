package tinycc.util.parsing.combinator

import java.nio.file.Path
import scala.util.Try

trait CliParsers extends SeqParsers[String] {
  override def elemToString(e: String): String = s"\"$e\""

  def shortOpt(name: String): Parser[String] = elem(s"-$name")

  def longOpt(name: String): Parser[String] = in => in.headOption match {
    case Some(tok) if tok == s"--$name" => Accept(tok, in.tail)
    case Some(tok) if tok.startsWith(s"--$name=") => Accept(s"--$name=", SeqReader(tok.substring(s"--$name=".length) +: in.seq.tail))
    case _ => Reject(List(ExpTree(s"'--$name'"), ExpTree(s"'--$name=VALUE'")), in)
  }

  lazy val path: Parser[Path] = elem.map(arg => Try(Path.of(arg)))
    .filter(_.isSuccess).map(_.get) described "path"

  type OptParser[T] = Parser[T => T]

  def applyOpts[T](action: T, opts: Seq[T => T]): T = opts.foldLeft(action)((action, opt) => opt(action))
}