package tinycc.cli

import tinycc.util.parsing.SourceLocation
import tinycc.util.parsing.combinator.{Parsers, Reader}

import java.nio.file.Path
import scala.language.implicitConversions
import scala.util.Try

case class SeqReader[T](that: Seq[T]) extends Reader[T] {
  override def headOption: Option[T] = that.headOption

  override def isEmpty: Boolean = that.isEmpty

  override def tail: SeqReader[T] = SeqReader(that.tail)

  override def loc: SourceLocation = ???

  override def iterator: Iterator[T] = that.iterator
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

  lazy val EOF: Parser[Unit] = in => if (in.isEmpty) Accept((), in) else Reject("expected end of input", in)

  implicit def stringParser(s: String): Parser[String] = elem.filter(_ == s).label(s)

  lazy val file: Parser[Path] = elem.map(arg => Try(Path.of(arg)))
    .filter(_.isSuccess)
    .map(_.get).label("valid file")

  lazy val COMPILE: Parser[Action.Compile] = "compile" ~> file ^^ Compile

  lazy val ACTION: Parser[Action] = COMPILE //| failure("unknown action")

  def parseArgs(args: Seq[String]): Action = (ACTION <~ EOF)(SeqReader(args)) match {
    case Accept(action, _) => action
    case Reject(msg, _, _) => throw new RuntimeException(msg)
  }

}
