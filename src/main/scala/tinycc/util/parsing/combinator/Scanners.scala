package tinycc.util.parsing.combinator

import tinycc.util.parsing.{ParserException, SourceLocation}

trait Scanners extends Lexical {
  type Token

  def WHITESPACE: Parser[Any] = rep[Any](whitespace)

  def TOKEN: Parser[Token]

  def handleReject(expected: ExpTree, remainder: Input): Nothing =
    throw new ParserException(formatErrorMessage(expected, remainder), remainder.loc)

  case class Scanner(in: CharReader) extends Reader[Token] {
    def this(in: String) = this(CharReader(in))

    private val (afterWs, tokOption, afterTok) = parse(WHITESPACE, in) match {
      case Accept(_, afterWs, _) if afterWs.isEmpty => (afterWs, None, afterWs)
      case Accept(_, afterWs, _) =>
        parse(TOKEN, afterWs) match {
          case Accept(tok, afterTok, _) => (afterWs, Some(tok), afterTok)
          case Reject(expected, remainder, _) => handleReject(expected, remainder)
        }
      case Reject(expected, remainder, _) => handleReject(expected, remainder)
    }

    override def headOption: Option[Token] = tokOption

    override def tail: Scanner = Scanner(afterTok)

    override def loc: SourceLocation = afterWs.loc

    override def isEmpty: Boolean = in.isEmpty || afterWs.isEmpty
  }
}