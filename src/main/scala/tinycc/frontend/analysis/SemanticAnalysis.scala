package tinycc.frontend.analysis

import tinycc.frontend.Declarations
import tinycc.frontend.ast._
import tinycc.util.{ErrorLevel, Reporter}
import tinycc.util.parsing.SourceLocation
import tinycc.ProgramException

import scala.collection.mutable

class SemanticAnalysisException(val messages: Seq[SemanticAnalysisException.Message]) extends ProgramException(messages.map(_.message).mkString("\n")) {
  override def format(reporter: Reporter): String = messages.map(_.format(reporter)).mkString("\n")
}

object SemanticAnalysisException {
  class Message(val level: ErrorLevel, message: String, val loc: SourceLocation) extends ProgramException("semantic analysis: " + message) {
    override def format(reporter: Reporter): String = reporter.formatError(level, getMessage, loc)
  }
}

final class LexicalStack {
  private var frames: List[mutable.Map[Symbol, IdentifierDecl]] = Nil

  def withFrame[R](thunk: => R): R = {
    frames ::= mutable.Map.empty
    try
      thunk
    finally
      frames = frames.tail
  }

  def isGlobalFrame: Boolean = frames.sizeIs == 1

  def lookupDecl(identifier: Symbol): Option[IdentifierDecl] =
    frames.collectFirst(Function.unlift(_.get(identifier)))

  def lookupDeclInCurrentFrame(identifier: Symbol): Option[IdentifierDecl] =
    frames.headOption.flatMap(_.get(identifier))

  def putDecl(identifier: Symbol, decl: IdentifierDecl): Unit =
    frames.head(identifier) = decl
}

/** Semantic analysis maps variable and function identifiers to theirs respective declarations. Because global variables
 * and functions can have multiple declarations (and one definition), previous declarations can be accessed through
 * prevDecl (linked list). */
final class SemanticAnalysis(program: AstProgram) {

  import ErrorLevel._
  import IdentifierDecl._
  import SemanticAnalysisException.Message

  implicit private val declarations: mutable.Map[AstIdentifierOrDecl, IdentifierDecl] = mutable.Map.empty

  private val lexicalStack: LexicalStack = new LexicalStack

  private val errors: mutable.Buffer[Message] = mutable.Buffer.empty

  def result(): Declarations = {
    visit(program) // First frame is created by visitBlock.
    if (errors.nonEmpty)
      throw new SemanticAnalysisException(errors.toSeq)
    declarations
  }

  private def visit(node: AstNode): Unit = node match {
    case node: AstIdentifier =>
      lexicalStack.lookupDecl(node.symbol) match {
        case Some(decl) =>
          declarations(node) = decl
        case None =>
          errors += new Message(Error, s"identifier '${node.symbol.name}' undeclared", node.loc)
      }

    case node: AstProgram =>
      lexicalStack.withFrame({
        node.body.foreach(visit)
      })

    case node: AstBlock =>
      lexicalStack.withFrame({
        node.body.foreach(visit)
      })

    case node: AstVarDecl =>
      node.value.foreach(visit)

      lexicalStack.lookupDeclInCurrentFrame(node.symbol) match {
        case Some(prevDecl: VarDecl) if lexicalStack.isGlobalFrame =>
          // Ok, forward declarations of variables are allowed.
          declarations(node) = prevDecl
          lexicalStack.putDecl(node.symbol, VarDecl(node))

        case Some(prevDecl: VarDecl) =>
          errors += new Message(Error, s"local variable '${node.symbol.name}' redeclared", node.loc)
          errors += new Message(Note, s"previous declaration of '${node.symbol.name}' here", prevDecl.loc)

        case Some(prevDecl) =>
          errors += new Message(Error, s"'${node.symbol.name}' redeclared as different kind of symbol", node.loc)
          errors += new Message(Note, s"previous declaration of '${node.symbol.name}' here", prevDecl.loc)
        case None =>
          lexicalStack.putDecl(node.symbol, VarDecl(node))
      }

    case node: AstFunDecl =>
      lexicalStack.lookupDeclInCurrentFrame(node.symbol) match {
        case Some(prevDecl: FunDecl) =>
          // Ok, forward declarations of functions are allowed.
          declarations(node) = prevDecl
          lexicalStack.putDecl(node.symbol, FunDecl(node))

        case Some(prevDecl) =>
          errors += new Message(Error, s"'${node.symbol.name}' redeclared as different kind of symbol", node.loc)
          errors += new Message(Note, s"previous declaration of '${node.symbol.name}' here", prevDecl.loc)
        case None =>
          lexicalStack.putDecl(node.symbol, FunDecl(node))
      }

      lexicalStack.withFrame({
        node.args.zipWithIndex.foreach({ case ((_, argName), idx) =>
          lexicalStack.lookupDeclInCurrentFrame(argName) match {
            case Some(_) =>
              errors += new Message(Error, s"duplicate argument '${argName.name}'", node.loc)

            case None =>
              lexicalStack.putDecl(argName, FunArgDecl(node, idx))
          }
        })
        node.body.foreach(visit)
      })

    case node: AstSwitch =>
      val caseValues = mutable.Set.empty[Long]
      node.cases.foreach({ case (value, body) =>
        if (!caseValues.add(value))
          errors += new Message(Error, s"duplicate case value '$value'", body.loc)
        visit(body)
      })
      node.defaultCase.foreach(visit)

    case node: AstFor =>
      lexicalStack.withFrame({
        node.init.foreach(visit)
        node.guard.foreach(visit)
        node.increment.foreach(visit)
        visit(node.body)
      })

    case node: AstAssignment =>
      visit(node.lvalue)
      node.lvalue match {
        case id: AstIdentifier => id.declOption match {
          case Some(_: FunDecl) =>
            errors += new Message(Error, s"assignment to function '${id.symbol.name}'", node.loc)

          case _ =>
          // Undeclared identifier is already handled by visitIdentifier.
        }

        case _ =>
      }
      visit(node.value)

    case node =>
      node.children.foreach(visit)
  }
}

sealed trait IdentifierDecl extends Product with Serializable {
  def loc: SourceLocation
}

object IdentifierDecl {
  case class FunArgDecl(node: AstFunDecl, index: Int) extends IdentifierDecl {
    override def loc: SourceLocation = node.loc
  }

  case class FunDecl(node: AstFunDecl) extends IdentifierDecl {
    def prevDecl(implicit declarations: Declarations): Option[FunDecl] = node.prevDecl

    override def loc: SourceLocation = node.loc
  }

  case class VarDecl(node: AstVarDecl) extends IdentifierDecl {
    def prevDecl(implicit declarations: Declarations): Option[VarDecl] = node.prevDecl

    override def loc: SourceLocation = node.loc
  }
}