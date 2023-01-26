package tinycc.frontend.analysis

import tinycc.frontend.Declarations
import tinycc.frontend.ast._
import tinycc.util.parsing.SourceLocation

import scala.collection.mutable

class SemanticAnalysisException(message: String, loc: SourceLocation) extends RuntimeException(message) {
  //  override def format(reporter: Reporter): String = reporter.formatError("semantic analysis", message, loc)
}

class LexicalStack {
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

class SemanticAnalysis(program: AstBlock) {

  implicit private val declarations: mutable.Map[AstIdentifierOrDecl, IdentifierDecl] = mutable.Map.empty

  private val lexicalStack: LexicalStack = new LexicalStack

  private val errors: mutable.Buffer[SemanticAnalysisException] = mutable.Buffer.empty

  lazy val result: Either[Seq[SemanticAnalysisException], Declarations] = {
    program.accept(new Visitor_) // First frame is created by visitBlock.
    if (errors.nonEmpty)
      Left(errors.toSeq)
    else
      Right(declarations)
  }

  private class Visitor_ extends AstVisitor[Unit] {
    override def visit(node: AstNode): Unit = {}

    override def visitIdentifier(node: AstIdentifier): Unit = {
      lexicalStack.lookupDecl(node.symbol) match {
        case Some(decl) =>
          declarations(node) = decl
        case None =>
          errors += new SemanticAnalysisException(s"identifier '${node.symbol.name}' undeclared", node.loc)
      }
    }

    override def visitSequence(node: AstSequence): Unit = {
      node.body.foreach(_.accept(this))
    }

    override def visitBlock(node: AstBlock): Unit = {
      lexicalStack.withFrame({
        node.body.foreach(_.accept(this))
      })
    }

    override def visitDecl(node: AstDecl): Unit = super.visitDecl(node)

    override def visitVarDecl(node: AstVarDecl): Unit = {
      node.value.foreach(_.accept(this))

      lexicalStack.lookupDeclInCurrentFrame(node.symbol) match {
        case Some(prevDecl: VarDecl) if lexicalStack.isGlobalFrame =>
          // Ok, forward declarations of variables are allowed.
          declarations(node) = prevDecl
          lexicalStack.putDecl(node.symbol, VarDecl(node, Some(prevDecl)))

        case Some(prevDecl: VarDecl) =>
          errors += new SemanticAnalysisException(s"local variable '${node.symbol.name}' redeclared", node.loc)
          errors += new SemanticAnalysisException(s"previous declaration of '${node.symbol.name}' here", prevDecl.loc)

        case Some(prevDecl) =>
          errors += new SemanticAnalysisException(s"'${node.symbol.name}' redeclared as different kind of symbol", node.loc)
          errors += new SemanticAnalysisException(s"previous declaration of '${node.symbol.name}' here", prevDecl.loc)
        case None =>
          lexicalStack.putDecl(node.symbol, VarDecl(node))
      }
    }

    override def visitFunDecl(node: AstFunDecl): Unit = {
      lexicalStack.lookupDeclInCurrentFrame(node.symbol) match {
        case Some(prevDecl: FunDecl) =>
          // Ok, forward declarations of functions are allowed.
          declarations(node) = prevDecl
          lexicalStack.putDecl(node.symbol, FunDecl(node, Some(prevDecl)))

        case Some(prevDecl) =>
          errors += new SemanticAnalysisException(s"'${node.symbol.name}' redeclared as different kind of symbol", node.loc)
          errors += new SemanticAnalysisException(s"previous declaration of '${node.symbol.name}' here", prevDecl.loc)
        case None =>
          lexicalStack.putDecl(node.symbol, FunDecl(node))
      }

      lexicalStack.withFrame({
        node.args.zipWithIndex.foreach({ case ((_, argName), idx) =>
          lexicalStack.lookupDeclInCurrentFrame(argName) match {
            case Some(_) =>
              errors += new SemanticAnalysisException(s"duplicate argument '${argName.name}'", node.loc)

            case None =>
              lexicalStack.putDecl(argName, FunArgDecl(node, idx))
          }
        })
        node.body.foreach(_.accept(this))
      })
    }

    override def visitIf(node: AstIf): Unit = {
      node.guard.accept(this)
      node.trueCase.accept(this)
      node.falseCase.foreach(_.accept(this))
    }

    override def visitSwitch(node: AstSwitch): Unit = {
      val caseValues = mutable.Set.empty[Long]
      node.cases.foreach({ case (value, body) =>
        if (!caseValues.add(value))
          errors += new SemanticAnalysisException(s"duplicate case value '$value'", body.loc)
        body.accept(this)
      })
      node.defaultCase.foreach(_.accept(this))
    }

    override def visitWhile(node: AstWhile): Unit = {
      node.guard.accept(this)
      node.body.accept(this)
    }

    override def visitDoWhile(node: AstDoWhile): Unit = {
      node.body.accept(this)
      node.guard.accept(this)
    }

    override def visitFor(node: AstFor): Unit = {
      lexicalStack.withFrame({
        node.init.foreach(_.accept(this))
        node.guard.foreach(_.accept(this))
        node.increment.foreach(_.accept(this))
        node.body.accept(this)
      })
    }

    override def visitReturn(node: AstReturn): Unit = {
      node.expr.foreach(_.accept(this))
    }

    override def visitBinaryOp(node: AstBinaryOp): Unit = {
      node.left.accept(this)
      node.right.accept(this)
    }

    override def visitAssignment(node: AstAssignment): Unit = {
      node.lvalue.accept(this)

      node.lvalue match {
        case id: AstIdentifier => id.declOption match {
          case Some(_: FunDecl) =>
            errors += new SemanticAnalysisException(s"assignment to function '${id.symbol.name}'", node.loc)
          case _ =>
          // Undeclared identifier is already handled by visitIdentifier.
        }
      }

      node.value.accept(this)
    }

    override def visitUnaryOp(node: AstUnaryOp): Unit = {
      node.expr.accept(this)
    }

    override def visitUnaryPostOp(node: AstUnaryPostOp): Unit = {
      node.expr.accept(this)
    }

    override def visitAddress(node: AstAddress): Unit = {
      node.expr.accept(this)
    }

    override def visitDeref(node: AstDeref): Unit = {
      node.expr.accept(this)
    }

    override def visitIndex(node: AstIndex): Unit = {
      node.base.accept(this)
      node.index.accept(this)
    }

    override def visitMember(node: AstMember): Unit = {
      node.base.accept(this)
    }

    override def visitMemberPtr(node: AstMemberPtr): Unit = {
      node.base.accept(this)
    }

    override def visitCall(node: AstCall): Unit = {
      node.expr.accept(this)
      node.args.foreach(_.accept(this))
    }

    override def visitCast(node: AstCast): Unit = {
      node.newTy.accept(this)
      node.expr.accept(this)
    }

    override def visitWrite(node: AstWrite): Unit = {
      node.expr.accept(this)
    }
  }
}

sealed trait IdentifierDecl {
  def loc: SourceLocation
}

case class FunArgDecl(node: AstFunDecl, index: Int) extends IdentifierDecl {
  override def loc: SourceLocation = node.loc
}

//trait ForwardDeclarable[T <: ForwardDeclarable[T]] extends IdentifierDecl with Iterable[T] {
//  def prevDecl: Option[T]
//
//  def iterator: Iterator[T] =
//    Iterator.unfold[T, Option[T]](Some(this.asInstanceOf[T]))(_.map(decl => (decl, decl.prevDecl)))
//}

case class FunDecl(node: AstFunDecl) extends IdentifierDecl {
  def prevDecl(implicit declarations: Declarations): Option[FunDecl] = node.prevDecl

  override def loc: SourceLocation = node.loc
}

case class VarDecl(node: AstVarDecl) extends IdentifierDecl {
  def prevDecl(implicit declarations: Declarations): Option[VarDecl] = node.prevDecl

  override def loc: SourceLocation = node.loc
}