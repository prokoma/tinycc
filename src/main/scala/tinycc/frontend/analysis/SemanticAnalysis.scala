package tinycc.frontend.analysis

import tinycc.frontend.Declarations
import tinycc.frontend.ast._
import tinycc.util.parsing.SourceLocation

import scala.collection.mutable

class SemanticAnalysisException(message: String, loc: SourceLocation) extends RuntimeException(message) {
//  override def format(reporter: Reporter): String = reporter.formatError("semantic analysis", message, loc)
}

object SemanticAnalysisException {
  def errorUndeclaredIdentifier(id: AstIdentifier, loc: SourceLocation): SemanticAnalysisException =
    new SemanticAnalysisException(s"Use of undeclared identifier '${id.value}'.", loc)

  def errorVarRedefinition(id: Symbol, prevDecl: IdentifierDecl, loc: SourceLocation): SemanticAnalysisException =
    new SemanticAnalysisException(s"Redefinition of variable '$id' (previously declared at ${prevDecl.loc})'", loc)

  def errorArgRedefinition(name: Symbol, funDecl: AstFunDecl, loc: SourceLocation): SemanticAnalysisException =
    new SemanticAnalysisException(s"Redefinition of argument '$name'", loc)

  def errorAssignToFun(id: AstIdentifier, loc: SourceLocation): SemanticAnalysisException =
    new SemanticAnalysisException(s"Cannot assign to a function (use & to get function address).", loc)

  def errorCaseRedefinition(value: Long, node: AstSwitch, loc: SourceLocation): SemanticAnalysisException =
    new SemanticAnalysisException(s"Redefinition of case value $value", loc)
}

/**
 * 1. Link {AstIdentifier}s with their {IdentifierDecls}
 * 2. Check for duplicate member names in struct
 * 3. Check for duplicate argument names
 * 4. Check for variable redefinition
 * */
class SemanticAnalysis(program: AstBlock) {

  import SemanticAnalysisException._;

  implicit private val declarations: mutable.Map[AstIdentifier, IdentifierDecl] = mutable.Map.empty

  private var lexicalStack: List[mutable.Map[Symbol, IdentifierDecl]] = Nil

  private def withLexicalFrame[R](thunk: => R): R = {
    lexicalStack ::= mutable.Map.empty
    try
      thunk
    finally
      lexicalStack = lexicalStack.tail
  }

  private def lookupDecl(idName: Symbol): Option[IdentifierDecl] =
    lexicalStack.collectFirst(Function.unlift(_.get(idName)))

  lazy val result: Declarations = {
    withLexicalFrame({
      program.accept(new Visitor_())
    })
    declarations
  }

  private class Visitor_ extends AstVisitor[Unit] {
    override def visit(node: AstNode): Unit = {}

    override def visitIdentifier(node: AstIdentifier): Unit = {
      declarations(node) = lookupDecl(node.value).getOrElse(throw errorUndeclaredIdentifier(node))
    }

    override def visitType(node: AstType): Unit = super.visitType(node)

    override def visitPointerType(node: AstPointerType): Unit = {
      node.base.accept(this)
    }

    override def visitArrayType(node: AstArrayType): Unit = {
      node.base.accept(this)
    }

    override def visitNamedType(node: AstNamedType): Unit = super.visitNamedType(node) // TODO: check if type is declared

    override def visitSequence(node: AstSequence): Unit = {
      node.body.foreach(_.accept(this))
    }

    override def visitBlock(node: AstBlock): Unit = {
      withLexicalFrame({
        node.body.foreach(_.accept(this))
      })
    }

    override def visitDecl(node: AstDecl): Unit = super.visitDecl(node)

    override def visitVarDecl(node: AstVarDecl): Unit = {
      node.value.foreach(_.accept(this))

      lexicalStack.head.get(node.name).foreach(prevDecl =>
        throw errorVarRedefinition(node.name, prevDecl, node.loc))
      lexicalStack.head(node.name) = VarDecl(node)
    }

    override def visitFunDecl(node: AstFunDecl): Unit = {
      if(!lexicalStack.head.contains(node.name))
        lexicalStack.head(node.name) = FunDecl(node)

      withLexicalFrame({
        val argNames = mutable.Set.empty[Symbol]
        node.args.zipWithIndex.foreach({ case ((_, argName), idx) =>
          if(argNames.contains(argName))
            throw errorArgRedefinition(argName, node, node.loc)
          argNames += argName
          lexicalStack.head(argName) = FunArgDecl(node, idx, node.loc)
        })
        node.body.foreach(_.accept(this))
      })
    }

    override def visitStructDecl(node: AstStructDecl): Unit = super.visitStructDecl(node) // TODO: where should named types be handled?

    override def visitFunPtrDecl(node: AstFunPtrDecl): Unit = super.visitFunPtrDecl(node)

    override def visitIf(node: AstIf): Unit = {
      node.guard.accept(this)
      node.trueCase.accept(this)
      node.falseCase.foreach(_.accept(this))
    }

    override def visitSwitch(node: AstSwitch): Unit = {
      val caseValues = mutable.Set.empty[Long]
      node.cases.foreach({ case (value, body) =>
        if(caseValues.contains(value))
          throw errorCaseRedefinition(value, node, body.loc)
        caseValues += value
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
      withLexicalFrame({
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

case class FunDecl(node: AstFunDecl) extends IdentifierDecl {
  override def loc: SourceLocation = node.loc
}

case class FunArgDecl(fun: AstFunDecl, index: Int, loc: SourceLocation) extends IdentifierDecl

case class VarDecl(node: AstVarDecl) extends IdentifierDecl {
  override def loc: SourceLocation = node.loc
}