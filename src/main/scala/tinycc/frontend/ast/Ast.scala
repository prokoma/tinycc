package tinycc.frontend.ast

import tinycc.frontend.{DoubleLiteral, Identifier, IntLiteral, Operator, StringLiteral}

class AstInteger(val value: Long, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitInteger(this)
}

object AstInteger {
  def apply(tok: IntLiteral, loc: SourceLocation) = new AstInteger(tok.value, loc)
}

class AstDouble(val value: Double, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitDouble(this)
}

object AstDouble {
  def apply(tok: DoubleLiteral, loc: SourceLocation) = new AstDouble(tok.value, loc)
}

class AstChar(val value: Char, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitChar(this)
}

object AstChar {
  def apply(tok: StringLiteral, loc: SourceLocation): AstChar = {
    assert(tok.quote == '\'')
    assert(tok.value.length == 1)
    new AstChar(tok.value(0), loc)
  }
}

class AstString(val value: String, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitString(this)
}

object AstString {
  def apply(tok: StringLiteral, loc: SourceLocation) = new AstString(tok.value, loc)
}

class AstIdentifier(val value: Symbol, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitIdentifier(this)
}

class AstSequence(val body: Seq[AstNode], val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitSequence(this)
}

class AstBlock(val body: Seq[AstNode], val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitBlock(this)
}

trait AstDecl extends AstNode {
  def name: Symbol

  override def accept[R](vis: AstVisitor[R]): R = vis.visitDecl(this)
}

class AstVarDecl(val name: Symbol, val varTy: AstType, val value: Option[AstNode], val loc: SourceLocation) extends AstDecl {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitVarDecl(this)
}

class AstFunDecl(val name: Symbol, val returnTy: AstType, val args: Seq[(AstType, Symbol)], val body: Option[AstNode], val loc: SourceLocation) extends AstDecl {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitFunDecl(this)
}

class AstStructDecl(val name: Symbol, val fields: Seq[(AstType, Symbol)], val loc: SourceLocation) extends AstDecl {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitStructDecl(this)
}

class AstFunPtrDecl(val name: Symbol, val returnTy: AstType, val argTys: Seq[AstType], val loc: SourceLocation) extends AstDecl {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitFunPtrDecl(this)
}

class AstIf(val guard: AstNode, val trueCase: AstNode, val falseCase: Option[AstNode], val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitIf(this)
}

class AstSwitch(val guard: AstNode, val cases: Seq[(Int, AstNode)], val defaultCase: Option[AstNode], val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitSwitch(this)
}

class AstWhile(val guard: AstNode, val body: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitWhile(this)
}

class AstDoWhile(val guard: AstNode, val body: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitDoWhile(this)
}

class AstFor(val init: Option[AstNode], val guard: Option[AstNode], val increment: Option[AstNode], val body: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitFor(this)
}

class AstBreak(val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitBreak(this)
}

class AstContinue(val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitContinue(this)
}

class AstReturn(val expr: Option[AstNode], val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitReturn(this)
}

class AstBinaryOp(val op: Symbol, val left: AstNode, val right: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitBinaryOp(this)
}

class AstAssignment(val lvalue: AstNode, val value: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitAssignment(this)
}

class AstUnaryOp(val op: Symbol, val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitUnaryOp(this)
}

class AstUnaryPostOp(val op: Symbol, val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitUnaryPostOp(this)
}

class AstAddress(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitAddress(this)
}

class AstDeref(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitDeref(this)
}

class AstIndex(val base: AstNode, val index: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitIndex(this)
}

class AstMember(val base: AstNode, val member: Symbol, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitMember(this)
}

class AstMemberPtr(val base: AstNode, val member: Symbol, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitMemberPtr(this)
}

class AstCall(val expr: AstNode, val args: Seq[AstNode], val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitCall(this)
}

class AstCast(val expr: AstNode, val newTy: AstType, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitCast(this)
}

class AstWrite(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitWrite(this)
}

class AstRead(val loc: SourceLocation) extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitRead(this)
}

