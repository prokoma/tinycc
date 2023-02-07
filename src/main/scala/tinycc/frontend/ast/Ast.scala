package tinycc.frontend.ast

import tinycc.util.parsing.SourceLocation

sealed trait AstNode {
  def loc: SourceLocation

  def children: Seq[AstNode] = Seq.empty
}

sealed trait AstType extends AstNode

class AstPointerType(val base: AstType, val loc: SourceLocation) extends AstType {
  override def children: Seq[AstNode] = Seq(base)
}

class AstArrayType(val base: AstType, val loc: SourceLocation) extends AstType {
  override def children: Seq[AstNode] = Seq(base)
}

class AstNamedType(val symbol: Symbol, val loc: SourceLocation) extends AstType

class AstInteger(val value: Long, val loc: SourceLocation) extends AstNode

class AstDouble(val value: Double, val loc: SourceLocation) extends AstNode

class AstChar(val value: Char, val loc: SourceLocation) extends AstNode

class AstString(val value: String, val loc: SourceLocation) extends AstNode

sealed trait AstIdentifierOrDecl extends AstNode {
  def symbol: Symbol
}

class AstIdentifier(val symbol: Symbol, val loc: SourceLocation) extends AstNode with AstIdentifierOrDecl

class AstSequence(val body: Seq[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = body
}

class AstBlock(val body: Seq[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = body
}

sealed trait AstDecl extends AstNode with AstIdentifierOrDecl {
  def symbol: Symbol
}

class AstVarDecl(val symbol: Symbol, val varTy: AstType, val value: Option[AstNode], val loc: SourceLocation) extends AstDecl {
  override def children: Seq[AstNode] = Seq(varTy) ++ value
}

class AstFunDecl(val symbol: Symbol, val returnTy: AstType, val args: Seq[(AstType, Symbol)], val body: Option[AstNode], val loc: SourceLocation) extends AstDecl {
  override def children: Seq[AstNode] = Seq(returnTy) ++ args.map(_._1) ++ body

  def hasBody: Boolean = body.isDefined
}

sealed trait AstNamedTypeDecl extends AstDecl

class AstStructDecl(val symbol: Symbol, val fields: Option[Seq[(AstType, Symbol)]], val loc: SourceLocation) extends AstNamedTypeDecl {
  override def children: Seq[AstNode] = Seq.from(fields.flatMap(_.map(_._1))

  def hasFields: Boolean = fields.isDefined
}

class AstFunPtrDecl(val symbol: Symbol, val returnTy: AstType, val argTys: Seq[AstType], val loc: SourceLocation) extends AstNamedTypeDecl {
  override def children: Seq[AstNode] = Seq(returnTy) ++ argTys
}

class AstIf(val guard: AstNode, val trueCase: AstNode, val falseCase: Option[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(guard, trueCase) ++ falseCase
}

class AstSwitch(val guard: AstNode, val cases: Seq[(Long, AstNode)], val defaultCase: Option[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(guard) ++ cases.map(_._2) ++ defaultCase
}

class AstWhile(val guard: AstNode, val body: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(guard, body)
}

class AstDoWhile(val guard: AstNode, val body: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(guard, body)
}

class AstFor(val init: Option[AstNode], val guard: Option[AstNode], val increment: Option[AstNode], val body: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq.from(init ++ guard ++ increment ++ Seq(body))
}

class AstBreak(val loc: SourceLocation) extends AstNode

class AstContinue(val loc: SourceLocation) extends AstNode

class AstReturn(val expr: Option[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq.from(expr)
}

class AstBinaryOp(val op: Symbol, val left: AstNode, val right: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(left, right)
}

class AstAssignment(val lvalue: AstNode, val value: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(lvalue, value)
}

class AstUnaryOp(val op: Symbol, val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstUnaryPostOp(val op: Symbol, val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstAddress(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstDeref(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstIndex(val base: AstNode, val index: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(base, index)
}

class AstMember(val base: AstNode, val member: Symbol, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(base)
}

class AstMemberPtr(val base: AstNode, val member: Symbol, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(base)
}

class AstCall(val expr: AstNode, val args: Seq[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr) ++ args
}

class AstCast(val expr: AstNode, val newTy: AstType, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr, newTy)
}

class AstWrite(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstRead(val loc: SourceLocation) extends AstNode

