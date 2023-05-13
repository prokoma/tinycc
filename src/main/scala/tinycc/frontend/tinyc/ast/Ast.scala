package tinycc.frontend.tinyc.ast

import tinycc.util.parsing.SourceLocation

sealed abstract class AstNode {
  def loc: SourceLocation

  def children: Seq[AstNode] = Seq.empty

  override def toString: String = AstNode.printer.printToString(this) + s"[$loc]"
}

object AstNode {
  private val printer = new AstPrinter
}

sealed trait AstType extends AstNode

class AstPointerType(val base: AstType, val loc: SourceLocation) extends AstType {
  override def children: Seq[AstNode] = Seq(base)
}

class AstArrayType(val base: AstType, val size: AstNode, val loc: SourceLocation) extends AstType {
  override def children: Seq[AstNode] = Seq(base, size)
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

class AstProgram(val body: Seq[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = body
}

sealed trait AstDecl extends AstNode with AstIdentifierOrDecl {
  def symbol: Symbol
}

class AstVarDecl(val symbol: Symbol, val varTy: AstType, val value: Option[AstNode], val loc: SourceLocation) extends AstDecl {
  override def children: Seq[AstNode] = Seq(varTy) ++ value

  def hasValue: Boolean = value.isDefined
}

class AstFunDecl(val symbol: Symbol, val returnTy: AstType, val args: Seq[(AstType, Symbol)], val body: Option[AstNode], val loc: SourceLocation) extends AstDecl {
  override def children: Seq[AstNode] = Seq(returnTy) ++ args.map(_._1) ++ body

  def hasBody: Boolean = body.isDefined
}

sealed trait AstNamedTypeDecl extends AstDecl

class AstStructDecl(val symbol: Symbol, val fields: Option[Seq[(AstType, Symbol)]], val loc: SourceLocation) extends AstNamedTypeDecl {
  override def children: Seq[AstNode] = fields.map(_.map(_._1)).getOrElse(Seq.empty)

  def hasFields: Boolean = fields.isDefined
}

class AstFunPtrDecl(val symbol: Symbol, val returnTy: AstType, val argTys: Seq[AstType], val loc: SourceLocation) extends AstNamedTypeDecl {
  override def children: Seq[AstNode] = Seq(returnTy) ++ argTys
}

class AstIf(val guard: AstNode, val trueCase: AstNode, val falseCase: Option[AstNode], val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(guard, trueCase) ++ falseCase
}

class AstSwitch(val guard: AstNode, val cases: Seq[(Long, AstBlock)], val defaultCase: Option[AstBlock], val loc: SourceLocation) extends AstNode {
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

object AstBinaryOp {
  def unapply(node: AstBinaryOp): Option[(Symbol, AstNode, AstNode, SourceLocation)] = Some((node.op, node.left, node.right, node.loc))
}

class AstAssignment(val lvalue: AstNode, val value: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(lvalue, value)
}

class AstUnaryOp(val op: Symbol, val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

object AstUnaryOp {
  def unapply(node: AstUnaryOp): Option[(Symbol, AstNode, SourceLocation)] = Some((node.op, node.expr, node.loc))
}

class AstUnaryPostOp(val op: Symbol, val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

object AstUnaryPostOp {
  def unapply(node: AstUnaryPostOp): Option[(Symbol, AstNode, SourceLocation)] = Some((node.op, node.expr, node.loc))
}

class AstAddress(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstDeref(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstIndex(val expr: AstNode, val index: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr, index)
}

class AstMember(val expr: AstNode, val member: Symbol, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstMemberPtr(val expr: AstNode, val member: Symbol, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
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

class AstWriteNum(val expr: AstNode, val loc: SourceLocation) extends AstNode {
  override def children: Seq[AstNode] = Seq(expr)
}

class AstRead(val loc: SourceLocation) extends AstNode

