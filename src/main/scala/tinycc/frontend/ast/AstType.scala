package tinycc.frontend.ast

sealed trait AstType extends AstNode {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitType(this)
}

class AstPointerType(val base: AstType, val loc: SourceLocation) extends AstType {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitPointerType(this)
}

class AstArrayType(val base: AstType, val loc: SourceLocation) extends AstType {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitArrayType(this)
}

class AstNamedType(val name: Symbol, val loc: SourceLocation) extends AstType {
  override def accept[R](vis: AstVisitor[R]): R = vis.visitNamedType(this)
}