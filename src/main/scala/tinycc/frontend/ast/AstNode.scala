package tinycc.frontend.ast

trait AstNode {
  def loc: SourceLocation

  def accept[R](vis: AstVisitor[R]): R
}
