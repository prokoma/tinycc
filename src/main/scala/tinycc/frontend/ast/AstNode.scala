package tinycc.frontend.ast

import tinycc.util.parsing.SourceLocation

trait AstNode {
  def loc: SourceLocation

  def accept[R](vis: AstVisitor[R]): R
}
