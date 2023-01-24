package tinycc

import tinycc.frontend.analysis.IdentifierDecl
import tinycc.frontend.ast.{AstIdentifier, AstNode}

package object frontend {
//  type TypeMap = collection.Map[AstNode, Ty]
  type Declarations = collection.Map[AstIdentifier, IdentifierDecl]

  implicit class ASTTypeMapExt(that: AstNode) {
//    def ty(implicit typeMap: TypeMap): Ty = typeMap(that)
  }

  implicit class ASTDeclarationsExt(that: AstIdentifier) {
    def decl(implicit declarations: Declarations): IdentifierDecl = declarations(that)
  }
}
