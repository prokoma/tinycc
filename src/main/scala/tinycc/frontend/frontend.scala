package tinycc

import tinycc.frontend.Types.Ty
import tinycc.frontend.analysis.IdentifierDecl
import tinycc.frontend.analysis.IdentifierDecl.{FunDecl, VarDecl}
import tinycc.frontend.ast._

package object frontend {
  type TypeMap = Map[AstNode, Ty]
  type Declarations = Map[AstIdentifierOrDecl, IdentifierDecl]

  implicit class AstTypeAccess(that: AstNode) {
    def ty(implicit typeMap: TypeMap): Ty = typeMap(that)
  }

  implicit class AstIdentifierDeclAccess(that: AstIdentifier) {
    def decl(implicit declarations: Declarations): IdentifierDecl = declarations(that)

    def declOption(implicit declarations: Declarations): Option[IdentifierDecl] = declarations.get(that)
  }

  implicit class AstPrevVarDeclAccess(that: AstVarDecl) {
    def prevDecl(implicit declarations: Declarations): Option[VarDecl] = declarations.get(that).map(_.asInstanceOf[VarDecl])

    def prevDecls(implicit declarations: Declarations): Seq[VarDecl] = Seq.unfold(prevDecl)(_.map(d => (d, d.prevDecl)))
  }

  implicit class AstPrevFunDeclAccess(that: AstFunDecl) {
    def prevDecl(implicit declarations: Declarations): Option[FunDecl] = declarations.get(that).map(_.asInstanceOf[FunDecl])

    def prevDecls(implicit declarations: Declarations): Seq[FunDecl] = Seq.unfold(prevDecl)(_.map(d => (d, d.prevDecl)))
  }
}
