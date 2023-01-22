package tinycc.frontend.ast

trait AstVisitor[R] {
  def visit(node: AstNode): R

  def visitInteger(node: AstInteger): R = visit(node)

  def visitDouble(node: AstDouble): R = visit(node)

  def visitChar(node: AstChar): R = visit(node)

  def visitString(node: AstString): R = visit(node)

  def visitIdentifier(node: AstIdentifier): R = visit(node)

  def visitType(node: AstType): R = visit(node)

  def visitPointerType(node: AstPointerType): R = visitType(node)

  def visitArrayType(node: AstArrayType): R = visitType(node)

  def visitNamedType(node: AstNamedType): R = visitType(node)

  def visitSequence(node: AstSequence): R = visit(node)

  def visitBlock(node: AstBlock): R = visit(node)

  def visitDecl(node: AstDecl): R = visit(node)

  def visitVarDecl(node: AstVarDecl): R = visitDecl(node)

  def visitFunDecl(node: AstFunDecl): R = visitDecl(node)

  def visitStructDecl(node: AstStructDecl): R = visitDecl(node)

  def visitFunPtrDecl(node: AstFunPtrDecl): R = visitDecl(node)

  def visitIf(node: AstIf): R = visit(node)

  def visitSwitch(node: AstSwitch): R = visit(node)

  def visitWhile(node: AstWhile): R = visit(node)

  def visitDoWhile(node: AstDoWhile): R = visit(node)

  def visitFor(node: AstFor): R = visit(node)

  def visitBreak(node: AstBreak): R = visit(node)

  def visitContinue(node: AstContinue): R = visit(node)

  def visitReturn(node: AstReturn): R = visit(node)

  def visitBinaryOp(node: AstBinaryOp): R = visit(node)

  def visitAssignment(node: AstAssignment): R = visit(node)

  def visitUnaryOp(node: AstUnaryOp): R = visit(node)

  def visitUnaryPostOp(node: AstUnaryPostOp): R = visit(node)

  def visitAddress(node: AstAddress): R = visit(node)

  def visitDeref(node: AstDeref): R = visit(node)

  def visitIndex(node: AstIndex): R = visit(node)

  def visitMember(node: AstMember): R = visit(node)

  def visitMemberPtr(node: AstMemberPtr): R = visit(node)

  def visitCall(node: AstCall): R = visit(node)

  def visitCast(node: AstCast): R = visit(node)

  def visitWrite(node: AstWrite): R = visit(node)

  def visitRead(node: AstRead): R = visit(node)
}














