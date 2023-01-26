package tinycc.frontend.analysis

import tinycc.frontend.Types.{CharTy, DoubleTy, FunTy, IntTy, PtrTy, StructTy, Ty, VoidTy}
import tinycc.frontend.{Declarations, Symbols, TypeMap}
import tinycc.frontend.ast.{AstAddress, AstArrayType, AstAssignment, AstBinaryOp, AstBlock, AstBreak, AstCall, AstCast, AstChar, AstContinue, AstDecl, AstDeref, AstDoWhile, AstDouble, AstFor, AstFunDecl, AstFunPtrDecl, AstIdentifier, AstIf, AstIndex, AstInteger, AstMember, AstMemberPtr, AstNamedType, AstNode, AstPointerType, AstRead, AstReturn, AstSequence, AstString, AstStructDecl, AstSwitch, AstType, AstUnaryOp, AstUnaryPostOp, AstVarDecl, AstVisitor, AstWhile, AstWrite}
import tinycc.util.parsing.SourceLocation

import scala.collection.mutable

class TypeAnalysisException(message: String, loc: SourceLocation) extends RuntimeException(message) {
  //  override def format(reporter: Reporter): String = reporter.formatError("type checker", message, loc)
}

//
//object TypeAnalysisException {
//  def errorExpectedType(expected: Ty, actual: Ty, desc: String, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Expected $expected $desc, but got $actual", loc)
//
//  def errorExpectedPtrType(actual: Ty, desc: String, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Expected pointer $desc, but got $actual", loc)
//
//  def errorExpectedFunType(actual: Ty, desc: String, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Expected function $desc, but got $actual", loc)
//
//  def errorExpectedNumArgs(expected: Int, actual: Int, desc: String, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Expected $expected arguments $desc, $actual given.", loc)
//
//  def errorExpectedLValue(actual: AST, desc: String, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Expected l-value $desc, but got $actual.", loc)
//
//  def errorIncompatibleFunTypes(cur: FunTy, prev: FunTy, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Incompatible function types: $cur, previously declared as $prev.", loc)
//
//  def errorFunRedefinition(cur: ASTFunDecl, prev: ASTFunDecl, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Redefinition of function ${cur.nameString}, previously declared at ${prev.location()}.", loc)
//
//  def errorInvalidCast(from: Ty, to: Ty, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Cannot cast $from to $to.", loc)
//
//  def errorInvalidArraySize(size: AST): TypeAnalysisException =
//    new TypeAnalysisException(s"Invalid array size: $size, only positive integer literals are supported.", size.location())
//
//  def errorExpectedPtrOrArray(actual: Ty, desc: String, loc: SourceLocation): TypeAnalysisException =
//    new TypeAnalysisException(s"Expected pointer or array $desc, but got $actual.", loc)
//}
//
class TypeAnalysis(program: AstBlock, _declarations: Declarations) {
  implicit private def declarations: Declarations = _declarations

  implicit private val typeMap: mutable.Map[AstNode, Ty] = mutable.Map.empty

  private var curFunTyOption: Option[FunTy] = None

  private def curFunTy: FunTy = curFunTyOption.get

  private val funDeclMap: mutable.Map[Symbol, AstFunDecl] = mutable.Map.empty

  private val namedTypeMap: mutable.Map[Symbol, (Ty, SourceLocation)] = mutable.Map.empty

  private val errors: mutable.Buffer[TypeAnalysisException] = mutable.Buffer.empty

  lazy val result: Either[Seq[TypeAnalysisException], TypeMap] = {
    program.accept(new Visitor_)
    if (errors.nonEmpty)
      Left(errors.toSeq)
    else
      Right(typeMap)
  }

//  private def checkAssignableFrom(ty: Ty, other: Ty): Boolean = {
//    if(!ty.isAssignableFrom(other)) {
//      errors += new TypeAnalysisException(s"expected $")
//    }
//  }

  // At the end of each visit method, typeMap(node) must be set.
  private class Visitor_ extends AstVisitor[Unit] {
    private def visitAndGetTy(node: AstNode): Ty = {
      node.accept(this)
      node.ty
    }

    private def visitAndGetTyIfComplete(node: AstNode, label: String): Ty = {
      val ty = visitAndGetTy(node)
      if (!ty.isComplete)
        errors += new TypeAnalysisException(s"cannot use incomplete type $ty in $label", node.loc)
      ty
    }

    private def visitAndCheckGuard(guard: AstNode, label: String): Unit = {
      val guardTy = visitAndGetTy(guard)
      if (!IntTy.isAssignableFrom(guardTy))
        errors += new TypeAnalysisException(s"expected $IntTy (or compatible type) in $label guard, got $guardTy", guard.loc)
    }

    override def visit(node: AstNode): Unit =
      throw new UnsupportedOperationException(s"Type checking for $node is not defined.")

    override def visitInteger(node: AstInteger): Unit = {
      typeMap(node) = IntTy
    }

    override def visitDouble(node: AstDouble): Unit = {
      typeMap(node) = DoubleTy
    }

    override def visitChar(node: AstChar): Unit = {
      typeMap(node) = CharTy
    }

    override def visitString(node: AstString): Unit = {
      typeMap(node) = PtrTy(CharTy)
    }

    override def visitIdentifier(node: AstIdentifier): Unit = {
      typeMap(node) = node.decl match {
        case FunDecl(node) => node.ty
        case FunArgDecl(node, index) => node.ty.asInstanceOf[FunTy].argTys(index)
        case VarDecl(node) => node.ty
      }
    }

    override def visitPointerType(node: AstPointerType): Unit = {
      node.base.accept(this)
      typeMap(node) = PtrTy(node.base.ty)
    }

    override def visitArrayType(node: AstArrayType): Unit = {
      node.base.accept(this)
      typeMap(node) = ???
    }

    override def visitNamedType(node: AstNamedType): Unit = {
      typeMap(node) = node.name match {
        case Symbols.kwVoid => VoidTy
        case Symbols.kwChar => CharTy
        case Symbols.kwInt => IntTy
        case Symbols.kwDouble => DoubleTy
        case name =>
          namedTypeMap.get(name) match {
            case Some((ty, _)) => ty
            case None =>
              errors += new TypeAnalysisException(s"undeclared named type '${name.name}'", node.loc)
              VoidTy // We put VoidTy to the type map as a fallback so accessing type of this node doesn't throw exception.
          }
      }
    }

    override def visitSequence(node: AstSequence): Unit = {
      typeMap(node) = node.body.map(visitAndGetTy).lastOption.getOrElse(VoidTy)
    }

    override def visitBlock(node: AstBlock): Unit = {
      node.body.foreach(_.accept(this))
      typeMap(node) = VoidTy
    }

    override def visitVarDecl(node: AstVarDecl): Unit = {
      val varTy = visitAndGetTyIfComplete(node.varTy, s"declaration of variable '${node.symbol.name}'")

      // Check if the variable is compatible with its previous declaration.
      node.prevDecl match {
        case Some(VarDecl(prevNode)) if prevNode.varTy.ty != varTy =>
          errors += new TypeAnalysisException(s"incompatible declaration of variable '${node.symbol.name}' ($varTy), previously declared as ${varTy.ty}", node.loc)
          errors += new TypeAnalysisException(s"previous declaration of '${node.symbol.name}'", prevNode.loc)

        case _ =>
      }

      // Check for multiple definitions of a variable.
      if (node.value.isDefined) {
        val prevDef = node.prevDecls.collectFirst({ case VarDecl(prevNode) if prevNode.value.isDefined => prevNode })
        prevDef match {
          case Some(prevNode) =>
            errors += new TypeAnalysisException(s"redefinition of variable '${node.symbol.name}' ($varTy)", node.loc)
            errors += new TypeAnalysisException(s"previous definition of '${node.symbol.name}'", prevNode.loc)

          case None =>
        }
      }

      node.value.foreach(value => {
        val valueTy = visitAndGetTy(value)
        if (!varTy.isAssignableFrom(valueTy))
          errors += new TypeAnalysisException(s"cannot assign value of type $valueTy to variable '${node.symbol.name}' of type ${varTy}", value.loc)
      })

      typeMap(node) = varTy
    }

    override def visitFunDecl(node: AstFunDecl): Unit = {
      val ty = FunTy(
        visitAndGetTyIfComplete(node.returnTy, s"definition of '${node.symbol.name}' (return type)"),
        node.args.map({
          case (argTy, argName) =>
            visitAndGetTyIfComplete(argTy, s"definition of '${node.symbol.name}' (arg '$argName')")
        }).toIndexedSeq)

      curFunTyOption = Some(ty)
      typeMap(node) = ty

      funDeclMap.get(node.symbol) match {
        case Some(prevDecl) =>
          val prevTy = prevDecl.ty
          if (prevTy != ty) {
            errors += new TypeAnalysisException(s"incompatible declaration of function '${node.symbol.name}' (${node.ty}), previously declared as ${node.ty}", node.loc)
            errors += new TypeAnalysisException(s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
          }
          if (prevDecl.body.isDefined && node.body.isDefined) {
            errors += new TypeAnalysisException(s"redefinition of function '${node.symbol.name}' (${node.ty})", node.loc)
            errors += new TypeAnalysisException(s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
          }

          if (prevDecl.body.isEmpty)
            funDeclMap(node.symbol) = node // Store the last complete definition of the function, so the check above works.

        case None =>
          funDeclMap(node.symbol) = node
      }

      node.body.foreach(_.accept(this))
    }

    override def visitStructDecl(node: AstStructDecl): Unit = {
      lazy val fields =
        node.fields.map(_.map({ case (astTy, name) =>
          (visitAndGetTyIfComplete(astTy, s"definition of 'struct ${node.symbol.name}'"), name)
        }).toIndexedSeq)

      namedTypeMap.get(node.symbol) match {
        case Some((prevTy: StructTy, _)) if !node.hasFields =>
          // Adding another forward declaration after either full or forward declaration is allowed.
          typeMap(node) = prevTy

        case Some((prevTy: StructTy, prevDeclLoc)) if prevTy.isComplete && node.hasFields =>
          errors += new TypeAnalysisException(s"redefinition of 'struct ${node.symbol.name}'", node.loc)
          errors += new TypeAnalysisException(s"previous definition of 'struct ${node.symbol.name}'", prevDeclLoc)
          typeMap(node) = prevTy

        case Some((prevTy: StructTy, _)) if !prevTy.isComplete && node.hasFields =>
          // Completion of a partially defined struct.
          namedTypeMap(node.symbol) = (prevTy, node.loc)
          prevTy.fields = fields
          typeMap(node) = prevTy

        case Some((_, prevDeclLoc)) =>
          errors += new TypeAnalysisException(s"redefinition of 'struct ${node.symbol.name}' as another kind of type", node.loc)
          errors += new TypeAnalysisException(s"previous declaration of '${node.symbol.name}'", prevDeclLoc)
          typeMap(node) = StructTy(fields)

        case None =>
          val ty = StructTy()
          // Add the forward declaration to the map, so it can be used inside of the struct.
          namedTypeMap(node.symbol) = (ty, node.loc)
          ty.fields = fields
          typeMap(node) = ty
      }
    }

    override def visitFunPtrDecl(node: AstFunPtrDecl): Unit = {
      lazy val ty =
        PtrTy(FunTy(visitAndGetTyIfComplete(node.returnTy, s"definition of 'typedef ${node.symbol.name}' (return type)"),
          node.argTys.zipWithIndex.map({ case (argTy, index) =>
            visitAndGetTyIfComplete(argTy, s"definition of 'typedef ${node.symbol.name}' (arg #${index + 1})")
          }).toIndexedSeq))

      namedTypeMap.get(node.symbol) match {
        case Some((_, prevDeclLoc)) =>
          errors += new TypeAnalysisException(s"redefinition of 'typedef ${node.symbol.name}", node.loc)
          errors += new TypeAnalysisException(s"previous definition of 'typedef ${node.symbol.name}'", prevDeclLoc)
          typeMap(node) = ty

        case None =>
          namedTypeMap(node.symbol) = (ty, node.loc)
          typeMap(node) = ty
      }
    }

    override def visitIf(node: AstIf): Unit = {
      visitAndCheckGuard(node.guard, "if stmt")

      node.trueCase.accept(this)
      node.falseCase.foreach(_.accept(this))
      typeMap(node) = VoidTy
    }

    override def visitSwitch(node: AstSwitch): Unit = {
      visitAndCheckGuard(node.guard, "switch stmt")

      node.cases.foreach(_._2.accept(this))
      typeMap(node) = VoidTy
    }

    override def visitWhile(node: AstWhile): Unit = {
      visitAndCheckGuard(node.guard, "while stmt")
      node.body.accept(this)
      typeMap(node) = VoidTy
    }

    override def visitDoWhile(node: AstDoWhile): Unit = {
      visitAndCheckGuard(node.guard, "do..while stmt")
      node.body.accept(this)
      typeMap(node) = VoidTy
    }

    override def visitFor(node: AstFor): Unit = {
      node.init.foreach(_.accept(this))
      node.guard.foreach(visitAndCheckGuard(_, "for stmt"))
      node.increment.foreach(_.accept(this))
      node.body.accept(this)
      typeMap(node) = VoidTy
    }

    override def visitBreak(node: AstBreak): Unit = {
      typeMap(node) = VoidTy
    }

    override def visitContinue(node: AstContinue): Unit = {
      typeMap(node) = VoidTy
    }

    override def visitReturn(node: AstReturn): Unit = {
      val exprTy = node.expr.map(visitAndGetTy).getOrElse(VoidTy)
      val exprLoc = node.expr.getOrElse(node).loc

      curFunTyOption match {
        case Some(funTy) =>
          if (!funTy.returnTy.isAssignableFrom(exprTy))
            errors += new TypeAnalysisException(s"expected ${funTy.returnTy} (or compatible type), got $exprTy", exprLoc)

        case None =>
          errors += new TypeAnalysisException(s"cannot use return outside of function", node.loc)
      }

      typeMap(node) = VoidTy
    }

    override def visitBinaryOp(node: AstBinaryOp): Unit = super.visitBinaryOp(node)

    override def visitAssignment(node: AstAssignment): Unit = {
      checkLvalue(node.lvalue, "on left side of assignment")
      val lvalueTy = visitAndGetTy(node.lvalue)
      val valueTy = visitAndGetTy(node.value)
      if(!lvalueTy.isAssignableFrom(valueTy))
        errors += new TypeAnalysisException(s"cannot assign $valueTy to l-value of type $lvalueTy", node.loc)
      typeMap(node) = lvalueTy
    }

    override def visitUnaryOp(node: AstUnaryOp): Unit = {
      val exprTy = visitAndGetTy(node.expr)

      node.op match {
        case Symbols.add | Symbols.sub | Symbols.neg =>
          if(!IntTy.isAssignableFrom(exprTy)) {
            errors += new TypeAnalysisException(s"wrong type argument to unary plus (expected int-like type)", node.loc)
            typeMap(node) = IntTy
          } else
            typeMap(node) = exprTy

        case Symbols.not =>
          if(!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy])
            errors += new TypeAnalysisException(s"wrong type argument to unary not (expected int-like or pointer type)", node.loc)
          typeMap(node) = IntTy

        case Symbols.inc | Symbols.dec =>
          checkLvalue(node.expr, s"in unary ${node.op.name}")
          if (!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy]) {
            errors += new TypeAnalysisException(s"wrong type argument to unary not (expected int-like or pointer type)", node.loc)
            typeMap(node) = IntTy
          } else
            typeMap(node) = exprTy

        case op => new UnsupportedOperationException(s"Type checking for unary prefix $op is not defined.")
      }
    }

    override def visitUnaryPostOp(node: AstUnaryPostOp): Unit = {
      val exprTy = visitAndGetTy(node.expr)

      node.op match {
        case Symbols.inc | Symbols.dec =>
          checkLvalue(node.expr, s"in unary ${node.op.name}")
          if (!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy]) {
            errors += new TypeAnalysisException(s"wrong type argument to unary not (expected int-like or pointer type)", node.loc)
            typeMap(node) = IntTy
          } else
            typeMap(node) = exprTy

        case op => new UnsupportedOperationException(s"Type checking for unary postfix $op is not defined.")
      }
    }

    override def visitAddress(node: AstAddress): Unit = super.visitAddress(node)

    override def visitDeref(node: AstDeref): Unit = super.visitDeref(node)

    override def visitIndex(node: AstIndex): Unit = super.visitIndex(node)

    override def visitMember(node: AstMember): Unit = super.visitMember(node)

    override def visitMemberPtr(node: AstMemberPtr): Unit = super.visitMemberPtr(node)

    override def visitCall(node: AstCall): Unit = super.visitCall(node)

    override def visitCast(node: AstCast): Unit = super.visitCast(node)

    override def visitWrite(node: AstWrite): Unit = {
      val exprTy = visitAndGetTy(node.expr)
      if(!CharTy.isAssignableFrom(exprTy))
        errors += new TypeAnalysisException(s"expected $CharTy (or compatible type), got $exprTy", node.expr.loc)
      typeMap(node) = VoidTy
    }

    override def visitRead(node: AstRead): Unit = {
      typeMap(node) = CharTy
    }

  }
}
//
//def analyze(node: AST): TypeMap = {
//  typeMap.clear()
//  funDeclMap.clear()
//  namedTypeMap.clear()
//  visitNode(node)
//  typeMap
//}
//
//private def resolveNodeTy(node: AST): Ty = {
//  if (!typeMap.contains(node))
//    visitNode(node)
//  typeMap(node)
//}
//
//private def visitNode(node: AST): Unit = node.asRealType match {
//  // Types
//
//  case f: ASTFunDecl =>
//    curFunTyOption = Some(FunTy(
//      resolveNodeTy(f.typeDecl),
//      f.args.map(arg => resolveNodeTy(arg._1)).toIndexedSeq))
//    typeMap(node) = curFunTy
//
//    funDeclMap.get(f.nameString) match {
//      case Some(prevDecl) =>
//        val prevTy = resolveNodeTy(prevDecl).asInstanceOf[FunTy]
//        if (prevTy != curFunTy)
//          throw errorIncompatibleFunTypes(curFunTy, prevTy, f.location())
//        if (prevDecl.body.isDefined && f.body.isDefined)
//          throw errorFunRedefinition(f, prevDecl, f.location())
//        if (prevDecl.body.isEmpty)
//          funDeclMap(f.nameString) = f
//
//      case None =>
//        funDeclMap(f.nameString) = f
//    }
//
//    if (f.body.isDefined)
//      f.body.foreach(visitNode)
//
//  case nt: ASTNamedType =>
//    typeMap(node) = nt.getName match {
//      case Symbols.kwInt => IntTy
//      case Symbols.kwVoid => VoidTy
//      case name if namedTypeMap.contains(name.name()) =>
//        namedTypeMap(name.name())
//      case _ => throw new TypeAnalysisException(s"Checker for ${nt.getName.name()} is not implemented.", nt.location())
//    }
//
//  case p: ASTPointerType =>
//    typeMap(node) = PtrTy(resolveNodeTy(p.getBase))
//
//  case a: ASTArrayType =>
//    a.getSize.asRealType match {
//      case size: ASTInteger =>
//        if (size.getValue <= 0)
//          throw errorInvalidArraySize(a.getSize)
//        val baseTy = resolveNodeTy(a.getBase)
//        if (baseTy.isInstanceOf[ArrayTy])
//          throw new NotImplementedError("Multi dimensional arrays are not implemented.")
//        typeMap(node) = ArrayTy(baseTy, size.getValue.toInt)
//
//      case _ =>
//        throw errorInvalidArraySize(a.getSize)
//    }
//
//  case fpd: ASTFunPtrDecl =>
//    typeMap(node) = PtrTy(FunTy(resolveNodeTy(fpd.returnType), fpd.argTypes.map(resolveNodeTy).toIndexedSeq))
//    namedTypeMap(fpd.nameString) = typeMap(node)
//
//  // Stmts
//
//  case vd: ASTVarDecl =>
//    typeMap(node) = VoidTy
//    vd.children.foreach(visitNode)
//    if (vd.value.nonEmpty)
//      assertTypeAssignableFrom(
//        resolveNodeTy(vd.varType),
//        resolveNodeTy(vd.value.get),
//        s"on the right side of variable declaration",
//        vd.location())
//
//  case i: ASTIf =>
//    typeMap(node) = VoidTy
//    assertCond(i.cond, "if")
//    i.children.foreach(visitNode)
//
//  case w: ASTWhile =>
//    typeMap(node) = VoidTy
//    assertCond(w.cond, "while")
//    w.children.foreach(visitNode)
//
//  case dw: ASTDoWhile =>
//    typeMap(node) = VoidTy
//    assertCond(dw.cond, "do..while")
//    dw.children.foreach(visitNode)
//
//  case f: ASTFor =>
//    typeMap(node) = VoidTy
//    f.cond.foreach(assertCond(_, "for"))
//    f.children.foreach(visitNode)
//
//  case s: ASTSwitch =>
//    typeMap(node) = VoidTy
//    assertCond(s.cond, "switch")
//    s.children.foreach(visitNode)
//
//  case b: ASTBlock =>
//    typeMap(node) = VoidTy
//    b.children.foreach(visitNode)
//
//  case _: ASTBreak | _: ASTContinue =>
//    typeMap(node) = VoidTy
//
//  case r: ASTReturn =>
//    typeMap(node) = VoidTy
//    r.children.foreach(visitNode)
//    assertTypeAssignableFrom(
//      curFunTy.returnTy,
//      r.value.map(resolveNodeTy).getOrElse(VoidTy),
//      s"as return value",
//      r.location())
//
//  case w: ASTWrite =>
//    typeMap(node) = VoidTy
//    assertTypeAssignableFrom(
//      IntTy,
//      resolveNodeTy(w.getValue),
//      s"as print argument",
//      w.getValue.location())
//
//  // Exprs
//
//  case a: ASTAssignment =>
//    assertLvalue(a.getLvalue, "on right hand side of =")
//    val lvalueTy = resolveNodeTy(a.getLvalue)
//    val valueTy = resolveNodeTy(a.getValue)
//    assertTypeAssignableFrom(lvalueTy, valueTy, "on right hand side of =", a.getValue.location())
//    typeMap(node) = valueTy
//
//  case _: ASTInteger =>
//    typeMap(node) = IntTy
//
//  case s: ASTSequence => // ASTSequence or ASTBlock
//    typeMap(node) = VoidTy
//    s.children.foreach(visitNode)
//
//  case b: ASTBinaryOp =>
//    visitBinaryOp(b)
//
//  case u: ASTUnaryOp =>
//    visitUnaryOp(u)
//
//  case up: ASTUnaryPostOp =>
//    visitUnaryPostOp(up)
//
//  case _: ASTRead =>
//    typeMap(node) = IntTy
//
//  case i: ASTIdentifier =>
//    typeMap(node) = i.decl match {
//      case FunDecl(node) => resolveNodeTy(node)
//      case VarDecl(node) => resolveNodeTy(node.getVarType)
//      case FunArgDecl(fun, index, _) => resolveNodeTy(fun).asInstanceOf[FunTy].argTys(index)
//    }
//
//  case a: ASTAddress =>
//    val targetTy = resolveNodeTy(a.getTarget)
//    assertLvalue(a.getTarget, "as argument of &")
//    typeMap(node) = PtrTy(targetTy)
//
//  case d: ASTDeref =>
//    typeMap(node) = resolveNodeTy(d.getTarget) match {
//      case PtrTy(targetTy) => targetTy
//      case ty => throw errorExpectedPtrType(ty, "in deref", d.location())
//    }
//
//  case c: ASTCall =>
//    resolveNodeTy(c.function) match {
//      case FunTy(returnTy, argTys) =>
//        typeMap(node) = returnTy
//        if (c.args.size != argTys.size)
//          throw errorExpectedNumArgs(argTys.size, c.args.size, "in call expression", c.function.location())
//        c.args.zip(argTys).zipWithIndex.foreach({ case ((arg, defTy), i) =>
//          val actualTy = resolveNodeTy(arg)
//          assertTypeAssignableFrom(defTy, actualTy, s"in $i-th argument of function call", arg.location())
//        })
//
//      case ty => throw errorExpectedFunType(ty, "in call expression", c.function.location())
//    }
//
//  case c: ASTCast =>
//    val targetTy = resolveNodeTy(c.getType)
//    val valueTy = resolveNodeTy(c.getValue)
//    if (targetTy.getCastModeFrom(valueTy).isEmpty)
//      throw errorInvalidCast(valueTy, targetTy, c.location())
//    typeMap(node) = targetTy
//
//  case i: ASTIndex =>
//    val baseTy = resolveNodeTy(i.getBase)
//    val indexTy = resolveNodeTy(i.getIndex)
//    //      assertLvalue(i.getBase, "as array access base")
//    typeMap(node) = baseTy match {
//      case PtrTy(targetTy) => targetTy
//      case ArrayTy(elemTy, _) => elemTy
//      case _ => throw errorExpectedPtrOrArray(baseTy, "as array access base", i.getBase.location())
//    }
//    assertTypeAssignableFrom(IntTy, indexTy, "in array access index", i.getIndex.location())
//
//
//  case n => throw new NotImplementedError(s"TypeChecker: $n")
//}
//
//private def isIntOrPtrTy(ty: Ty): Boolean = ty match {
//  case IntTy | PtrTy(_) => true
//  case _ => false
//}
//
//private def visitUnaryOp(node: ASTUnaryOp): Unit =
//  typeMap(node) = (node.getOp, resolveNodeTy(node.getArg)) match {
//    case (Symbols.add | Symbols.sub | Symbols.not | Symbols.neg, argTy@IntTy) =>
//      argTy
//
//    case (Symbols.add, argTy: PtrTy) =>
//      argTy
//
//    case (Symbols.inc | Symbols.dec, argTy) if isIntOrPtrTy(argTy) =>
//      assertLvalue(node.getArg, "as ++/-- arg")
//      argTy
//
//    case _ => throw new NotImplementedError(s"TypeChecker for ASTUnaryOp: '$node'")
//  }
//
//private def visitUnaryPostOp(node: ASTUnaryPostOp): Unit =
//  typeMap(node) = (node.getOp, resolveNodeTy(node.getArg)) match {
//    case (Symbols.inc | Symbols.dec, argTy) if isIntOrPtrTy(argTy) =>
//      assertLvalue(node.getArg, "as ++/-- arg")
//      argTy
//
//    case _ => throw new NotImplementedError(s"TypeChecker for ASTUnaryPostOp: '$node'")
//  }
//
//private def visitBinaryOp(node: ASTBinaryOp): Unit =
//  typeMap(node) = (node.getOp, resolveNodeTy(node.getLeft), resolveNodeTy(node.getRight)) match {
//    case (Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.bitAnd | Symbols.bitOr | Symbols.xor | Symbols.mod, leftTy@IntTy, rightTy) =>
//      if (leftTy != rightTy)
//        throw errorExpectedType(leftTy, rightTy, s"on the right side of ${node.getOp}", node.location())
//      leftTy
//
//    case (Symbols.add | Symbols.sub, leftTy: PtrTy, rightTy@IntTy) =>
//      leftTy
//
//    case (Symbols.lt | Symbols.gt | Symbols.gte | Symbols.lte | Symbols.eq | Symbols.neq, leftTy, rightTy) if isIntOrPtrTy(leftTy) =>
//      if (leftTy != rightTy)
//        throw errorExpectedType(leftTy, rightTy, s"on the right side of ${node.getOp}", node.location())
//      IntTy
//
//    case (Symbols.and | Symbols.or, leftTy@IntTy, rightTy) =>
//      if (leftTy != rightTy)
//        throw errorExpectedType(leftTy, rightTy, s"on the right side of ${node.getOp}", node.location())
//      IntTy
//
//    case _ => throw new NotImplementedError(s"TypeChecker for ASTBinaryOp: '$node'")
//  }
//
//private def assertCond(cond: AST, label: String): Unit = resolveNodeTy(cond) match {
//  case IntTy =>
//  case condTy => throw errorExpectedType(IntTy, condTy, s"in $label condition", cond.location())
//}
//
//private def assertLvalue(node: AST, label: String): Unit = node.asRealType match {
//  case _: ASTIdentifier | _: ASTDeref | _: ASTIndex =>
//
//  case n => errorExpectedLValue(n, label, node.location())
//}
//
//private def assertTypeAssignableFrom(varTy: Ty, actualTy: Ty, label: String, loc: SourceLocation): Unit =
//  if (!varTy.isAssignableFrom(actualTy))
//    throw errorExpectedType(varTy, actualTy, label, loc)
//}