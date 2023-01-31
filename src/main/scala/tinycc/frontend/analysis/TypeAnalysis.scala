package tinycc.frontend.analysis

import tinycc.frontend.Types._
import tinycc.frontend.{Declarations, Symbols, TypeMap}
import tinycc.frontend.ast.{AstAddress, AstArrayType, AstAssignment, AstBinaryOp, AstBlock, AstBreak, AstCall, AstCast, AstChar, AstContinue, AstDecl, AstDeref, AstDoWhile, AstDouble, AstFor, AstFunDecl, AstFunPtrDecl, AstIdentifier, AstIf, AstIndex, AstInteger, AstMember, AstMemberPtr, AstNamedType, AstNode, AstPointerType, AstRead, AstReturn, AstSequence, AstString, AstStructDecl, AstSwitch, AstType, AstUnaryOp, AstUnaryPostOp, AstVarDecl, AstVisitor, AstWhile, AstWrite}
import tinycc.util.parsing.SourceLocation

import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

class TypeAnalysisException(message: String, loc: SourceLocation) extends RuntimeException(message) {
  //  override def format(reporter: Reporter): String = reporter.formatError("type checker", message, loc)
}

class TypeAnalysis(program: AstBlock, _declarations: Declarations) {
  implicit private def declarations: Declarations = _declarations

  implicit private val typeMap: mutable.Map[AstNode, Ty] = mutable.Map.empty

  private var curFunTyOption: Option[FunTy] = None

  private def curFunTy: FunTy = curFunTyOption.get

  private val funDeclMap: mutable.Map[Symbol, AstFunDecl] = mutable.Map.empty

  private val namedTypeMap: mutable.Map[Symbol, (Ty, SourceLocation)] = mutable.Map.empty

  private val errors: mutable.Buffer[TypeAnalysisException] = mutable.Buffer.empty

  lazy val result: Either[Seq[TypeAnalysisException], TypeMap] = {
    program.accept(new _Visitor)
    if (errors.nonEmpty)
      Left(errors.toSeq)
    else
      Right(typeMap)
  }

  private def hasAddress(node: AstNode): Boolean = node match {
    case _: AstIdentifier | AstDeref | AstIndex => true
    case _ => false
  }

  private def checkLvalue(node: AstNode, label: String): Boolean = {
    val hasAddr = hasAddress(node)
    if (!hasAddr)
      errors += new TypeAnalysisException(s"expected l-value $label", node.loc)
    hasAddr
  }

  // At the end of each visit method, typeMap(node) must be set.
  private class _Visitor extends AstVisitor[Unit] {
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

    // ----------------

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
        case FunDecl(node) => PtrTy(node.ty)
        case FunArgDecl(node, index) => node.ty.asInstanceOf[FunTy].argTys(index)
        case VarDecl(node) => node.ty
      }
    }

    override def visitPointerType(node: AstPointerType): Unit = {
      typeMap(node) = PtrTy(visitAndGetTy(node.base))
    }

    override def visitArrayType(node: AstArrayType): Unit = {
      node.base.accept(this) // TODO
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
              ErrorTy
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
          errors += new TypeAnalysisException(s"incompatible declaration of variable '${node.symbol.name}' ($varTy), previously declared as ${prevNode.varTy.ty}", node.loc)
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
          typeMap(node) = StructTy(Some(node.symbol), fields)

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

    override def visitBinaryOp(node: AstBinaryOp): Unit = {
      val leftTy = visitAndGetTy(node.left)
      val rightTy = visitAndGetTy(node.right)

      def promoteArithmeticTys(leftTy: ArithmeticTy, rightTy: ArithmeticTy): ArithmeticTy = {
        if (leftTy < rightTy && rightTy.isAssignableFrom(leftTy))
          rightTy // Promote operands to rightTy.
        else if (leftTy > rightTy && leftTy.isAssignableFrom(rightTy))
          leftTy // Promote operands to leftTy.
        else
          leftTy
      }

      def errorInvalidOperands: Ty = {
        errors += new TypeAnalysisException(s"invalid operands to binary ${node.op.name} (got $leftTy and $rightTy)", node.loc)
        VoidTy
      }

      typeMap(node) = node.op match {
        case Symbols.add => (leftTy, rightTy) match {
            case (leftTy: ArithmeticTy, rightTy: ArithmeticTy) =>
              promoteArithmeticTys(leftTy, rightTy)

            case (leftTy: PtrTy, _: IntegerTy) =>
              leftTy

            case _ => errorInvalidOperands
          }

        case Symbols.sub => (leftTy, rightTy) match {
          case (leftTy: ArithmeticTy, rightTy: ArithmeticTy) =>
            promoteArithmeticTys(leftTy, rightTy)

          case (leftTy: PtrTy, _: IntegerTy) =>
            leftTy

          case (leftTy: PtrTy, rightTy: PtrTy) if leftTy == rightTy =>
            leftTy

          case _ => errorInvalidOperands
        }

        case Symbols.mul | Symbols.div => (leftTy, rightTy) match {
          case (leftTy: ArithmeticTy, rightTy: ArithmeticTy) =>
            promoteArithmeticTys(leftTy, rightTy)

          case _ => errorInvalidOperands
        }

        case Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.xor => (leftTy, rightTy) match {
          case (leftTy: IntegerTy, rightTy: IntegerTy) =>
            promoteArithmeticTys(leftTy, rightTy)

          case _ => errorInvalidOperands
        }

        case Symbols.lt | Symbols.gt | Symbols.`ge` | Symbols.`le` => (leftTy, rightTy) match {
          case (_: ArithmeticTy, _: ArithmeticTy) =>
            IntTy

          case (leftTy: PtrTy, rightTy: PtrTy) if leftTy == rightTy =>
            IntTy

          case _ => errorInvalidOperands
        }

        case Symbols.eq | Symbols.`ne` => (leftTy, rightTy) match {
          case (_: ArithmeticTy, _: ArithmeticTy) =>
            IntTy

          case (leftTy, rightTy) if leftTy == rightTy =>
            IntTy

          case _ => errorInvalidOperands
        }

        case Symbols.and | Symbols.or => (leftTy, rightTy) match {
          case (_: ScalarTy, _: ScalarTy) =>
            IntTy

          case _ => errorInvalidOperands
        }

        case op => throw new UnsupportedOperationException(s"Type checking for binary ${op.name} is not defined.")
      }
    }

    override def visitAssignment(node: AstAssignment): Unit = {
      checkLvalue(node.lvalue, "on the left side of assignment")
      val lvalueTy = visitAndGetTy(node.lvalue)
      val valueTy = visitAndGetTy(node.value)
      if (!lvalueTy.isAssignableFrom(valueTy))
        errors += new TypeAnalysisException(s"cannot assign $valueTy to l-value of type $lvalueTy", node.loc)
      typeMap(node) = lvalueTy
    }

    override def visitUnaryOp(node: AstUnaryOp): Unit = {
      val exprTy = visitAndGetTy(node.expr)

      node.op match {
        case Symbols.add | Symbols.sub | Symbols.neg =>
          if (!IntTy.isAssignableFrom(exprTy)) {
            errors += new TypeAnalysisException(s"wrong type argument to unary plus (expected int-like type), got $exprTy", node.loc)
            typeMap(node) = IntTy
          } else
            typeMap(node) = exprTy

        case Symbols.not =>
          if (!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy])
            errors += new TypeAnalysisException(s"wrong type argument to unary not (expected int-like or pointer type), got $exprTy", node.loc)
          typeMap(node) = IntTy

        case Symbols.inc | Symbols.dec =>
          checkLvalue(node.expr, s"in unary ${node.op.name}")
          if (!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy]) {
            errors += new TypeAnalysisException(s"wrong type argument to unary not (expected int-like or pointer type), got $exprTy", node.loc)
            typeMap(node) = IntTy
          } else
            typeMap(node) = exprTy

        case op => new UnsupportedOperationException(s"Type checking for unary prefix ${op.name} is not defined.")
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

        case op => new UnsupportedOperationException(s"Type checking for unary postfix ${op.name} is not defined.")
      }
    }

    override def visitAddress(node: AstAddress): Unit = {
      checkLvalue(node.expr)
      typeMap(node) = visitAndGetTy(node.expr) match {
        case exprTy@PtrTy(_: FunTy) => exprTy
        case exprTy => PtrTy(exprTy)
      }
    }

    override def visitDeref(node: AstDeref): Unit = {
      typeMap(node) = visitAndGetTy(node.expr) match {
        case exprTy@PtrTy(_: FunTy) => exprTy
        case PtrTy(baseTy) => baseTy
        case exprTy =>
          errors += new TypeAnalysisException(s"cannot dereference non-pointer type ($exprTy)", node.loc)
          exprTy
      }
    }

    override def visitIndex(node: AstIndex): Unit = {
      val baseTy = visitAndGetTy(node.base)
      val indexTy = visitAndGetTy(node.index)
      if (!IntTy.isAssignableFrom(indexTy))
        errors += new TypeAnalysisException(s"expected $IntTy (or compatible type), got $indexTy", node.index.loc)
      typeMap(node) = baseTy match {
        case PtrTy(_: FunTy) =>
          errors += new TypeAnalysisException(s"cannot index into pointer to function", node.loc)
          baseTy

        case PtrTy(targetTy) =>
          targetTy

        case _ =>
          errors += new TypeAnalysisException(s"expected array or pointer, got $baseTy", node.loc)
          baseTy
      }
    }

    override def visitMember(node: AstMember): Unit = {
      typeMap(node) = visitAndGetTy(node.base) match {
        case baseTy: StructTy if !baseTy.isComplete =>
          errors += new TypeAnalysisException(s"cannot access fields in an incomplete struct", node.loc)
          VoidTy
        case baseTy@StructTy(_, Some(fields)) =>
          val fieldTy = fields.collectFirst({ case (ty, name) if name == node.member => ty })
          fieldTy match {
            case Some(value) => value
            case None =>
              errors += new TypeAnalysisException(s"unknown member ${node.member.name} in $baseTy", node.loc)
              VoidTy
          }

        case baseTy =>
          errors += new TypeAnalysisException(s"expected struct type, got $baseTy", node.loc)
          VoidTy
      }
    }

    override def visitMemberPtr(node: AstMemberPtr): Unit = {
      typeMap(node) = visitAndGetTy(node.base) match {
        case PtrTy(baseTy: StructTy) if !baseTy.isComplete =>
          errors += new TypeAnalysisException(s"cannot access fields in an incomplete struct $baseTy", node.loc)
          VoidTy
        case PtrTy(baseTy@StructTy(_, Some(fields))) =>
          val fieldTy = fields.collectFirst({ case (ty, name) if name == node.member => ty })
          fieldTy match {
            case Some(value) => value
            case None =>
              errors += new TypeAnalysisException(s"unknown member ${node.member.name} in $baseTy", node.loc)
              VoidTy
          }

        case baseTy =>
          errors += new TypeAnalysisException(s"expected pointer to struct type, got $baseTy", node.loc)
          VoidTy
      }
    }

    override def visitCall(node: AstCall): Unit = {
      val exprTy = visitAndGetTy(node.expr)
      val argExprTys = node.args.map(visitAndGetTy)

      typeMap(node) = exprTy match {
        case PtrTy(FunTy(returnTy, argTys)) =>
          if (argTys.size == argExprTys.size) {
            argExprTys.zip(argTys).zipWithIndex.foreach({ case ((argExprTy, argTy), i) =>
              if (!argTy.isAssignableFrom(argExprTy))
                errors += new TypeAnalysisException(s"expected $argTy (or compatible type), $argExprTy given (arg #$i)", node.loc)
            })
          } else
            errors += new TypeAnalysisException(s"expected ${argTys.size} arguments, ${argExprTys.size} given", node.loc)
          returnTy

        case _ =>
          errors += new TypeAnalysisException(s"expected function pointer, got $exprTy", node.loc)
          VoidTy
      }
    }

    override def visitCast(node: AstCast): Unit = {
      val newTy = visitAndGetTy(node.newTy)
      val exprTy = visitAndGetTy(node.expr)
      if (newTy.getCastModeFrom(exprTy).isEmpty)
        errors += new TypeAnalysisException(s"cannot cast $exprTy to $newTy", node.loc)
      typeMap(node) = newTy
    }

    override def visitWrite(node: AstWrite): Unit = {
      val exprTy = visitAndGetTy(node.expr)
      if (!CharTy.isAssignableFrom(exprTy))
        errors += new TypeAnalysisException(s"expected $CharTy (or compatible type), got $exprTy", node.expr.loc)
      typeMap(node) = VoidTy
    }

    override def visitRead(node: AstRead): Unit = {
      typeMap(node) = CharTy
    }

  }
}