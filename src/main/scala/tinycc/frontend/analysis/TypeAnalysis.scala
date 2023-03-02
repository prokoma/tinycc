package tinycc.frontend.analysis

import tinycc.cli.Reporter
import tinycc.frontend.Types._
import tinycc.frontend.ast._
import tinycc.frontend.{Declarations, Symbols, TypeMap}
import tinycc.util.parsing.SourceLocation
import tinycc.{ErrorLevel, ProgramException}

import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered

class TypeAnalysisException(messages: Seq[TypeAnalysisException.Message]) extends ProgramException(messages.map(_.message).mkString("\n")) {
  override def format(reporter: Reporter): String = messages.map(_.format(reporter)).mkString("\n")
}

object TypeAnalysisException {
  class Message(val level: ErrorLevel, message: String, val loc: SourceLocation) extends ProgramException(message) {
    override def format(reporter: Reporter): String = reporter.formatError(level, message, loc)
  }
}

final class TypeAnalysis(program: AstProgram, _declarations: Declarations) {

  import ErrorLevel._
  import IdentifierDecl._
  import TypeAnalysisException.Message

  implicit private def declarations: Declarations = _declarations

  implicit private val typeMap: mutable.Map[AstNode, Ty] = mutable.Map.empty

  private var curFunTyOption: Option[FunTy] = None

  private def curFunTy: FunTy = curFunTyOption.get

  private val funDeclMap: mutable.Map[Symbol, AstFunDecl] = mutable.Map.empty

  private val namedTypeMap: mutable.Map[Symbol, (Ty, SourceLocation)] = mutable.Map.empty

  private val errors: mutable.Buffer[Message] = mutable.Buffer.empty

  lazy val result: Either[TypeAnalysisException, TypeMap] = {
    visitAndGetTy(program)
    if (errors.nonEmpty)
      Left(new TypeAnalysisException(errors.toSeq))
    else
      Right(typeMap)
  }

  private def hasAddress(node: AstNode): Boolean = node match {
    case _: AstIdentifier | _: AstDeref | _: AstIndex => true
    case _ => false
  }

  private def visitLValue(node: AstNode, label: String): Unit = {
    val hasAddr = hasAddress(node)
    if (!hasAddr)
      errors += new Message(Error, s"expected l-value $label", node.loc)
  }

  private def visitGuard(guard: AstNode, label: String): Unit = {
    val guardTy = visitAndGetTy(guard)
    if (!IntTy.isAssignableFrom(guardTy))
      errors += new Message(Error, s"expected $IntTy (or compatible type) $label, got $guardTy", guard.loc)
  }

  private def visitAndGetTyIfComplete(node: AstNode, label: String): Ty = {
    val ty = visitAndGetTy(node)
    if (!ty.isComplete)
      errors += new Message(Error, s"cannot use incomplete type $ty $label", node.loc)
    ty
  }

  private def visitAndGetTy(node: AstNode): Ty = typeMap.getOrElseUpdate(node, node match {
    case _: AstInteger => IntTy
    case _: AstDouble => DoubleTy
    case _: AstChar => CharTy
    case node: AstString => ArrayTy(CharTy, node.value.length)

    case node: AstIdentifier =>
      // need to call visitAndGetTy on the declaration, because it may be later in the code
      // it always points to the complete declaration
      node.decl match {
        case FunDecl(node) => PtrTy(visitAndGetTy(node))
        case FunArgDecl(node, index) => visitAndGetTy(node).asInstanceOf[FunTy].argTys(index)
        case VarDecl(node) => visitAndGetTy(node)
      }

    case node: AstPointerType =>
      PtrTy(visitAndGetTy(node.base))

    case node: AstArrayType =>
      PtrTy(visitAndGetTy(node.base))

    case node: AstNamedType =>
      node.symbol match {
        case Symbols.kwVoid => VoidTy
        case Symbols.kwChar => CharTy
        case Symbols.kwInt => IntTy
        case Symbols.kwDouble => DoubleTy
        case name =>
          namedTypeMap.get(name) match {
            case Some((ty, _)) => ty
            case None =>
              errors += new Message(Error, s"undeclared named type '${name.name}'", node.loc)
              ErrorTy
          }
      }

    case node: AstSequence =>
      node.body.map(visitAndGetTy).lastOption.getOrElse(VoidTy)

    case node: AstBlock =>
      node.body.foreach(visitAndGetTy)
      VoidTy

    case node: AstProgram =>
      node.body.foreach(visitAndGetTy)
      VoidTy

    case node: AstVarDecl =>
      val varTy = visitAndGetTyIfComplete(node.varTy, s"in declaration of variable '${node.symbol.name}'")

      // Check if the variable is compatible with its previous declaration.
      node.prevDecl match {
        case Some(VarDecl(prevNode)) if prevNode.varTy.ty != varTy =>
          errors += new Message(Error, s"incompatible declaration of variable '${node.symbol.name}' ($varTy), previously declared as ${prevNode.varTy.ty}", node.loc)
          errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevNode.loc)

        case _ =>
      }

      node.value.foreach(value => {
        // Check for multiple definitions of a variable.
        val prevDef = node.prevDecls.collectFirst({ case VarDecl(prevNode) if prevNode.value.isDefined => prevNode })
        prevDef match {
          case Some(prevNode) =>
            errors += new Message(Error, s"redefinition of variable '${node.symbol.name}' ($varTy)", node.loc)
            errors += new Message(Note, s"previous definition of '${node.symbol.name}'", prevNode.loc)

          case None =>
        }

        // Check value type.
        val valueTy = visitAndGetTy(value)
        if (!varTy.isAssignableFrom(valueTy))
          errors += new Message(Error, s"cannot assign value of type $valueTy to variable '${node.symbol.name}' of type $varTy", value.loc)
      })
      varTy

    case node: AstFunDecl =>
      val ty = FunTy(
        visitAndGetTyIfComplete(node.returnTy, s"in definition of '${node.symbol.name}' (return type)"),
        node.args.map({
          case (argTy, argName) =>
            visitAndGetTyIfComplete(argTy, s"in definition of '${node.symbol.name}' (arg '$argName')")
        }).toIndexedSeq)

      curFunTyOption = Some(ty)
      typeMap(node) = ty // Set the type early to prevent infinite recursion if the function is used in the body.

      funDeclMap.get(node.symbol) match {
        case Some(prevDecl) =>
          val prevTy = visitAndGetTy(prevDecl)
          if (prevTy != ty) {
            errors += new Message(Error, s"incompatible declaration of function '${node.symbol.name}' (${ty}), previously declared as ${prevTy}", node.loc)
            errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
          }
          if (prevDecl.body.isDefined && node.body.isDefined) {
            errors += new Message(Error, s"redefinition of function '${node.symbol.name}' (${ty})", node.loc)
            errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
          }

          if (prevDecl.body.isEmpty)
            funDeclMap(node.symbol) = node // Store the last complete definition of the function, so the check above works.

        case None =>
          funDeclMap(node.symbol) = node
      }

      node.body.foreach(visitAndGetTy)
      ty

    case node: AstStructDecl =>
      lazy val fields =
        node.fields.map(_.map({ case (astTy, name) =>
          (visitAndGetTyIfComplete(astTy, s"in definition of 'struct ${node.symbol.name}'"), name)
        }).toIndexedSeq)

      namedTypeMap.get(node.symbol) match {
        case Some((prevTy: StructTy, _)) if !node.hasFields =>
          // Adding another forward declaration after either full or forward declaration is allowed.
          prevTy

        case Some((prevTy: StructTy, prevDeclLoc)) if prevTy.isComplete && node.hasFields =>
          errors += new Message(Error, s"redefinition of 'struct ${node.symbol.name}'", node.loc)
          errors += new Message(Note, s"previous definition of 'struct ${node.symbol.name}'", prevDeclLoc)
          prevTy

        case Some((prevTy: StructTy, _)) if !prevTy.isComplete && node.hasFields =>
          // Completion of a partially defined struct.
          namedTypeMap(node.symbol) = (prevTy, node.loc)
          prevTy.fields = fields
          prevTy

        case Some((_, prevDeclLoc)) =>
          errors += new Message(Error, s"redefinition of 'struct ${node.symbol.name}' as another kind of type", node.loc)
          errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDeclLoc)
          StructTy(Some(node.symbol), fields)

        case None =>
          val ty = StructTy()
          // Add the forward declaration to the map, so it can be used inside of the struct.
          namedTypeMap(node.symbol) = (ty, node.loc)
          ty.fields = fields // Now we can evaluate the field types.
          ty
      }

    case node: AstFunPtrDecl =>
      lazy val ty =
        PtrTy(FunTy(visitAndGetTyIfComplete(node.returnTy, s"in definition of 'typedef ${node.symbol.name}' (return type)"),
          node.argTys.zipWithIndex.map({ case (argTy, index) =>
            visitAndGetTyIfComplete(argTy, s"in definition of 'typedef ${node.symbol.name}' (arg #${index + 1})")
          }).toIndexedSeq))

      namedTypeMap.get(node.symbol) match {
        case Some((_, prevDeclLoc)) =>
          errors += new Message(Error, s"redefinition of 'typedef ${node.symbol.name}", node.loc)
          errors += new Message(Note, s"previous definition of 'typedef ${node.symbol.name}'", prevDeclLoc)
          ty

        case None =>
          namedTypeMap(node.symbol) = (ty, node.loc)
          ty
      }

    case node: AstIf =>
      visitGuard(node.guard, "in if stmt guard")
      visitAndGetTy(node.trueCase)
      node.falseCase.foreach(visitAndGetTy)
      VoidTy

    case node: AstSwitch =>
      visitGuard(node.guard, "in switch stmt guard")
      node.cases.foreach({ case (_, body) => visitAndGetTy(body) })
      VoidTy

    case node: AstWhile =>
      visitGuard(node.guard, "in while stmt guard")
      visitAndGetTy(node.body)
      VoidTy

    case node: AstDoWhile =>
      visitGuard(node.guard, "in do..while stmt guard")
      visitAndGetTy(node.body)
      VoidTy

    case node: AstFor =>
      node.init.foreach(visitAndGetTy)
      node.guard.foreach(visitGuard(_, "in for stmt guard"))
      node.increment.foreach(visitAndGetTy)
      visitAndGetTy(node.body)
      VoidTy

    case node: AstBreak => VoidTy
    case node: AstContinue => VoidTy

    case node: AstReturn =>
      val exprTy = node.expr.map(visitAndGetTy).getOrElse(VoidTy)
      val exprLoc = node.expr.getOrElse(node).loc

      curFunTyOption match {
        case Some(funTy) =>
          if (!funTy.returnTy.isAssignableFrom(exprTy))
            errors += new Message(Error, s"expected ${funTy.returnTy} (or compatible type), got $exprTy", exprLoc)

        case None =>
          errors += new Message(Error, s"cannot use return outside of function", node.loc)
      }
      VoidTy

    case node: AstBinaryOp =>
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
        errors += new Message(Error, s"invalid operands to binary ${node.op.name} (got $leftTy and $rightTy)", node.loc)
        ErrorTy
      }

      node.op match {
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

        case Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.`bitXor` => (leftTy, rightTy) match {
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

    case node: AstAssignment =>
      visitLValue(node.lvalue, "on the left side of assignment")
      val lvalueTy = visitAndGetTy(node.lvalue)
      val valueTy = visitAndGetTy(node.value)
      if (!lvalueTy.isAssignableFrom(valueTy))
        errors += new Message(Error, s"cannot assign $valueTy to l-value of type $lvalueTy", node.loc)
      lvalueTy

    case node: AstUnaryOp =>
      val exprTy = visitAndGetTy(node.expr)

      node.op match {
        case Symbols.add | Symbols.sub | Symbols.neg =>
          if (!IntTy.isAssignableFrom(exprTy)) {
            errors += new Message(Error, s"wrong type argument to unary ${node.op.name} (expected int-like type), got $exprTy", node.loc)
            IntTy
          } else
            exprTy

        case Symbols.not =>
          if (!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy])
            errors += new Message(Error, s"wrong type argument to unary ${node.op.name} (expected int-like or pointer type), got $exprTy", node.loc)
          IntTy

        case Symbols.inc | Symbols.dec =>
          visitLValue(node.expr, s"in unary ${node.op.name}")
          if (!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy]) {
            errors += new Message(Error, s"wrong type argument to unary ${node.op.name} (expected int-like or pointer type), got $exprTy", node.loc)
            IntTy
          } else
            exprTy

        case op =>
          throw new UnsupportedOperationException(s"Type checking for unary prefix ${op.name} is not defined.")
      }

    case node: AstUnaryPostOp =>
      val exprTy = visitAndGetTy(node.expr)

      node.op match {
        case Symbols.inc | Symbols.dec =>
          visitLValue(node.expr, s"in unary ${node.op.name}")
          if (!IntTy.isAssignableFrom(exprTy) && !exprTy.isInstanceOf[PtrTy]) {
            errors += new Message(Error, s"wrong type argument to unary not (expected int-like or pointer type)", node.loc)
            IntTy
          } else
            exprTy

        case op =>
          throw new UnsupportedOperationException(s"Type checking for unary postfix ${op.name} is not defined.")
      }

    case node: AstAddress =>
      visitLValue(node.expr, "in & expr")
      visitAndGetTy(node.expr) match {
        case exprTy@PtrTy(_: FunTy) => exprTy
        case exprTy => PtrTy(exprTy)
      }

    case node: AstDeref =>
      visitAndGetTy(node.expr) match {
        case exprTy@PtrTy(_: FunTy) => exprTy
        case PtrTy(baseTy) => baseTy
        case exprTy =>
          errors += new Message(Error, s"cannot dereference non-pointer type ($exprTy)", node.loc)
          exprTy
      }

    case node: AstIndex =>
      val exprTy = visitAndGetTy(node.expr)
      val indexTy = visitAndGetTy(node.index)
      if (!IntTy.isAssignableFrom(indexTy))
        errors += new Message(Error, s"expected $IntTy (or compatible type), got $indexTy", node.index.loc)
      exprTy match {
        case PtrTy(_: FunTy) =>
          errors += new Message(Error, s"cannot index into pointer to function", node.loc)
          exprTy

        case exprTy: IndexableTy => exprTy.baseTy

        case exprTy =>
          errors += new Message(Error, s"expected array or pointer, got $exprTy", node.loc)
          exprTy
      }

    case node: AstMember =>
      visitAndGetTy(node.expr) match {
        case baseTy: StructTy if !baseTy.isComplete =>
          errors += new Message(Error, s"cannot access fields in an incomplete struct", node.loc)
          VoidTy
        case baseTy@StructTy(_, Some(fields)) =>
          val fieldTy = fields.collectFirst({ case (ty, name) if name == node.member => ty })
          fieldTy match {
            case Some(value) => value
            case None =>
              errors += new Message(Error, s"unknown member ${node.member.name} in $baseTy", node.loc)
              VoidTy
          }

        case baseTy =>
          errors += new Message(Error, s"expected struct type, got $baseTy", node.loc)
          VoidTy
      }

    case node: AstMemberPtr =>
      visitAndGetTy(node.expr) match {
        case PtrTy(baseTy: StructTy) if !baseTy.isComplete =>
          errors += new Message(Error, s"cannot access fields in an incomplete struct $baseTy", node.loc)
          VoidTy
        case PtrTy(baseTy@StructTy(_, Some(fields))) =>
          val fieldTy = fields.collectFirst({ case (ty, name) if name == node.member => ty })
          fieldTy match {
            case Some(value) => value
            case None =>
              errors += new Message(Error, s"unknown member ${node.member.name} in $baseTy", node.loc)
              VoidTy
          }

        case baseTy =>
          errors += new Message(Error, s"expected pointer to struct type, got $baseTy", node.loc)
          VoidTy
      }

    case node: AstCall =>
      val exprTy = visitAndGetTy(node.expr)
      val argExprTys = node.args.map(visitAndGetTy)

      exprTy match {
        case PtrTy(FunTy(returnTy, argTys)) =>
          if (argTys.size == argExprTys.size) {
            argExprTys.zip(argTys).zipWithIndex.foreach({ case ((argExprTy, argTy), i) =>
              if (!argTy.isAssignableFrom(argExprTy))
                errors += new Message(Error, s"expected $argTy (or compatible type), $argExprTy given (arg #$i)", node.loc)
            })
          } else
            errors += new Message(Error, s"expected ${argTys.size} arguments, ${argExprTys.size} given", node.loc)
          returnTy

        case _ =>
          errors += new Message(Error, s"expected function pointer, got $exprTy", node.loc)
          VoidTy
      }

    case node: AstCast =>
      val newTy = visitAndGetTy(node.newTy)
      val exprTy = visitAndGetTy(node.expr)
      if (newTy.isExplicitlyCastableFrom(exprTy))
        errors += new Message(Error, s"cannot cast $exprTy to $newTy", node.loc)
      newTy

    case node: AstWrite =>
      val exprTy = visitAndGetTy(node.expr)
      if (!CharTy.isAssignableFrom(exprTy))
        errors += new Message(Error, s"expected $CharTy (or compatible type), got $exprTy", node.expr.loc)
      VoidTy

    case node: AstWriteNum =>
      val exprTy = visitAndGetTy(node.expr)
      if (!IntTy.isAssignableFrom(exprTy))
        errors += new Message(Error, s"expected $IntTy (or compatible type), got $exprTy", node.expr.loc)
      VoidTy

    case node: AstRead => CharTy
  })
}