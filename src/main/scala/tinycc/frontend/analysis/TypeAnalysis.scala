package tinycc.frontend.analysis

import tinycc.ProgramException
import tinycc.frontend.Types._
import tinycc.frontend.ast._
import tinycc.frontend.parser.Symbols
import tinycc.frontend.{Declarations, TypeMap}
import tinycc.util.parsing.SourceLocation
import tinycc.util.{ErrorLevel, Reporter}

import scala.annotation.tailrec
import scala.collection.mutable

object TypeAnalysis {
  class TypeAnalysisException(messages: Seq[TypeAnalysisException.Message]) extends ProgramException(messages.map(_.message).mkString("\n")) {
    override def format(reporter: Reporter): String = messages.map(_.format(reporter)).mkString("\n")
  }

  object TypeAnalysisException {
    class Message(val level: ErrorLevel, message: String, val loc: SourceLocation) extends ProgramException("type analysis: " + message) {
      override def format(reporter: Reporter): String = reporter.formatError(level, getMessage, loc)
    }
  }

  @tailrec
  def hasAddress(node: AstNode): Boolean = node match {
    case _: AstIdentifier | _: AstDeref | _: AstMemberPtr | _: AstIndex => true
    case node: AstMember => hasAddress(node.expr)
    case _ => false
  }
}

final class TypeAnalysis(program: AstProgram, _declarations: Declarations) {

  import ErrorLevel._
  import IdentifierDecl._
  import TypeAnalysis.TypeAnalysisException.Message
  import TypeAnalysis._

  implicit private def declarations: Declarations = _declarations

  private val typeMap: mutable.Map[AstNode, Ty] = mutable.Map.empty

  private var curFunTyOption: Option[FunTy] = None

  private val namedTypeDeclMap: mutable.Map[Symbol, AstNamedTypeDecl] = mutable.Map.empty

  private val errors: mutable.Buffer[Message] = mutable.Buffer.empty

  def result(): TypeMap = {
    getTy(program)
    if (errors.nonEmpty)
      throw new TypeAnalysisException(errors.toSeq)
    typeMap.toMap
  }

  private def checkLvalueNode(node: AstNode, label: String): Unit = {
    if (!hasAddress(node))
      errors += new Message(Error, s"expected l-value $label", node.loc)
  }

  private def checkCompatibleType(ty: Ty, expected: Ty, context: String, loc: SourceLocation): Boolean = {
    val ok = expected.isAssignableFrom(ty)
    if (!ok)
      errors += new Message(Error, s"expected $expected (or compatible type) $context, got $ty", loc)
    ok
  }

  private def checkIntegerType(ty: Ty, context: String, loc: SourceLocation): Boolean = {
    val ok = ty.isInstanceOf[IntegerTy]
    if (!ok)
      errors += new Message(Error, s"expected integer type (int or char) $context, got $ty", loc)
    ok
  }

  private def checkScalarType(ty: Ty, context: String, loc: SourceLocation): Boolean = {
    val ok = ty.isInstanceOf[ScalarTy]
    if (!ok)
      errors += new Message(Error, s"expected scalar type (convertible to bool) $context, got $ty", loc)
    ok
  }

  private def checkCompleteType(ty: Ty, context: String, loc: SourceLocation): Boolean = {
    val ok = ty.complete
    if (!ok)
      errors += new Message(Error, s"expected complete type $context, got $ty", loc)
    ok
  }

  def getFieldTy(ty: StructTy, name: Symbol, context: String, loc: SourceLocation): Ty = {
    if (!checkCompleteType(ty, context, loc))
      return VoidTy

    val fieldTy = ty.fields.get.collectFirst({ case (t, n) if n == name => t })
    fieldTy.getOrElse({
      errors += new Message(Error, s"unknown member '${name.name}' in $ty", loc)
      VoidTy
    })
  }

  private def getTy(node: AstNode): Ty = {
    if (!typeMap.contains(node))
      visit(node)
    typeMap(node)
  }

  private def visit(node: AstNode): Unit = node match {
    // literals
    case _: AstInteger =>
      typeMap(node) = IntTy
    case _: AstDouble =>
      typeMap(node) = DoubleTy
    case _: AstChar =>
      typeMap(node) = CharTy
    case node: AstString =>
      typeMap(node) = ArrayTy(CharTy, node.value.length + 1) // + 1 for '\0'

    case node: AstIdentifier =>
      typeMap(node) = node.decl match {
        // return PtrTy(FunTy) for function identifiers (&foo and foo are equivalent)
        case FunDecl(node) => PtrTy(getTy(node))
        case FunArgDecl(node, index) => getTy(node).asInstanceOf[FunTy].argTys(index)
        case VarDecl(node) => getTy(node)
      }

    case node: AstPointerType =>
      typeMap(node) = PtrTy(getTy(node.base))

    case node: AstArrayType =>
      val baseTy = getTy(node.base)
      checkCompatibleType(getTy(node.size), IntTy, "as array size", node.size.loc)
      // check that array size is a positive integer literal
      typeMap(node) = node.size match {
        case size: AstInteger if size.value > 0 =>
          ArrayTy(baseTy, size.value.toInt)

        case _ =>
          errors += new Message(Error, s"expected a positive integer literal as array size", node.size.loc)
          PtrTy(baseTy) // return a pointer, so the analysis can continue
      }

    case node: AstNamedType =>
      typeMap(node) = node.symbol match {
        case Symbols.kwVoid => VoidTy
        case Symbols.kwChar => CharTy
        case Symbols.kwInt => IntTy
        case Symbols.kwDouble => DoubleTy

        case name => namedTypeDeclMap.get(name).map(getTy).getOrElse({
          errors += new Message(Error, s"undeclared names type '${name.name}'", node.loc)
          VoidTy
        })
      }

    case node: AstSequence =>
      // type of a sequence is the type of the last expression in its body, or void for empty sequences (but that shouldn't happen)
      typeMap(node) = node.body.map(getTy).lastOption.getOrElse(VoidTy)

    case node: AstBlock =>
      node.body.foreach(getTy)
      typeMap(node) = VoidTy

    case node: AstProgram =>
      node.body.foreach(getTy)
      typeMap(node) = VoidTy

    // typechecking declarations is a more complex affair
    case node: AstVarDecl => visitVarDecl(node)
    case node: AstFunDecl => visitFunDecl(node)
    case node: AstStructDecl => visitStructDecl(node)
    case node: AstFunPtrDecl => visitFunPtrDecl(node)

    case node: AstIf =>
      checkScalarType(getTy(node.guard), "in if stmt guard", node.guard.loc)
      getTy(node.trueCase)
      node.falseCase.foreach(getTy)
      typeMap(node) = VoidTy

    case node: AstSwitch =>
      checkScalarType(getTy(node.guard), "in switch stmt guard", node.guard.loc)
      node.cases.foreach({ case (_, body) => getTy(body) })
      node.defaultCase.foreach(getTy)
      typeMap(node) = VoidTy

    case node: AstWhile =>
      checkScalarType(getTy(node.guard), "in while stmt guard", node.guard.loc)
      getTy(node.body)
      typeMap(node) = VoidTy

    case node: AstDoWhile =>
      checkScalarType(getTy(node.guard), "in do..while stmt guard", node.guard.loc)
      getTy(node.body)
      typeMap(node) = VoidTy

    case node: AstFor =>
      node.init.foreach(getTy)
      node.guard.foreach(guard => checkScalarType(getTy(guard), "in for stmt guard", guard.loc))
      node.increment.foreach(getTy)
      getTy(node.body)
      typeMap(node) = VoidTy

    case node: AstBreak =>
      typeMap(node) = VoidTy

    case node: AstContinue =>
      typeMap(node) = VoidTy

    case node: AstReturn =>
      // handle both return with value and return without value
      val (exprTy, exprLoc) = node.expr.map(expr => (getTy(expr), expr.loc)).getOrElse((VoidTy, node.loc))

      curFunTyOption match {
        case Some(funTy) => checkCompatibleType(exprTy, funTy.returnTy, "", exprLoc)

        case None =>
          errors += new Message(Error, s"cannot use return outside of function", node.loc)
      }
      typeMap(node) = VoidTy

    case node: AstAssignment =>
      checkLvalueNode(node.lvalue, "on the left side of assignment")
      val lvalueTy = getTy(node.lvalue)
      val valueTy = getTy(node.value)
      if (!lvalueTy.isAssignableFrom(valueTy))
        errors += new Message(Error, s"cannot assign $valueTy to l-value of type $lvalueTy", node.loc)
      typeMap(node) = lvalueTy // value will be implicitly casted to lvalueTy

    case node: AstBinaryOp =>
      val leftTy = getTy(node.left)
      val rightTy = getTy(node.right)

      def errorInvalidOperands: Ty = {
        errors += new Message(Error, s"invalid operands to binary ${node.op.name} (got $leftTy and $rightTy)", node.loc)
        VoidTy
      }

      typeMap(node) = node.op match {
        case Symbols.add => (leftTy, rightTy) match {
          case (leftTy: ArithmeticTy, rightTy: ArithmeticTy) => ArithmeticTy.getResultTy(leftTy, rightTy)

          // adding integer to a pointer returns the modified pointer
          case (leftTy: PtrTyBase, _: IntegerTy) => leftTy

          case _ => errorInvalidOperands
        }

        case Symbols.sub => (leftTy, rightTy) match {
          case (leftTy: ArithmeticTy, rightTy: ArithmeticTy) => ArithmeticTy.getResultTy(leftTy, rightTy)

          // subtracting integer from a pointer returns the modified pointer
          case (leftTy: PtrTyBase, _: IntegerTy) =>
            checkCompleteType(leftTy, s"in binary ${node.op.name}", node.left.loc) // we need to know the size of an element
            leftTy

          // subtracting two pointers return int (the difference in elements)
          case (leftTy: PtrTyBase, rightTy: PtrTyBase) if leftTy == rightTy =>
            checkCompleteType(leftTy, s"in binary ${node.op.name}", node.left.loc)
            IntTy

          case _ => errorInvalidOperands
        }

        case Symbols.mul | Symbols.div => (leftTy, rightTy) match {
          case (leftTy: ArithmeticTy, rightTy: ArithmeticTy) => ArithmeticTy.getResultTy(leftTy, rightTy)

          case _ => errorInvalidOperands
        }

        // we don't support floating point modulo (just like in C99)
        case Symbols.mod | Symbols.bitAnd | Symbols.bitOr | Symbols.bitXor => (leftTy, rightTy) match {
          case (leftTy: IntegerTy, rightTy: IntegerTy) => ArithmeticTy.getResultTy(leftTy, rightTy)

          case _ => errorInvalidOperands
        }

        // bitwise shift has type of the left operand (does not promote like arithmetic operators)
        case Symbols.shiftLeft | Symbols.shiftRight => (leftTy, rightTy) match {
          case (leftTy: IntegerTy, rightTy: IntegerTy) => leftTy

          case _ => errorInvalidOperands
        }

        case Symbols.lt | Symbols.gt | Symbols.`ge` | Symbols.`le` => (leftTy, rightTy) match {
          case (_: ArithmeticTy, _: ArithmeticTy) => IntTy

          // pointers can be compared if they point to the same base type
          // we compare the memory addresses, so we don't have to know the size
          case (PtrTyBase(leftBaseTy), PtrTyBase(rightBaseTy)) if leftBaseTy == rightBaseTy => IntTy

          case _ => errorInvalidOperands
        }

        case Symbols.eq | Symbols.`ne` => (leftTy, rightTy) match {
          case (_: ArithmeticTy, _: ArithmeticTy) => IntTy

          // for == and !=, we can also compare pointer with void*
          case (PtrTyBase(leftBaseTy), PtrTyBase(rightBaseTy)) if leftBaseTy == rightBaseTy || leftBaseTy == VoidTy || rightBaseTy == VoidTy => IntTy

          case _ => errorInvalidOperands
        }

        case Symbols.and | Symbols.or => (leftTy, rightTy) match {
          // logical operators can take any type convertible to bool (scalar type)
          case (_: ScalarTy, _: ScalarTy) => IntTy

          case _ => errorInvalidOperands
        }

        case op => throw new UnsupportedOperationException(s"Type checking for binary ${op.name} is not defined.")
      }

    case node: AstUnaryOp =>
      val exprTy = getTy(node.expr)

      def errorInvalidOperand: Ty = {
        errors += new Message(Error, s"invalid operand to unary ${node.op.name} (got $exprTy)", node.loc)
        VoidTy
      }

      typeMap(node) = node.op match {
        case Symbols.add | Symbols.sub => exprTy match {
          case exprTy: ArithmeticTy => exprTy

          case _ => errorInvalidOperand
        }

        case Symbols.neg => exprTy match {
          case exprTy: IntegerTy => exprTy

          case _ => errorInvalidOperand
        }

        case Symbols.not => exprTy match {
          case exprTy: ScalarTy => IntTy

          case _ => errorInvalidOperand
        }

        case Symbols.inc | Symbols.dec =>
          checkLvalueNode(node.expr, s"in unary ${node.op.name}")
          exprTy match {
            case exprTy: ArithmeticTy => exprTy

            case PtrTyBase(baseTy) if baseTy != VoidTy =>
              checkCompleteType(baseTy, s"in unary ${node.op.name}", node.expr.loc)
              exprTy

            case _ => errorInvalidOperand
          }

        case op => throw new UnsupportedOperationException(s"Type checking for unary prefix ${op.name} is not defined.")
      }

    case node: AstUnaryPostOp =>
      val exprTy = getTy(node.expr)

      def errorInvalidOperand: Ty = {
        errors += new Message(Error, s"invalid operand to unary postfix ${node.op.name} (got $exprTy)", node.loc)
        VoidTy
      }

      typeMap(node) = node.op match {
        case Symbols.inc | Symbols.dec =>
          checkLvalueNode(node.expr, s"in unary ${node.op.name}")
          exprTy match {
            case exprTy: ArithmeticTy => exprTy

            case PtrTyBase(baseTy) if baseTy != VoidTy =>
              checkCompleteType(baseTy, s"in unary ${node.op.name}", node.expr.loc)
              exprTy

            case _ => errorInvalidOperand
          }

        case op => throw new UnsupportedOperationException(s"Type checking for unary postfix ${op.name} is not defined.")
      }

    case node: AstAddress =>
      checkLvalueNode(node.expr, "in & expr")
      typeMap(node) = getTy(node.expr) match {
        // void foo();
        // &foo == foo
        case exprTy@PtrTy(_: FunTy) => exprTy

        // int arr[5];
        // &arr == arr
        case exprTy: ArrayTy => exprTy

        case exprTy => PtrTy(exprTy)
      }

    // the * operator
    case node: AstDeref =>
      typeMap(node) = getTy(node.expr) match {
        // void foo();
        // *foo == foo
        case exprTy@PtrTy(_: FunTy) => exprTy

        case PtrTyBase(baseTy) => baseTy

        case exprTy =>
          errors += new Message(Error, s"cannot dereference non-pointer type ($exprTy)", node.loc)
          VoidTy
      }

    case node: AstIndex =>
      val exprTy = getTy(node.expr)
      // allow only char and int as an index (not double)
      checkIntegerType(getTy(node.index), "as index", node.index.loc)
      typeMap(node) = exprTy match {
        case PtrTyBase(_: FunTy) =>
          errors += new Message(Error, s"cannot index into pointer to function", node.loc)
          VoidTy

        case PtrTyBase(baseTy) => baseTy

        case exprTy =>
          errors += new Message(Error, s"expected array or pointer, got $exprTy", node.loc)
          VoidTy
      }

    case node: AstMember =>
      typeMap(node) = getTy(node.expr) match {
        case exprTy: StructTy => getFieldTy(exprTy, node.member, "in member access", node.loc)

        case exprTy =>
          errors += new Message(Error, s"expected struct type, got $exprTy", node.loc)
          VoidTy
      }

    case node: AstMemberPtr =>
      typeMap(node) = getTy(node.expr) match {
        case PtrTyBase(baseTy: StructTy) => getFieldTy(baseTy, node.member, "in member pointer access", node.loc)

        case baseTy =>
          errors += new Message(Error, s"expected pointer to a struct type, got $baseTy", node.loc)
          VoidTy
      }

    case node: AstCall =>
      val exprTy = getTy(node.expr)
      val argExprTys = node.args.map(getTy)

      typeMap(node) = exprTy match {
        case PtrTy(FunTy(returnTy, argTys)) =>
          if (argTys.size == argExprTys.size) {
            argExprTys.zip(argTys).zipWithIndex.foreach({ case ((argExprTy, argTy), i) =>
              checkCompatibleType(argExprTy, argTy, s"arg #$i", node.loc)
            })
          } else
            errors += new Message(Error, s"expected ${argTys.size} arguments, ${argExprTys.size} given", node.loc)
          returnTy

        case _ =>
          errors += new Message(Error, s"expected function pointer, got $exprTy", node.loc)
          VoidTy
      }

    case node: AstCast =>
      val newTy = getTy(node.newTy)
      val exprTy = getTy(node.expr)
      if (!newTy.isExplicitlyCastableFrom(exprTy))
        errors += new Message(Error, s"cannot cast $exprTy to $newTy", node.loc)
      typeMap(node) = newTy

    case node: AstWrite =>
      checkCompatibleType(getTy(node.expr), CharTy, "as argument to print", node.expr.loc)
      typeMap(node) = VoidTy

    case node: AstWriteNum =>
      checkCompatibleType(getTy(node.expr), IntTy, "as argument to printnum", node.expr.loc)
      typeMap(node) = VoidTy

    case node: AstRead =>
      typeMap(node) = CharTy
  }

  private def visitFunPtrDecl(node: AstFunPtrDecl): Unit = {
    val context = s"in definition of 'typedef ${node.symbol.name}'"
    val returnTy = getTy(node.returnTy)
    val argTys = node.argTys.map(getTy).toIndexedSeq

    checkCompleteType(returnTy, context + " (return type)", node.returnTy.loc)
    argTys.zipWithIndex.foreach({ case (argTy, i) =>
      checkCompleteType(argTy, context + s" (arg #$i)", node.loc)
    })

    typeMap(node) = PtrTy(FunTy(returnTy, argTys))

    namedTypeDeclMap.get(node.symbol) match {
      case Some(prevDecl) =>
        errors += new Message(Error, s"redefinition of 'typedef ${node.symbol.name}", node.loc)
        errors += new Message(Note, s"previous definition of 'typedef ${node.symbol.name}'", prevDecl.loc)

      case None =>
        namedTypeDeclMap(node.symbol) = node
    }
  }

  private def visitStructDecl(node: AstStructDecl): Unit = node.fields match {
    // a full definition
    case Some(fields) =>
      lazy val fieldTys = fields.map({ case (astTy, name) =>
        val fieldTy = getTy(astTy)
        checkCompleteType(fieldTy, s"in member '$name' of definition of 'struct ${node.symbol.name}'", astTy.loc)
        (fieldTy, name)
      }).toIndexedSeq

      namedTypeDeclMap.get(node.symbol) match {
        case Some(prevDecl: AstStructDecl) =>
          val prevTy = getTy(prevDecl).asInstanceOf[StructTy]
          typeMap(node) = prevTy
          if (prevTy.complete) {
            errors += new Message(Error, s"redefinition of 'struct ${node.symbol.name}'", node.loc)
            errors += new Message(Note, s"previous definition of 'struct ${node.symbol.name}'", prevDecl.loc)
          } else {
            prevTy.fields = Some(fieldTys) // finish the previous type
          }

        case Some(prevDecl) =>
          errors += new Message(Error, s"redefinition of 'struct ${node.symbol.name}' as different kind of type", node.loc)
          errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
          typeMap(node) = StructTy(Some(node.symbol))
        // don't bother checking fields

        case None =>
          namedTypeDeclMap(node.symbol) = node
          val structTy = StructTy(Some(node.symbol))
          typeMap(node) = structTy
          structTy.fields = Some(fieldTys)
      }

    // forward declaration
    case None => namedTypeDeclMap.get(node.symbol) match {
      case Some(prevDecl: AstStructDecl) =>
        typeMap(node) = getTy(prevDecl)

      case Some(prevDecl) =>
        errors += new Message(Error, s"redefinition of 'struct ${node.symbol.name}' as different kind of type", node.loc)
        errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
        typeMap(node) = StructTy(Some(node.symbol))

      case None =>
        namedTypeDeclMap(node.symbol) = node
        typeMap(node) = StructTy(Some(node.symbol))
    }
  }

  private def visitFunDecl(node: AstFunDecl): Unit = {
    val context = s"in definition of '${node.symbol.name}'"
    val returnTy = getTy(node.returnTy)
    val argTys = node.args.map({ case (argTy, _) => getTy(argTy) }).toIndexedSeq

    checkCompleteType(returnTy, context + " (return type)", node.returnTy.loc)
    argTys.zipWithIndex.foreach({ case (argTy, i) =>
      checkCompleteType(argTy, context + s" (arg #$i)", node.loc)
    })

    val funTy = FunTy(returnTy, argTys)
    curFunTyOption = Some(funTy)
    typeMap(node) = funTy // Set the type early to prevent infinite recursion if the function is used in the body.

    node.prevDecl.foreach({ case FunDecl(prevDecl) =>
      val prevTy = getTy(prevDecl)
      if (prevTy != funTy) {
        errors += new Message(Error, s"incompatible declaration of function '${node.symbol.name}' ($funTy) previously declared as $prevTy", node.loc)
        errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
      }
    })

    node.body.foreach(body => {
      // Check for multiple definitions of a function.
      val prevDeclWithBody = node.prevDecls.collectFirst({ case FunDecl(prevDecl) if prevDecl.hasBody => prevDecl })
      prevDeclWithBody.foreach(prevDecl => {
        errors += new Message(Error, s"redefinition of function '${node.symbol.name}' (${funTy})", node.loc)
        errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
      })

      getTy(body)
    })
    curFunTyOption = None
  }

  private def visitVarDecl(node: AstVarDecl): Unit = {
    val varTy = getTy(node.varTy)
    checkCompleteType(varTy, s"in declaration of variable '${node.symbol.name}'", node.varTy.loc)
    typeMap(node) = varTy

    // Check if the variable is compatible with its previous declaration.
    node.prevDecl.foreach({ case VarDecl(prevDecl) =>
      val prevTy = getTy(prevDecl)
      if (prevTy != varTy) {
        errors += new Message(Error, s"incompatible declaration of variable '${node.symbol.name}' ($varTy), previously declared as $prevTy", node.loc)
        errors += new Message(Note, s"previous declaration of '${node.symbol.name}'", prevDecl.loc)
      }
    })

    node.value.foreach(value => {
      // Check for multiple definitions of a variable.
      val prevDeclWithValue = node.prevDecls.collectFirst({ case VarDecl(prevDecl) if prevDecl.hasValue => prevDecl })
      prevDeclWithValue.foreach(prevDecl => {
        errors += new Message(Error, s"redefinition of variable '${node.symbol.name}' ($varTy)", node.loc)
        errors += new Message(Note, s"previous definition of '${node.symbol.name}'", prevDecl.loc)
      })

      // Check value type.
      val valueTy = getTy(value)
      if (!varTy.isAssignableFrom(valueTy))
        errors += new Message(Error, s"cannot assign value of type $valueTy to variable '${node.symbol.name}' of type $varTy", value.loc)
    })
  }
}