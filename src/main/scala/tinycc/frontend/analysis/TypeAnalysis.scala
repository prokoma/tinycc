package tinycc.frontend.analysis

//import scala.collection.mutable
//
//class TypeAnalysisException(message: String, loc: SourceLocation) extends ProgramException(message) {
//  override def format(reporter: Reporter): String = reporter.formatError("type checker", message, loc)
//}
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
//class TypeAnalysis(_declarations: Declarations) {
//  implicit private def declarations: Declarations = _declarations
//
//  import TypeAnalysisException._
//
//  implicit private val typeMap: mutable.Map[AST, Ty] = mutable.Map.empty
//
//  private var curFunTyOption: Option[FunTy] = None
//
//  private def curFunTy: FunTy = curFunTyOption.get
//
//  private val funDeclMap: mutable.Map[String, ASTFunDecl] = mutable.Map.empty
//
//  private val namedTypeMap: mutable.Map[String, Ty] = mutable.Map.empty
//
//  def analyze(node: AST): TypeMap = {
//    typeMap.clear()
//    funDeclMap.clear()
//    namedTypeMap.clear()
//    visitNode(node)
//    typeMap
//  }
//
//  private def resolveNodeTy(node: AST): Ty = {
//    if(!typeMap.contains(node))
//      visitNode(node)
//    typeMap(node)
//  }
//
//  private def visitNode(node: AST): Unit = node.asRealType match {
//    // Types
//
//    case f: ASTFunDecl =>
//      curFunTyOption = Some(FunTy(
//        resolveNodeTy(f.typeDecl),
//        f.args.map(arg => resolveNodeTy(arg._1)).toIndexedSeq))
//      typeMap(node) = curFunTy
//
//      funDeclMap.get(f.nameString) match {
//        case Some(prevDecl) =>
//          val prevTy = resolveNodeTy(prevDecl).asInstanceOf[FunTy]
//          if(prevTy != curFunTy)
//            throw errorIncompatibleFunTypes(curFunTy, prevTy, f.location())
//          if(prevDecl.body.isDefined && f.body.isDefined)
//            throw errorFunRedefinition(f, prevDecl, f.location())
//          if(prevDecl.body.isEmpty)
//            funDeclMap(f.nameString) = f
//
//        case None =>
//          funDeclMap(f.nameString) = f
//      }
//
//      if(f.body.isDefined)
//        f.body.foreach(visitNode)
//
//    case nt: ASTNamedType =>
//      typeMap(node) = nt.getName match {
//        case Symbols.kwInt => IntTy
//        case Symbols.kwVoid => VoidTy
//        case name if namedTypeMap.contains(name.name()) =>
//          namedTypeMap(name.name())
//        case _ => throw new TypeAnalysisException(s"Checker for ${nt.getName.name()} is not implemented.", nt.location())
//      }
//
//    case p: ASTPointerType =>
//      typeMap(node) = PtrTy(resolveNodeTy(p.getBase))
//
//    case a: ASTArrayType =>
//      a.getSize.asRealType match {
//        case size: ASTInteger =>
//          if(size.getValue <= 0)
//            throw errorInvalidArraySize(a.getSize)
//          val baseTy = resolveNodeTy(a.getBase)
//          if(baseTy.isInstanceOf[ArrayTy])
//            throw new NotImplementedError("Multi dimensional arrays are not implemented.")
//          typeMap(node) = ArrayTy(baseTy, size.getValue.toInt)
//
//        case _ =>
//          throw errorInvalidArraySize(a.getSize)
//      }
//
//    case fpd: ASTFunPtrDecl =>
//      typeMap(node) = PtrTy(FunTy(resolveNodeTy(fpd.returnType), fpd.argTypes.map(resolveNodeTy).toIndexedSeq))
//      namedTypeMap(fpd.nameString) = typeMap(node)
//
//    // Stmts
//
//    case vd: ASTVarDecl =>
//      typeMap(node) = VoidTy
//      vd.children.foreach(visitNode)
//      if (vd.value.nonEmpty)
//        assertTypeAssignableFrom(
//          resolveNodeTy(vd.varType),
//          resolveNodeTy(vd.value.get),
//          s"on the right side of variable declaration",
//          vd.location())
//
//    case i: ASTIf =>
//      typeMap(node) = VoidTy
//      assertCond(i.cond, "if")
//      i.children.foreach(visitNode)
//
//    case w: ASTWhile =>
//      typeMap(node) = VoidTy
//      assertCond(w.cond, "while")
//      w.children.foreach(visitNode)
//
//    case dw: ASTDoWhile =>
//      typeMap(node) = VoidTy
//      assertCond(dw.cond, "do..while")
//      dw.children.foreach(visitNode)
//
//    case f: ASTFor =>
//      typeMap(node) = VoidTy
//      f.cond.foreach(assertCond(_, "for"))
//      f.children.foreach(visitNode)
//
//    case s: ASTSwitch =>
//      typeMap(node) = VoidTy
//      assertCond(s.cond, "switch")
//      s.children.foreach(visitNode)
//
//    case b: ASTBlock =>
//      typeMap(node) = VoidTy
//      b.children.foreach(visitNode)
//
//    case _: ASTBreak | _: ASTContinue =>
//      typeMap(node) = VoidTy
//
//    case r: ASTReturn =>
//      typeMap(node) = VoidTy
//      r.children.foreach(visitNode)
//      assertTypeAssignableFrom(
//        curFunTy.returnTy,
//        r.value.map(resolveNodeTy).getOrElse(VoidTy),
//        s"as return value",
//        r.location())
//
//    case w: ASTWrite =>
//      typeMap(node) = VoidTy
//      assertTypeAssignableFrom(
//        IntTy,
//        resolveNodeTy(w.getValue),
//        s"as print argument",
//        w.getValue.location())
//
//    // Exprs
//
//    case a: ASTAssignment =>
//      assertLvalue(a.getLvalue, "on right hand side of =")
//      val lvalueTy = resolveNodeTy(a.getLvalue)
//      val valueTy = resolveNodeTy(a.getValue)
//      assertTypeAssignableFrom(lvalueTy, valueTy, "on right hand side of =", a.getValue.location())
//      typeMap(node) = valueTy
//
//    case _: ASTInteger =>
//      typeMap(node) = IntTy
//
//    case s: ASTSequence => // ASTSequence or ASTBlock
//      typeMap(node) = VoidTy
//      s.children.foreach(visitNode)
//
//    case b: ASTBinaryOp =>
//      visitBinaryOp(b)
//
//    case u: ASTUnaryOp =>
//      visitUnaryOp(u)
//
//    case up: ASTUnaryPostOp =>
//      visitUnaryPostOp(up)
//
//    case _: ASTRead =>
//      typeMap(node) = IntTy
//
//    case i: ASTIdentifier =>
//      typeMap(node) = i.decl match {
//        case FunDecl(node) => resolveNodeTy(node)
//        case VarDecl(node) => resolveNodeTy(node.getVarType)
//        case FunArgDecl(fun, index, _) => resolveNodeTy(fun).asInstanceOf[FunTy].argTys(index)
//      }
//
//    case a: ASTAddress =>
//      val targetTy = resolveNodeTy(a.getTarget)
//      assertLvalue(a.getTarget, "as argument of &")
//      typeMap(node) = PtrTy(targetTy)
//
//    case d: ASTDeref =>
//      typeMap(node) = resolveNodeTy(d.getTarget) match {
//        case PtrTy(targetTy) => targetTy
//        case ty => throw errorExpectedPtrType(ty, "in deref", d.location())
//      }
//
//    case c: ASTCall =>
//      resolveNodeTy(c.function) match {
//        case FunTy(returnTy, argTys) =>
//          typeMap(node) = returnTy
//          if(c.args.size != argTys.size)
//            throw errorExpectedNumArgs(argTys.size, c.args.size, "in call expression", c.function.location())
//          c.args.zip(argTys).zipWithIndex.foreach({ case ((arg, defTy), i) =>
//            val actualTy = resolveNodeTy(arg)
//            assertTypeAssignableFrom(defTy, actualTy, s"in $i-th argument of function call", arg.location())
//          })
//
//        case ty => throw errorExpectedFunType(ty, "in call expression", c.function.location())
//      }
//
//    case c: ASTCast =>
//      val targetTy = resolveNodeTy(c.getType)
//      val valueTy = resolveNodeTy(c.getValue)
//      if(targetTy.getCastModeFrom(valueTy).isEmpty)
//        throw errorInvalidCast(valueTy, targetTy, c.location())
//      typeMap(node) = targetTy
//
//    case i: ASTIndex =>
//      val baseTy = resolveNodeTy(i.getBase)
//      val indexTy = resolveNodeTy(i.getIndex)
////      assertLvalue(i.getBase, "as array access base")
//      typeMap(node) = baseTy match {
//        case PtrTy(targetTy) => targetTy
//        case ArrayTy(elemTy, _) => elemTy
//        case _ => throw errorExpectedPtrOrArray(baseTy, "as array access base", i.getBase.location())
//      }
//      assertTypeAssignableFrom(IntTy, indexTy, "in array access index", i.getIndex.location())
//
//
//    case n => throw new NotImplementedError(s"TypeChecker: $n")
//  }
//
//  private def isIntOrPtrTy(ty: Ty): Boolean = ty match {
//    case IntTy | PtrTy(_) => true
//    case _ => false
//  }
//
//  private def visitUnaryOp(node: ASTUnaryOp): Unit =
//    typeMap(node) = (node.getOp, resolveNodeTy(node.getArg)) match {
//      case (Symbols.add | Symbols.sub | Symbols.not | Symbols.neg, argTy@IntTy) =>
//        argTy
//
//      case (Symbols.add, argTy: PtrTy) =>
//        argTy
//
//      case (Symbols.inc | Symbols.dec, argTy) if isIntOrPtrTy(argTy) =>
//        assertLvalue(node.getArg, "as ++/-- arg")
//        argTy
//
//      case _ => throw new NotImplementedError(s"TypeChecker for ASTUnaryOp: '$node'")
//    }
//
//  private def visitUnaryPostOp(node: ASTUnaryPostOp): Unit =
//    typeMap(node) = (node.getOp, resolveNodeTy(node.getArg)) match {
//      case (Symbols.inc | Symbols.dec, argTy) if isIntOrPtrTy(argTy) =>
//        assertLvalue(node.getArg, "as ++/-- arg")
//        argTy
//
//      case _ => throw new NotImplementedError(s"TypeChecker for ASTUnaryPostOp: '$node'")
//    }
//
//  private def visitBinaryOp(node: ASTBinaryOp): Unit =
//    typeMap(node) = (node.getOp, resolveNodeTy(node.getLeft), resolveNodeTy(node.getRight)) match {
//      case (Symbols.add | Symbols.sub | Symbols.mul | Symbols.div | Symbols.bitAnd | Symbols.bitOr | Symbols.xor | Symbols.mod, leftTy@IntTy, rightTy) =>
//        if (leftTy != rightTy)
//          throw errorExpectedType(leftTy, rightTy, s"on the right side of ${node.getOp}", node.location())
//        leftTy
//
//      case (Symbols.add | Symbols.sub, leftTy: PtrTy, rightTy@IntTy) =>
//        leftTy
//
//      case (Symbols.lt | Symbols.gt | Symbols.gte | Symbols.lte | Symbols.eq | Symbols.neq, leftTy, rightTy) if isIntOrPtrTy(leftTy) =>
//        if (leftTy != rightTy)
//          throw errorExpectedType(leftTy, rightTy, s"on the right side of ${node.getOp}", node.location())
//        IntTy
//
//      case (Symbols.and | Symbols.or, leftTy@IntTy, rightTy) =>
//        if (leftTy != rightTy)
//          throw errorExpectedType(leftTy, rightTy, s"on the right side of ${node.getOp}", node.location())
//        IntTy
//
//      case _ => throw new NotImplementedError(s"TypeChecker for ASTBinaryOp: '$node'")
//    }
//
//  private def assertCond(cond: AST, label: String): Unit = resolveNodeTy(cond) match {
//    case IntTy =>
//    case condTy => throw errorExpectedType(IntTy, condTy, s"in $label condition", cond.location())
//  }
//
//  private def assertLvalue(node: AST, label: String): Unit = node.asRealType match {
//    case _: ASTIdentifier |  _: ASTDeref | _: ASTIndex =>
//
//    case n => errorExpectedLValue(n, label, node.location())
//  }
//
//  private def assertTypeAssignableFrom(varTy: Ty, actualTy: Ty, label: String, loc: SourceLocation): Unit =
//    if(!varTy.isAssignableFrom(actualTy))
//      throw errorExpectedType(varTy, actualTy, label, loc)
//}