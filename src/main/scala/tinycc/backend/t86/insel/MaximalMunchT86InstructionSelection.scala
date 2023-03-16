//package tinycc.backend.t86.insel
//
//import tinycc.backend.BackendException
//import tinycc.backend.t86.T86Utils.buildArgsMap
//import tinycc.backend.t86._
//import tinycc.common.ir._
//import tinycc.util.NameGen
//
//import scala.collection.mutable
//
//class MaximalMunchT86InstructionSelection(program: IrProgram) extends T86TilingInstructionSelection(program) {
//  private val tileResults = mutable.Map.empty[(Var[_], Insn), Any]
//  private val programBuilder = new T86ProgramBuilder(Some(program))
//  private var globals: Seq[AllocGInsn] = _
//
//
//
//  def result(): T86Program = {
//    globals = program.globals.toSeq // cache
//    program.funs.foreach(tileIrFun)
//    programBuilder.result()
//  }
//
//  def tileIrFun(fun: IrFun): Unit = {
//    tileResults.clear()
//
//    val funBuilder = new T86FunBuilder(Some(fun))
//    val argsMap = buildArgsMap(fun.argTys, 2)
//    fun.basicBlocks.foreach(bb => tileBasicBlock(bb, funBuilder, argsMap))
//    programBuilder.appendFun(funBuilder.result())
//  }
//
//  def tileBasicBlock(bb: BasicBlock, funBuilder: T86FunBuilder, argsMap: IndexedSeq[Operand.MemRegImm]): Unit = {
//    val bodyBuilder = IndexedSeq.newBuilder[T86ListingElement]
//
//    val ctx = new Context {
//      private val labelNameGen = new NameGen
//
//      override def resolveVar[T](variable: Var[AsmEmitter[T]], insn: Insn): T = tileResults((variable, insn)).asInstanceOf[T]
//
//      override def emit(el: T86ListingElement): Unit = bodyBuilder += el
//
//      override def freshLabel(name: String): T86Label = T86Label(bb.uniqueName + "$" + labelNameGen(name))
//
//      override def freshReg(): Operand.Reg = funBuilder.freshReg()
//
//      override def freshFReg(): Operand.FReg = funBuilder.freshFReg()
//
//      override def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm = {
//        val res = funBuilder.resolveAllocL(insn)
//        emit(T86Comment(s"$res -> ${insn.name}"))
//        res
//      }
//
//      override def resolveAllocG(insn: AllocGInsn): Operand.MemImm = {
//        val res = programBuilder.resolveAllocG(insn)
//        emit(T86Comment(s"$res -> ${insn.name}"))
//        res
//      }
//
//      /** Can return either MemRegImm or Reg depending on calling conventions. */
//      override def resolveLoadArg(insn: LoadArgInsn): Operand = {
//        val res = argsMap(insn.index)
//        emit(T86Comment(s"$res -> arg ${insn.index}"))
//        res
//      }
//    }
//
//    val allCoveredInsns = mutable.Set.empty[Insn]
//    val allRequiredInsns = mutable.Set.empty[(Var[_], Insn)]
//    val tileMap = mutable.Map.empty[(Var[_], Insn), GenRule.Match[_]]
//
//    ctx.emit(ctx.getBasicBlockLabel(bb))
//    if (bb.isFunEntryBlock) {
//      ctx.emit(T86SpecialLabel.FunPrologueMarker)
//      ctx.emit(T86Comment(s"${bb.fun.name} prologue end"))
//    }
//
//    def usedInOtherBasicBlocks(insn: Insn): Boolean =
//      insn.uses.exists({
//        case use: OperandRef => use.insn.basicBlock != bb
//        case _ => true // just in case, return true
//      })
//
//    def usedInThisFun(insn: Insn): Boolean =
//      insn.uses.exists({
//        case use: OperandRef => use.insn.fun == bb.fun
//        case _ => true
//      })
//
//    // pretend that we see all global variables again at the start of each function
//    val body = if(bb.isFunEntryBlock)
//      globals.filter(usedInThisFun) ++ bb.body
//    else
//      bb.body
//
//    // instructions in a basic block are topologically sorted
//    // greedily cover instructions with tiles
//    body.reverse.foreach(insn => {
//      val requiredVars = variables.filter(v => allRequiredInsns.contains((v, insn)))
//
//      if (requiredVars.nonEmpty) {
//        if (requiredVars.size > 1 && insn.hasSideEffects)
//          throw new BackendException(s"Insn $insn is required by multiple tiles with different types, but has side effects.")
//
//        requiredVars.foreach(v => {
//          val varRules = sortedRules.filter(_.variable == v)
//          varRules.view.flatMap(_(insn)).headOption match {
//            case Some(m) =>
//              tileMap((m.rule.variable, insn)) = m
//              allCoveredInsns ++= m.coveredInsns
//              allRequiredInsns ++= m.requiredInsns
//
//            case None =>
//              throw new BackendException(s"Failed to cover $insn as $v (tried ${varRules.size} rules)")
//          }
//        })
//      } else if (!allCoveredInsns.contains(insn) || usedInOtherBasicBlocks(insn)) {
//        sortedRules.view.flatMap(_(insn)).headOption match {
//          case Some(m) =>
//            tileMap((m.rule.variable, insn)) = m
//            allCoveredInsns ++= m.coveredInsns
//            allRequiredInsns ++= m.requiredInsns
//
//          case None =>
//            throw new BackendException(s"Failed to cover $insn (tried ${sortedRules.size} rules)")
//        }
//      }
//    })
//
//    // now loop in the program order and generate code for the matched tiles
//    body.foreach(insn => {
//      variables.foreach(v => tileMap.get((v, insn)) match {
//        case Some(m) =>
//          tileResults((v, insn)) = m.value(ctx)
//
//        case None =>
//      })
//    })
//
//    funBuilder.appendBlock(new T86BasicBlock(bodyBuilder.result(), Some(bb)))
//  }
//}