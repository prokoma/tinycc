package tinycc.backend.t86.insel

import tinycc.backend.TilingInstructionSelection
import tinycc.backend.t86.T86Opcode._
import tinycc.backend.t86.T86Utils.buildArgsMap
import tinycc.backend.t86._
import tinycc.common.ir.IrOpcode.{AllocG, AllocL, FImm, GetElementPtr, GetFunPtr, IImm, SizeOf}
import tinycc.common.ir._
import tinycc.util.{Logging, NameGen}

import scala.collection.mutable

abstract class T86TilingInstructionSelection(program: IrProgram) extends T86InstructionSelection(program) with TilingInstructionSelection {

  trait Context {
    def resolveVar[T](variable: Var[AsmEmitter[T]], insn: Insn): T

    def emit(el: T86ListingElement): Unit

    def emit(op: T86Opcode.NullaryOp): Unit = emit(T86Insn(op))

    def emit(op: T86Opcode.UnaryOp, operand: Operand): Unit = emit(T86Insn(op, operand))

    def emit(op: T86Opcode.BinaryOp, operand0: Operand, operand1: Operand): Unit = emit(T86Insn(op, operand0, operand1))

    def freshLabel(): T86Label = freshLabel("asm")

    def freshLabel(prefix: String): T86Label

    def getBasicBlockLabel(bb: BasicBlock): T86Label = T86Label(bb.uniqueName)

    def getFunLabel(fun: IrFun): T86Label = getBasicBlockLabel(fun.entryBlock)

    def freshReg(): Operand.Reg

    def copyToFreshReg(op: Operand): Operand.Reg = {
      val res = freshReg()
      emit(MOV, res, op)
      res
    }

    def freshFReg(): Operand.FReg

    def copyToFreshFReg(op: Operand): Operand.FReg = {
      op match {
        case _: Operand.FImm | _: Operand.FReg | _: Operand.Reg | _: Operand.MemImm | _: Operand.MemReg =>
        case _ =>
          throw new AssertionError(s"unsupported operand type for MOV Fx: $op") // TODO: maybe check operand in emit
      }

      val res = freshFReg()
      emit(MOV, res, op)
      res
    }

    def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm

    def resolveAllocG(insn: AllocGInsn): Operand.MemImm

    /** Can return either MemRegImm or Reg depending on calling conventions. */
    def resolveLoadArg(insn: LoadArgInsn): Operand

    def resolvePhiReg(variable: AsmVar[Operand.Reg], insn: PhiInsn): Operand.Reg

    def resolvePhiFReg(variable: AsmVar[Operand.FReg], insn: PhiInsn): Operand.FReg
  }

  trait T86Var[+T] extends AsmVar[T] {
    override def resolveValue(insn: Insn): AsmEmitter[T] =
      (ctx: Context) => ctx.resolveVar(this, insn)
  }

  private trait T86ListingBuilder {
    private val code = Seq.newBuilder[T86ListingElement]

    def emit(el: T86ListingElement): Unit = code += el

    def ++=(els: IterableOnce[T86ListingElement]): Unit = code ++= els

    def result(): T86Listing = code.result()
  }

  protected def compileIrFun(fun: IrFun, programBuilder: T86ProgramBuilder): T86Fun = {
    val tileMap = getTileMapForFun(fun)

    val argsMap = buildArgsMap(fun.argTys, 2)
    val funBuilder = new T86FunBuilder(fun)

    /** Holds the results (value of T86Var - usually register number) for each root instruction. */
    val tileResults = mutable.Map.empty[Insn, Any]
    /** Holds the code that was emitted by each tile. */
    val tileCode = mutable.Map.empty[Insn, T86Listing]
    val basicBlockPhis = mutable.Map.empty[BasicBlock, Seq[(AsmVar[_], Insn, Operand)]].withDefaultValue(Seq.empty)
    val bbNameGens = mutable.Map.empty[BasicBlock, NameGen]

    var openInsns = Set.empty[Insn]

    def _resolveVar[T](variable: AsmVar[T], insn: Insn, onlyAlreadyResolved: Boolean = false): T = {
      assert(tileMap(insn).variable == variable, s"requested unplanned $variable for $insn")
      tileResults.getOrElseUpdate(insn, {
        assert(!onlyAlreadyResolved, s"missing value for $variable for $insn")
        assert(!openInsns.contains(insn), s"circular reference to $insn ($openInsns)")
        openInsns += insn

        log(s"resolving $variable for $insn")
        val ctx = newBuilderContext(insn.basicBlock)
        val result = tileMap(insn).value(ctx)
        tileResults(insn) = result
        tileCode(insn) = ctx.result()

        openInsns -= insn
        result
      }).asInstanceOf[T]
    }

    def newBuilderContext(bb: BasicBlock): Context with T86ListingBuilder = new Context with T86ListingBuilder {
      override def resolveVar[T](variable: Var[AsmEmitter[T]], insn: Insn): T = {
        val tileMatch = tileMap(insn)
        if(tileMatch.variable != variable) {
          log(s"casting result of $insn from ${tileMatch.variable} to $variable")
          emitCastFromTo(_resolveVar(tileMatch.variable, insn), tileMatch.variable, variable)(this)
        } else
          _resolveVar(tileMatch.variable, insn).asInstanceOf[T]
      }

      override def freshLabel(prefix: String): T86Label = {
        val nameGen = bbNameGens.getOrElseUpdate(bb, new NameGen)
        T86Label(bb.uniqueName + "$" + nameGen(prefix))
      }

      override def freshReg(): Operand.Reg = funBuilder.freshReg()

      override def freshFReg(): Operand.FReg = funBuilder.freshFReg()

      override def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm = {
        val res = funBuilder.resolveAllocL(insn)
        emit(T86Comment(s"$res -> %${insn.name}"))
        res
      }

      override def resolveAllocG(insn: AllocGInsn): Operand.MemImm = {
        val res = programBuilder.resolveAllocG(insn)
        emit(T86Comment(s"$res -> %${insn.name}"))
        res
      }

      /** Can return either MemRegImm or Reg depending on calling conventions. */
      override def resolveLoadArg(insn: LoadArgInsn): Operand = {
        val res = argsMap(insn.index)
        emit(T86Comment(s"$res -> arg #${insn.index}"))
        res
      }

      override def resolvePhiReg(variable: AsmVar[Operand.Reg], insn: PhiInsn): Operand.Reg = {
        val res = freshReg()
        insn.args.foreach({ case (argInsn, _) =>
          basicBlockPhis(argInsn.basicBlock) = basicBlockPhis(argInsn.basicBlock).appended((variable, argInsn, res)) // i want the result of variable for argInsn copied into res
        })
        res
      }

      override def resolvePhiFReg(variable: AsmVar[Operand.FReg], insn: PhiInsn): Operand.FReg = {
        val res = freshFReg()
        insn.args.foreach({ case (argInsn, _) =>
          basicBlockPhis(argInsn.basicBlock) = basicBlockPhis(argInsn.basicBlock).appended((variable, argInsn, res)) // i want the result of variable for argInsn copied into res
        })
        res
      }
    }

    log(s"resolving insns in $fun")
    fun.insns.foreach(insn => tileMap.get(insn) match {
      case Some(GenRule.Match(rule, _)) => _resolveVar(rule.variable, insn)
      case _ =>
    })

    fun.basicBlocks.foreach(bb => {
      val bodyBuilder = IndexedSeq.newBuilder[T86ListingElement]

      val prologueCtx = newBuilderContext(bb)
      prologueCtx.emit(prologueCtx.getBasicBlockLabel(bb))
      if (bb.isFunEntryBlock) {
        prologueCtx.emit(T86SpecialLabel.FunPrologueMarker)
        prologueCtx.emit(T86Comment(s"${bb.fun.name} prologue end"))

        log(s"emitting code for globals")
        val externalGlobals = fun.program.globals.filter(_.fun != fun)
        prologueCtx ++= externalGlobals.flatMap(tileCode.getOrElse(_, Seq.empty))
      }
      bodyBuilder ++= prologueCtx.result()

      log(s"emitting code for $bb")
      bb.body.foreach(insn => {
        if (insn.isInstanceOf[TerminatorInsn]) {
          // emit all MOVs for Phis just before terminator, so their live range is shorter
          basicBlockPhis(bb).foreach({ case (variable, argInsn, dest) =>
            val src = _resolveVar(variable.asInstanceOf[AsmVar[Operand]], argInsn, onlyAlreadyResolved = true)
            (dest, src) match {
              case (dest: Operand.Reg, src: Operand.Reg) => bodyBuilder += T86Insn(MOV, dest, src)
              case (dest: Operand.FReg, src: Operand.FReg) => bodyBuilder += T86Insn(MOV, dest, src)
              case _ => throw new UnsupportedOperationException(s"cannot move $src into $dest")
            }
          })
        }

        tileCode.get(insn) match {
          case Some(tileCode) => bodyBuilder ++= tileCode
          case None =>
        }
      })

      funBuilder.appendBlock(new T86BasicBlock(bodyBuilder.result(), bb))
    })

    funBuilder.result()
  }

  override def result(): T86Program = {
    val programBuilder = new T86ProgramBuilder(program)

    log(s"using ${rules.size} rules")

    // allocate memory for globals in the same order as they are defined in the program
    program.globals.foreach(programBuilder.resolveAllocG)

    program.funs.foreach(fun => programBuilder.appendFun(compileIrFun(fun, programBuilder)))
    programBuilder.result()
  }

  override def name: String = "T86TilingInstructionSelection"

  override def canCoverByMultipleTiles(insn: Insn): Boolean = insn.op match {
    case AllocL | AllocG => true
    case IImm | FImm | SizeOf | GetFunPtr | GetElementPtr => true
    case _ => false
  }
}