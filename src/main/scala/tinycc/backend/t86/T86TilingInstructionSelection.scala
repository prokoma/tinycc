package tinycc.backend.t86

import tinycc.backend.t86.T86Opcode._
import tinycc.backend.{BackendException, TilingInstructionSelection}
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir._

import scala.annotation.tailrec
import scala.collection.mutable

trait T86TilingInstructionSelection extends TilingInstructionSelection {

  trait Context {
    def resolveVar[T](v: Var[AsmEmitter[T]], insn: Insn): T

    def emit(el: T86ProgramElement): Unit

    def emit(op: T86Opcode): Unit = emit(T86Insn(op))

    def emit(op: T86Opcode, operand: Operand): Unit = emit(T86Insn(op, operand))

    def emit(op: T86Opcode, operand0: Operand, operand1: Operand): Unit = emit(T86Insn(op, operand0, operand1))

    def freshLabel(): T86Label

    def getFunLabel(fun: IrFun): T86Label =
      T86Label(fun.name)

    def getBasicBlockLabel(bb: BasicBlock): T86Label =
      T86Label(bb.uniqueName)

    def voidReg(): Operand.Reg = Operand.BasicReg(0)

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

    def emitFunEpilogue(): Unit

    /** Can return either MemRegImm or Reg depending on calling conventions. */
    def resolveRetValueCallee(): Operand

    def resolveRetValueCaller(): Operand
  }

  trait T86Var[T] extends Var[AsmEmitter[T]] {
    override def resolveValue(insn: Insn): ValueTy =
      (ctx: Context) => ctx.resolveVar(this, insn)
  }

  case object RegVar extends T86Var[Operand.Reg]

  case object FRegVar extends T86Var[Operand.FReg]

  val variables: Seq[Var[_]] = Seq(RegVar, FRegVar)

  def pure[T](value: T): AsmEmitter[T] = (ctx: Context) => value

  def constImm(v: Long): AsmPat[Operand.Imm] = Pat(IImm)
    .filter({ case insn: IImmInsn => insn.value == v }) ^^ { case insn: IImmInsn => pure(Operand.Imm(insn.value)) }

  lazy val imm: AsmPat[Operand.Imm] = Pat(IImm) ^^ { case insn: IImmInsn => pure(Operand.Imm(insn.value)) }
  lazy val regOrImm: AsmPat[Operand] = imm | RegVar

  lazy val fimm: AsmPat[Operand.FImm] = Pat(FImm) ^^ { case insn: FImmInsn => pure(Operand.FImm(insn.value)) }
  lazy val freg: AsmPat[Operand.FReg] = FRegVar | (RegVar ^^ { reg => (ctx: Context) => ctx.copyToFreshFReg(reg(ctx)) }) // TODO: increase cost for casting between register types
  lazy val fregOrFimm: AsmPat[Operand] = freg | fimm
  lazy val regOrFregOrFimm: AsmPat[Operand] = RegVar | fregOrFimm

  lazy val memReg: AsmPat[Operand.MemReg] = RegVar ^^ { reg => (ctx: Context) => reg(ctx).mem }
  lazy val memAllocL: AsmPat[Operand.MemRegImm] = Pat(AllocL) ^^ { case insn: AllocLInsn => (ctx: Context) => ctx.resolveAllocL(insn) }
  lazy val memAllocG: AsmPat[Operand.MemImm] = Pat(AllocG) ^^ { case insn: AllocGInsn => (ctx: Context) => ctx.resolveAllocG(insn) }

  lazy val memAddr: AsmPat[Operand] = memReg | memAllocL | memAllocG
  lazy val memRegOrMemImm: AsmPat[Operand] = memReg | memAllocG

  protected def emitCmpAndReturnJmpOp(jmpOp: T86Opcode.CondJmpOp): ((Insn, AsmEmitter[Operand.Reg], AsmEmitter[Operand])) => AsmEmitter[T86Opcode.CondJmpOp] = {
    case (_, left, right) =>
      (ctx: Context) => {
        ctx.emit(CMP, left(ctx), right(ctx))
        jmpOp
      }
  }

  lazy val cmpInsn: AsmPat[CondJmpOp] = (
    Pat(CmpIEq, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JZ) |
      Pat(CmpINe, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JNE) |
      Pat(CmpULt, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JB) |
      Pat(CmpULe, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JBE) |
      Pat(CmpUGt, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JA) |
      Pat(CmpUGe, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JAE) |
      Pat(CmpSLt, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JL) |
      Pat(CmpSLe, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JLE) |
      Pat(CmpSGt, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JG) |
      Pat(CmpSGe, RegVar, regOrImm) ^^ emitCmpAndReturnJmpOp(T86Opcode.JGE) |

      // cmpieq (sub %1, %2), (iimm 0)
      Pat(CmpIEq, Pat(ISub, RegVar, regOrImm), constImm(0)) ^^ { case (_, (_, left, right), _) =>
        (ctx: Context) => {
          ctx.emit(CMP, left(ctx), right(ctx))
          T86Opcode.JZ
        }
      }
    )

  protected def emitFloatBinArithInsn(op: T86Opcode): ((Insn, AsmEmitter[Operand], AsmEmitter[Operand])) => AsmEmitter[Operand.FReg] = {
    case (_, left, right) =>
      (ctx: Context) => {
        val res = ctx.copyToFreshFReg(left(ctx))
        ctx.emit(op, res, right(ctx))
        res
      }
  }

  lazy val floatBinArithInsn: AsmPat[Operand.FReg] = (
    Pat(FAdd, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FADD) |
      Pat(FSub, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FSUB) |
      Pat(FMul, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FMUL) |
      Pat(FDiv, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FDIV)
    )

  protected def emitBinArithInsn(op: T86Opcode): ((Insn, AsmEmitter[Operand], AsmEmitter[Operand])) => AsmEmitter[Operand.Reg] = {
    case (_, left, right) =>
      (ctx: Context) => {
        val res = ctx.copyToFreshReg(left(ctx))
        ctx.emit(op, res, right(ctx))
        res
      }
  }

  protected def fregToReg(freg: AsmEmitter[Operand.FReg]): AsmEmitter[Operand.Reg] =
    (ctx: Context) => ctx.copyToFreshReg(freg(ctx))

  lazy val castSInt64ToDouble: AsmPat[Operand.FReg] = (
    Pat(SInt64ToDouble, RegVar) ^^ { case (_, reg) =>
      (ctx: Context) => {
        val res = ctx.freshFReg()
        ctx.emit(EXT, res, reg(ctx))
        res
      }
    } |
      Pat(SInt64ToDouble, Pat(IImm)) ^^ { case (_, iimm: IImmInsn) =>
        (ctx: Context) => ctx.copyToFreshFReg(Operand.FImm(iimm.value.toDouble))
      }
    )

  lazy val call: AsmPat[Operand] = CallInsnPat(RegVar) ^^ { case (insn, args) =>
    (ctx: Context) => {
      args.reverse.foreach(arg => {
        ctx.emit(PUSH, arg(ctx))
      })
      ctx.emit(CALL, ctx.getFunLabel(insn.targetFun.get).toOperand)
      ctx.emit(ADD, Operand.SP, Operand.Imm(args.size)) // pop arguments
      ctx.resolveRetValueCaller()
    }
  }

  lazy val callPtr = CallPtrInsnPat(RegVar, RegVar) ^^ { case (insn, ptr, args) =>
    (ctx: Context) => {
      args.reverse.foreach(arg => {
        ctx.emit(PUSH, arg(ctx))
      })
      ctx.emit(CALL, ptr(ctx))
      ctx.emit(ADD, Operand.SP, Operand.Imm(args.size)) // pop arguments
      ctx.resolveRetValueCaller()
    }
  }

  val rules: Seq[GenRule[_]] = Seq(

    GenRule(RegVar, Pat(IAdd, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.ADD)),
    GenRule(RegVar, Pat(ISub, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.SUB)),
    GenRule(RegVar, Pat(IAnd, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.AND)),
    GenRule(RegVar, Pat(IOr, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.OR)),
    GenRule(RegVar, Pat(IXor, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.XOR)),
    GenRule(RegVar, Pat(IShl, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.LSH)),
    GenRule(RegVar, Pat(IShr, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.RSH)),
    GenRule(RegVar, Pat(UMul, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.MUL)),
    GenRule(RegVar, Pat(UDiv, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.DIV)),
    GenRule(RegVar, Pat(SMul, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.IMUL)),
    GenRule(RegVar, Pat(SDiv, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.IDIV)),

    GenRule(FRegVar, floatBinArithInsn),
    GenRule(RegVar, floatBinArithInsn ^^ fregToReg), // TODO: increase cost for casting between register types

    // %2 = cmpieq %0, %1
    GenRule(RegVar, cmpInsn ^^ { cmpInsn =>
      (ctx: Context) => {
        val lab = ctx.freshLabel()
        val res = ctx.freshReg()
        ctx.emit(XOR, res, res)
        val jmpOp = cmpInsn(ctx)
        ctx.emit(jmpOp.neg, lab.toOperand)
        ctx.emit(MOV, res, Operand.Imm(1))
        ctx.emit(lab)
        res
      }
    }),

    // %2 = condbr (cmpieq %0, %1)
    GenRule(RegVar, Pat(CondBr, cmpInsn) ^^ { case (condBrInsn: CondBrInsn, cmpInsn) =>
      (ctx: Context) => {
        val jmpOp = cmpInsn(ctx)
        ctx.emit(jmpOp, ctx.getBasicBlockLabel(condBrInsn.trueBlock.get).toOperand)
        ctx.emit(JMP, ctx.getBasicBlockLabel(condBrInsn.falseBlock.get).toOperand)
        ctx.voidReg()
      }
    }),

    GenRule(RegVar, Pat(AllocL) ^^ { case insn: AllocLInsn =>
      (ctx: Context) =>
        val res = ctx.freshReg()
        ctx.emit(LEA, res, ctx.resolveAllocL(insn))
        res
    }),
    GenRule(RegVar, Pat(AllocG) ^^ { case insn: AllocGInsn =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolveAllocG(insn))
    }),

    GenRule(RegVar, Pat(Load, memAddr) ^^ { case (_, memAddr) =>
      (ctx: Context) => ctx.copyToFreshReg(memAddr(ctx))
    }),
    GenRule(FRegVar, Pat(Load, memRegOrMemImm) ^^ { case (_, memAddr) =>
      (ctx: Context) => ctx.copyToFreshFReg(memAddr(ctx))
    }),

    GenRule(RegVar, Pat(LoadArg) ^^ { case insn: LoadArgInsn =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolveLoadArg(insn))
    }),
    GenRule(RegVar, Pat(Store, memAddr, regOrImm | freg) ^^ { case (_, memAddr, value) =>
      (ctx: Context) =>
        ctx.emit(MOV, memAddr(ctx), value(ctx))
        ctx.voidReg()
    }),
    GenRule(RegVar, Pat(IImm) ^^ { case insn: IImmInsn =>
      (ctx: Context) => ctx.copyToFreshReg(Operand.Imm(insn.value))
    }),
    GenRule(FRegVar, Pat(FImm) ^^ { case insn: FImmInsn =>
      (ctx: Context) => ctx.copyToFreshFReg(Operand.FImm(insn.value))
    }),
    GenRule(RegVar, Pat(FImm) ^^ { case insn: FImmInsn =>
      (ctx: Context) => ctx.copyToFreshFReg(Operand.FImm(insn.value))
    } ^^ fregToReg),

    GenRule(RegVar, Pat(GetElementPtr, RegVar, RegVar) ^^ { case (insn: GetElementPtrInsn, base, index) =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        if (insn.fieldIndex == 0)
          ctx.emit(LEA, res, Operand.MemRegRegScaled(base(ctx), index(ctx), (insn.elemTy.sizeBytes / 8).ceil.toLong))
        else
          ???
        res
      }
    }),

    GenRule(RegVar, call ^^ { retValue => (ctx: Context) => ctx.copyToFreshReg(retValue(ctx)) }),
    GenRule(FRegVar, call ^^ { retValue => (ctx: Context) => ctx.copyToFreshFReg(retValue(ctx)) }),

    GenRule(RegVar, callPtr ^^ { retValue => (ctx: Context) => ctx.copyToFreshReg(retValue(ctx)) }),
    GenRule(FRegVar, callPtr ^^ { retValue => (ctx: Context) => ctx.copyToFreshFReg(retValue(ctx)) }),

    GenRule(RegVar, Pat(PutChar, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTCHAR, reg(ctx))
        ctx.voidReg()
      }
    }),
    GenRule(RegVar, Pat(PutNum, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTNUM, reg(ctx))
        ctx.voidReg()
      }
    }),
    GenRule(RegVar, Pat(GetChar) ^^ { insn =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(GETCHAR, res)
        res
      }
    }),
    GenRule(RegVar, Pat(DoubleToSInt64, freg) ^^ { case (_, freg) =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(NRW, res, freg(ctx))
        res
      }
    }),

    // Cast
    GenRule(FRegVar, castSInt64ToDouble),
    GenRule(RegVar, castSInt64ToDouble ^^ fregToReg),

    // Phi
    GenRule(RegVar, Pat(Ret, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(MOV, ctx.resolveRetValueCallee(), reg(ctx))
        ctx.emitFunEpilogue()
        ctx.voidReg()
      }
    }),
    GenRule(RegVar, Pat(RetVoid) ^^ { _ =>
      (ctx: Context) =>
        ctx.emitFunEpilogue()
        ctx.voidReg()
    }),
    GenRule(RegVar, Pat(Halt) ^^ { _ =>
      (ctx: Context) => {
        ctx.emit(HALT)
        ctx.voidReg()
      }
    }),
    GenRule(RegVar, Pat(Br) ^^ { case insn: BrInsn =>
      (ctx: Context) => {
        ctx.emit(JMP, ctx.getBasicBlockLabel(insn.succBlock.get).toOperand)
        ctx.voidReg()
      }
    }),

    GenRule(RegVar, Pat(CondBr, RegVar) ^^ { case (insn: CondBrInsn, reg) =>
      (ctx: Context) => {
        ctx.emit(CMP, reg(ctx), Operand.Imm(0))
        ctx.emit(JZ, ctx.getBasicBlockLabel(insn.falseBlock.get).toOperand)
        ctx.emit(JMP, ctx.getBasicBlockLabel(insn.trueBlock.get).toOperand)
        ctx.voidReg()
      }
    }),

  ).flatMap(_.flatten)

  Console.err.println(s"Using ${rules.size} rules")

  // we need to convert an IrProgram into list of T86 instructions with virtual registers
  // at first, we are doing instruction selection locally for each block - the only exceptions are allocl and allocg, but those don't emit any code
  // so, take each basic block and loop over instructions in reverse
  // - try to match tiles, now the tiles contain variables (register types) - what now?
  //   - currently all tiles take and return values in ordinary registers, so that shouldn't be a problem
  //   - later - match tiles using DP in the orther they are writeen in basic block (bottom up data flow),
  //      group matches by result type and store the best for each type
  // each instruction now has a matched tile, but not all of them will be used
  // loop over instructions in reverse order and call code gen functions

  // if a nonterminal is accessed, call code gen for that instruction
  // mark all processed instructions/tiles as visited and skip already visited insns in the loop
  // hrm, this won't work, because there can be multiple interleaved df trees in one basic block and we need to keep the order of execution
  //   - reordering of ir insns should be part of middleend

  // so loop again over the basic block in reverse and examine the chosen tiles
  // mark all interior instructions as covered, those are then skipped in the loop
  // mark the root and all leafs (vars) as required

  // ok, now ctx.emit() has been called for each instruction in order we have ASM code for a basic block
  // process a whole function like this, now we know about all allocl's and allocg's
  // the result of this thing is a vector of instructions for each basic block
  // use labels instead of patching
  // context is reset for each basic block
}

class MaximalMunchT86InstructionSelection(program: IrProgram) extends T86InstructionSelection(program) with T86TilingInstructionSelection {
  private val tileResults = mutable.Map.empty[(Var[_], Insn), Any]

  private var globalsSize: Long = 0
  private val globalsMap = mutable.Map.empty[AllocGInsn, Long]

  lazy val result: Either[BackendException, T86Program] = {
    val textSection = T86SectionLabel("text") +: program.funs.map(tileIrFun).reduce(_ ++ _)
    Right(textSection)
  }

  def getSizeWords(ty: IrTy): Long =
    (ty.sizeBytes / 8).ceil.toLong

  private lazy val sortedRules = rules.sortBy(r => -r.rhs.size)

  def tileIrFun(fun: IrFun): T86Program = {
    val codeBuilder = Seq.newBuilder[T86ProgramElement]

    var localsSize: Long = 0
    val localsMap = mutable.Map.empty[AllocLInsn, Long]

    var argsSize: Long = 0
    val argsMap = fun.argTys.map(ty => {
      val oldSize = argsSize
      argsSize += getSizeWords(ty)
      oldSize
    })

    val epilogueMarker = T86Comment("EPILOGUE_HERE")
    val backupRegs: mutable.Buffer[Operand] = mutable.Buffer.empty

    val ctx = new Context {
      private var nextReg: Long = 1 // 0 is reserved for return value
      private var nextFreg: Long = 0
      private var nextLabel = 0

      override def resolveVar[T](v: Var[AsmEmitter[T]], insn: Insn): T = tileResults((v, insn)).asInstanceOf[T]

      override def emit(el: T86ProgramElement): Unit =
        codeBuilder += el

      override def freshReg(): Operand.Reg = {
        nextReg += 1
        val reg = Operand.BasicReg(nextReg - 1)
        backupRegs += reg
        reg
      }

      override def freshFReg(): Operand.FReg = {
        nextFreg += 1
        val reg = Operand.BasicFReg(nextFreg - 1)
        backupRegs += reg
        reg
      }

      override def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm = {
        val offset = localsMap.getOrElseUpdate(insn, {
          localsSize += getSizeWords(insn.varTy)
          localsSize
        })
        Operand.MemRegImm(Operand.BP, -offset)
      }

      override def resolveAllocG(insn: AllocGInsn): Operand.MemImm = {
        val offset = globalsMap.getOrElseUpdate(insn, {
          val oldSize = globalsSize
          globalsSize += getSizeWords(insn.varTy)
          oldSize
        })
        Operand.MemImm(offset)
      }

      /** Can return either MemRegImm or Reg depending on calling conventions. */
      override def resolveLoadArg(insn: LoadArgInsn): Operand = {
        val offset = argsMap(insn.index)
        Operand.MemRegImm(Operand.BP, offset + 2) // return address + stored BP
      }

      override def emitFunEpilogue(): Unit =
        codeBuilder += epilogueMarker

      /** Can return either MemRegImm or Reg depending on calling conventions. */
      override def resolveRetValueCallee(): Operand =
        Operand.BasicReg(0)

      override def resolveRetValueCaller(): Operand =
        Operand.BasicReg(0)

      override def freshLabel(): T86Label = {
        nextLabel += 1
        T86Label(fun.name + "$tmp" + (nextLabel - 1))
      }
    }

    fun.basicBlocks.foreach(bb => tileBasicBlock(bb, ctx))

    val prologueBuilder = Seq.newBuilder[T86ProgramElement]
    prologueBuilder += T86Label(fun.name)
    prologueBuilder += T86Insn(PUSH, Operand.BP)
    prologueBuilder += T86Insn(MOV, Operand.BP, Operand.SP)
    prologueBuilder += T86Insn(SUB, Operand.SP, Operand.Imm(localsSize))
    // TODO: if arguments are passed via registers, copy them to fresh registers
    backupRegs.foreach({
      case reg: Operand.Reg =>
        prologueBuilder += T86Insn(PUSH, reg)
      case freg: Operand.FReg =>
        prologueBuilder += T86Insn(FPUSH, freg)
      case op =>
        throw new IllegalArgumentException(op.toString)
    })

    val epilogueBuilder = Seq.newBuilder[T86ProgramElement]
    backupRegs.reverse.foreach({
      case reg: Operand.Reg =>
        epilogueBuilder += T86Insn(POP, reg)
      case freg: Operand.FReg =>
        epilogueBuilder += T86Insn(FPOP, freg)
      case op =>
        throw new IllegalArgumentException(op.toString)
    })
    epilogueBuilder += T86Insn(MOV, Operand.SP, Operand.BP)
    epilogueBuilder += T86Insn(POP, Operand.BP)
    epilogueBuilder += T86Insn(RET)

    val epilogue = epilogueBuilder.result()
    prologueBuilder.result() ++ codeBuilder.result().flatMap {
      case elem if elem == epilogueMarker => epilogue
      case elem => Seq(elem)
    }
  }

  def tileBasicBlock(bb: BasicBlock, ctx: Context): Unit = {
    val allCoveredInsns = mutable.Set.empty[Insn]
    val allRequiredInsns = mutable.Set.empty[(Var[_], Insn)]
    val tileMap = mutable.Map.empty[(Var[_], Insn), GenRule.Match[_]]

    ctx.emit(T86Label(bb.fun.name + "$" + bb.name))

    // instructions in a basic block are topologically sorted
    // greedily cover instructions with tiles
    bb.body.reverse.foreach(insn => {
      val requiredVars = variables.filter(v => allRequiredInsns.contains((v, insn)))

      if (requiredVars.nonEmpty) {
        if (requiredVars.size > 1 && insn.hasSideEffects)
          throw new BackendException(s"Insn $insn is required by multiple tiles with different types, but has side effects.")

        requiredVars.foreach(v => {
          val varRules = sortedRules.filter(_.v == v)
          varRules.view.flatMap(_(insn)).headOption match {
            case Some(m) =>
              tileMap((m.rule.v, insn)) = m
              allCoveredInsns ++= m.coveredInsns
              allRequiredInsns ++= m.requiredInsns

            case None =>
              throw new BackendException(s"Failed to cover $insn as $v (tried ${varRules.size} rules)")
          }
        })
      } else if (!allCoveredInsns.contains(insn)) {
        sortedRules.view.flatMap(_(insn)).headOption match {
          case Some(m) =>
            tileMap((m.rule.v, insn)) = m
            allCoveredInsns ++= m.coveredInsns
            allRequiredInsns ++= m.requiredInsns

          case None =>
            throw new BackendException(s"Failed to cover $insn (tried ${sortedRules.size} rules)")
        }
      }
    })

    // now loop in the program order and generate code for the matched tiles
    bb.body.foreach(insn => {
      variables.foreach(v => tileMap.get((v, insn)) match {
        case Some(m) =>
          tileResults((v, insn)) = m.value(ctx)

        case None =>
      })
    })
  }
}

//class DynamicProgrammingT86InstructionSelection(program: IrProgram) extends T86InstructionSelection(program) with T86TilingInstructionSelection {
//  lazy val result: Either[BackendException, T86Listing] = ???
//}
//
//class NOLTIST86InstructionSelection(program: IrProgram) extends T86InstructionSelection(program) with T86TilingInstructionSelection {
//  lazy val result: Either[BackendException, T86Listing] = ???
//}