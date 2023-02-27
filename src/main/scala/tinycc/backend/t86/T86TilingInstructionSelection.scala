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

    def freshReg(): Operand.Reg

    def copyToFreshReg(op: Operand): Operand.Reg = {
      val res = freshReg()
      emit(MOV, res, op)
      res
    }

    def freshFReg(): Operand.FReg

    def copyFReg(freg: Operand.FReg): Operand.FReg = {
      val res = freshFReg()
      emit(MOV, res, freg)
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

  protected def emitBinArithInsn(op: T86Opcode): ((Insn, AsmEmitter[Operand.Reg], AsmEmitter[Operand])) => AsmEmitter[Operand.Reg] = {
    case (_, left, right) =>
      (ctx: Context) => {
        val res = ctx.copyToFreshReg(left(ctx))
        ctx.emit(op, res, right(ctx))
        res
      }
  }

  protected def emitCmpInsn(jmpOp: T86Opcode.CondJmpOp): ((Insn, AsmEmitter[Operand.Reg], AsmEmitter[Operand])) => AsmEmitter[Operand.Reg] = {
    case (_, left, right) =>
      (ctx: Context) => {
        val lab = ctx.freshLabel()
        val res = ctx.freshReg()
        ctx.emit(XOR, res, res)
        ctx.emit(CMP, left(ctx), right(ctx))
        ctx.emit(jmpOp.neg, lab.toOperand)
        ctx.emit(MOV, res, Operand.Imm(1))
        ctx.emit(lab)
        res
      }
  }

  val rules: Seq[GenRule[_]] = Seq(

    GenRule(RegVar, Pat(IAdd, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.ADD)),
    GenRule(RegVar, Pat(ISub, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.SUB)),
    GenRule(RegVar, Pat(IAnd, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.AND)),
    GenRule(RegVar, Pat(IOr, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.OR)),
    GenRule(RegVar, Pat(IXor, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.XOR)),
    GenRule(RegVar, Pat(IShl, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.LSH)),
    GenRule(RegVar, Pat(IShr, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.RSH)),
    GenRule(RegVar, Pat(UMul, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.MUL)),
    GenRule(RegVar, Pat(UDiv, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.DIV)),
    GenRule(RegVar, Pat(SMul, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.IMUL)),
    GenRule(RegVar, Pat(SDiv, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.IDIV)),

    GenRule(RegVar, Pat(CmpIEq, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JZ)),
    GenRule(RegVar, Pat(CmpINe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JNE)),
    GenRule(RegVar, Pat(CmpULt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JB)),
    GenRule(RegVar, Pat(CmpULe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JBE)),
    GenRule(RegVar, Pat(CmpUGt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JA)),
    GenRule(RegVar, Pat(CmpUGe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JAE)),
    GenRule(RegVar, Pat(CmpSLt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JL)),
    GenRule(RegVar, Pat(CmpSLe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JLE)),
    GenRule(RegVar, Pat(CmpSGt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JG)),
    GenRule(RegVar, Pat(CmpSGe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JGE)),

    GenRule(RegVar, Pat[AllocLInsn](AllocL) ^^ { insn =>
      (ctx: Context) =>
        val res = ctx.freshReg()
        ctx.emit(LEA, res, ctx.resolveAllocL(insn))
        res
    }),
    GenRule(RegVar, Pat[AllocGInsn](AllocG) ^^ { insn =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolveAllocG(insn))
    }),

    GenRule(RegVar, Pat[LoadInsn, RegVar.ValueTy](Load, RegVar) ^^ { case (_, addr) =>
      (ctx: Context) => ctx.copyToFreshReg(addr(ctx).mem)
    }),
    GenRule(RegVar, Pat[LoadInsn, AllocLInsn](Load, Pat[AllocLInsn](AllocL)) ^^ { case (_, allocl) =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolveAllocL(allocl))
    }),
    GenRule(RegVar, Pat[LoadInsn, AllocGInsn](Load, Pat[AllocGInsn](AllocG)) ^^ { case (_, allocg) =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolveAllocG(allocg))
    }),

    GenRule(RegVar, Pat[LoadArgInsn](LoadArg) ^^ { insn =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolveLoadArg(insn))
    }),
    GenRule(RegVar, Pat[StoreInsn, RegVar.ValueTy, RegVar.ValueTy](Store, RegVar, RegVar) ^^ { case (_, addr, value) =>
      (ctx: Context) =>
        ctx.emit(MOV, addr(ctx).mem, value(ctx))
        ctx.freshReg()
    }),
    GenRule(RegVar, Pat[IImmInsn](IImm) ^^ { insn =>
      (ctx: Context) => ctx.copyToFreshReg(Operand.Imm(insn.value))
    }),
    GenRule(RegVar, Pat[GetElementPtrInsn, RegVar.ValueTy, RegVar.ValueTy](GetElementPtr, RegVar, RegVar) ^^ { case (insn, base, index) =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        if (insn.fieldIndex == 0)
          ctx.emit(LEA, res, Operand.MemRegRegScaled(base(ctx), index(ctx), (insn.elemTy.sizeBytes / 8).ceil.toLong))
        else
          ???
        res
      }
    }),
    GenRule(RegVar, CallInsnPat[RegVar.ValueTy](RegVar) ^^ { case (insn, args) =>
      (ctx: Context) => {
        args.foreach(arg => {
          ctx.emit(PUSH, arg(ctx))
        })
        ctx.emit(CALL, ctx.getFunLabel(insn.targetFun.get).toOperand)
        ctx.emit(ADD, Operand.SP, Operand.Imm(args.size)) // pop arguments
        val res = ctx.freshReg()
        ctx.emit(MOV, res, ctx.resolveRetValueCaller())
        res
      }
    }),
    GenRule(RegVar, CallPtrInsnPat[RegVar.ValueTy, RegVar.ValueTy](RegVar, RegVar) ^^ { case (insn, ptr, args) =>
      (ctx: Context) => {
        args.foreach(arg => {
          ctx.emit(PUSH, arg(ctx))
        })
        ctx.emit(CALL, ptr(ctx))
        ctx.emit(ADD, Operand.SP, Operand.Imm(args.size)) // pop arguments
        val res = ctx.freshReg()
        ctx.emit(MOV, res, ctx.resolveRetValueCaller())
        res
      }
    }),
    GenRule(RegVar, Pat[PutCharInsn, RegVar.ValueTy](PutChar, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTCHAR, reg(ctx))
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat[PutNumInsn, RegVar.ValueTy](PutNum, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTNUM, reg(ctx))
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat[GetCharInsn](GetChar) ^^ { insn =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(GETCHAR, res)
        res
      }
    }),
    // Phi
    GenRule(RegVar, Pat[RetInsn, RegVar.ValueTy](Ret, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(MOV, ctx.resolveRetValueCallee(), reg(ctx))
        ctx.emitFunEpilogue()
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat[RetVoidInsn](RetVoid) ^^ { _ =>
      (ctx: Context) =>
        ctx.emitFunEpilogue()
        ctx.freshReg()
    }),
    GenRule(RegVar, Pat[HaltInsn](Halt) ^^ { _ =>
      (ctx: Context) => {
        ctx.emit(HALT)
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat[BrInsn](Br) ^^ { insn =>
      (ctx: Context) => {
        ctx.emit(JMP, ctx.getBasicBlockLabel(insn.succBlock.get).toOperand)
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat[CondBrInsn, RegVar.ValueTy](CondBr, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(CMP, reg(ctx), Operand.Imm(0))
        ctx.emit(JZ, ctx.getBasicBlockLabel(insn.falseBlock.get).toOperand)
        ctx.emit(JZ, ctx.getBasicBlockLabel(insn.falseBlock.get).toOperand)
        ctx.freshReg()
      }
    }),

  )

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
    val code = mutable.Buffer.empty[T86ProgramElement]

    var localsSize: Long = 0
    val localsMap = mutable.Map.empty[AllocLInsn, Long]

    var argsSize: Long = 0
    val argsMap = fun.argTys.map(ty => {
      val oldSize = argsSize
      argsSize += getSizeWords(ty)
      oldSize
    })

    val epilogue = mutable.Buffer.empty[T86ProgramElement]
    epilogue += T86Insn(MOV, Operand.SP, Operand.BP)
    epilogue += T86Insn(POP, Operand.BP)

    val ctx = new Context {
      private var nextReg: Long = 1 // 0 is reserved for return value
      private var nextFreg: Long = 0
      private var nextLabel = 0

      override def resolveVar[T](v: Var[AsmEmitter[T]], insn: Insn): T = tileResults((v, insn)).asInstanceOf[T]

      override def emit(el: T86ProgramElement): Unit =
        code += el

      override def freshReg(): Operand.Reg = {
        nextReg += 1
        Operand.BasicReg(nextReg - 1)
      }

      override def freshFReg(): Operand.FReg = {
        nextFreg += 1
        Operand.BasicFReg(nextFreg - 1)
      }

      override def resolveAllocL(insn: AllocLInsn): Operand.MemRegImm = {
        val offset = localsMap.getOrElseUpdate(insn, {
          localsSize += getSizeWords(insn.varTy)
          -localsSize
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
        Operand.MemRegImm(Operand.BP, offset + 1) // 1 for return address
      }

      override def emitFunEpilogue(): Unit =
        code ++= epilogue

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

    val prologue = mutable.Buffer.empty[T86ProgramElement]
    prologue += T86Label(fun.name)
    prologue += T86Insn(PUSH, Operand.BP)
    prologue += T86Insn(MOV, Operand.BP, Operand.SP)
    prologue += T86Insn(SUB, Operand.SP, Operand.Imm(localsSize))
    // TODO: if arguments are passed via registers, copy them to fresh registers

    code.prependAll(prologue)

    code.toSeq
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