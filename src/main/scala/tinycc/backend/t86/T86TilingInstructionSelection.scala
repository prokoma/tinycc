package tinycc.backend.t86

import tinycc.backend.t86.T86Opcode._
import tinycc.backend.{BackendException, TilingInstructionSelection}
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir._

import scala.collection.mutable

trait T86TilingInstructionSelection extends TilingInstructionSelection {

  trait Context {
    def resolveVar[T](v: Var[AsmEmitter[T]], insn: Insn): T

    def emit(el: T86ProgramElement): Unit

    def emit(op: T86Opcode): Unit = emit(T86Insn(op))

    def emit(op: T86Opcode, operand: Operand): Unit = emit(T86Insn(op, operand))

    def emit(op: T86Opcode, operand0: Operand, operand1: Operand): Unit = emit(T86Insn(op, operand0, operand1))

    def freshLabel(): T86Label

    def getBasicBlockLabel(bb: BasicBlock): T86Label =
      T86Label(bb.fun.name + "$" + bb.name)

    def freshReg(): Operand.Reg

    def copyReg(reg: Operand.Reg): Operand.Reg = {
      val res = freshReg()
      emit(MOV, res, reg)
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
        val res = ctx.copyReg(left(ctx))
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

    GenRule(RegVar, Pat.apply(IAdd, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.ADD)),
    GenRule(RegVar, Pat.apply(ISub, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.SUB)),
    GenRule(RegVar, Pat.apply(IAnd, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.AND)),
    GenRule(RegVar, Pat.apply(IOr, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.OR)),
    GenRule(RegVar, Pat.apply(IXor, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.XOR)),
    GenRule(RegVar, Pat.apply(IShl, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.LSH)),
    GenRule(RegVar, Pat.apply(IShr, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.RSH)),
    GenRule(RegVar, Pat.apply(UMul, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.MUL)),
    GenRule(RegVar, Pat.apply(UDiv, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.DIV)),
    GenRule(RegVar, Pat.apply(SMul, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.IMUL)),
    GenRule(RegVar, Pat.apply(SDiv, RegVar, RegVar) ^^ emitBinArithInsn(T86Opcode.IDIV)),

    GenRule(RegVar, Pat.apply(CmpIEq, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JZ)),
    GenRule(RegVar, Pat.apply(CmpINe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JNE)),
    GenRule(RegVar, Pat.apply(CmpULt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JB)),
    GenRule(RegVar, Pat.apply(CmpULe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JBE)),
    GenRule(RegVar, Pat.apply(CmpUGt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JA)),
    GenRule(RegVar, Pat.apply(CmpUGe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JAE)),
    GenRule(RegVar, Pat.apply(CmpSLt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JL)),
    GenRule(RegVar, Pat.apply(CmpSLe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JLE)),
    GenRule(RegVar, Pat.apply(CmpSGt, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JG)),
    GenRule(RegVar, Pat.apply(CmpSGe, RegVar, RegVar) ^^ emitCmpInsn(T86Opcode.JGE)),

    GenRule(RegVar, Pat.apply[AllocLInsn](AllocL) ^^ { insn =>
      (ctx: Context) =>
        val res = ctx.freshReg()
        ctx.emit(LEA, res, ctx.resolveAllocL(insn))
        res
    }),
    GenRule(RegVar, Pat.apply[AllocGInsn](AllocG) ^^ { insn =>
      (ctx: Context) =>
        val res = ctx.freshReg()
        ctx.emit(MOV, res, ctx.resolveAllocG(insn))
        res
    }),
    GenRule(RegVar, Pat.apply[LoadInsn, RegVar.ValueTy](Load, RegVar) ^^ { case (_, addr) =>
      (ctx: Context) =>
        val res = ctx.freshReg()
        ctx.emit(MOV, res, addr(ctx).mem)
        res
    }),
    GenRule(RegVar, Pat.apply[LoadArgInsn](LoadArg) ^^ { insn =>
      (ctx: Context) =>
        val res = ctx.freshReg()
        ctx.emit(MOV, res, ctx.resolveLoadArg(insn))
        res
    }),
    GenRule(RegVar, Pat.apply[StoreInsn, RegVar.ValueTy, RegVar.ValueTy](Store, RegVar, RegVar) ^^ { case (_, addr, value) =>
      (ctx: Context) =>
        val res = ctx.freshReg()
        ctx.emit(MOV, addr(ctx).mem, value(ctx))
        res
    }),
    GenRule(RegVar, Pat.apply[IImmInsn](IImm) ^^ { insn =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(MOV, res, Operand.Imm(insn.value))
        res
      }
    }),
    GenRule(RegVar, Pat.apply[GetElementPtrInsn, RegVar.ValueTy, RegVar.ValueTy](GetElementPtr, RegVar, RegVar) ^^ { case (insn, base, index) =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        if (insn.fieldIndex == 0)
          ctx.emit(LEA, res, Operand.MemRegRegScaled(base(ctx), index(ctx), (insn.elemTy.sizeBytes / 8).ceil.toLong))
        else
          ???
        res
      }
    }),
    // Call
    // CallPtr
    GenRule(RegVar, Pat.apply[PutCharInsn, RegVar.ValueTy](PutChar, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTCHAR, reg(ctx))
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat.apply[PutNumInsn, RegVar.ValueTy](PutNum, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTNUM, reg(ctx))
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat.apply[GetCharInsn](GetChar) ^^ { insn =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(GETCHAR, res)
        res
      }
    }),
    // Phi
    GenRule(RegVar, Pat.apply[RetInsn, RegVar.ValueTy](Ret, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(MOV, ctx.resolveRetValueCallee(), reg(ctx))
        ctx.emitFunEpilogue()
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat.apply[RetVoidInsn](RetVoid) ^^ { _ =>
      (ctx: Context) =>
        ctx.emitFunEpilogue()
        ctx.freshReg()
    }),
    GenRule(RegVar, Pat.apply[HaltInsn](Halt) ^^ { _ =>
      (ctx: Context) => {
        ctx.emit(HALT)
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat.apply[BrInsn](Br) ^^ { insn =>
      (ctx: Context) => {
        ctx.emit(JMP, ctx.getBasicBlockLabel(insn.succBlock.get).toOperand)
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat.apply[CondBrInsn, RegVar.ValueTy](CondBr, RegVar) ^^ { case (insn, reg) =>
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
    val code = program.funs.map(tileIrFun).reduce(_ ++ _)
    Right(code)
  }

  def getSizeWords(ty: IrTy): Long =
    (ty.sizeBytes / 8).ceil.toLong

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
    val tileMap = mutable.Map.empty[Insn, GenRule.Match[_]]

    // TODO: work with register constraints (Reg vs FReg)

    // instructions in a basic block are topologically sorted
    // greedily cover instructions with tiles
    bb.body.reverse.foreach(insn => {
      if (!allCoveredInsns.contains(insn)) {
        rules.view.flatMap(_(insn)).headOption match {
          case Some(m) =>
            tileMap(insn) = m
            allCoveredInsns ++= m.coveredInsns

          case None =>
            throw new BackendException(s"Failed to cover $insn (tried ${rules.size} rules)")
        }
      }
    })

    // now loop in the program order and generate code for the matched tiles
    bb.body.foreach(insn => tileMap.get(insn) match {
      case Some(m) =>
        tileResults((m.rule.v, insn)) = m.value(ctx)

      case None =>
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