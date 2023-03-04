package tinycc.backend.t86.insel

import tinycc.backend.TilingInstructionSelection
import tinycc.backend.t86.T86Opcode._
import tinycc.backend.t86._
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir._

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

//  def constFImm(v: Long): AsmPat[Operand.Imm] = Pat(IImm)
//    .filter({ case insn: IImmInsn => insn.value == v }) ^^ { case insn: IImmInsn => pure(Operand.Imm(insn.value)) }

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

  lazy val icmpInsn: AsmPat[CondJmpOp] = (
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

  protected def emitFCmpAndReturnJmpOp(jmpOp: T86Opcode.CondJmpOp): ((Insn, AsmEmitter[Operand.FReg], AsmEmitter[Operand])) => AsmEmitter[T86Opcode.CondJmpOp] = {
    case (_, left, right) =>
      (ctx: Context) => {
        ctx.emit(FCMP, left(ctx), right(ctx))
        jmpOp
      }
  }

  lazy val fcmpInsn: AsmPat[CondJmpOp] = (
    Pat(CmpFEq, freg, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JZ) |
      Pat(CmpFNe, freg, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JNE) |
      Pat(CmpFLt, freg, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JL) |
      Pat(CmpFLe, freg, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JLE) |
      Pat(CmpFGt, freg, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JG) |
      Pat(CmpFGe, freg, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JGE)

//      // cmpfeq (fsub %1, %2), (fimm 0)
//      Pat(CmpIEq, Pat(FSub, freg, fregOrFimm), constFImm(0)) ^^ { case (_, (_, left, right), _) =>
//        (ctx: Context) => {
//          ctx.emit(CMP, left(ctx), right(ctx))
//          T86Opcode.JZ
//        }
//      }
    )

  lazy val cmpInsn: AsmPat[CondJmpOp] = icmpInsn | fcmpInsn

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

  // TODO: replace with postprocessing of the entire ruleset
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
}