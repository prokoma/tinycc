package tinycc.backend.t86.insel

import tinycc.backend.t86.T86Opcode._
import tinycc.backend.t86.T86SpecialLabel.{CallEpilogueMarker, CallPrologueMarker}
import tinycc.backend.t86._
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir._

trait GenRules extends T86TilingInstructionSelection {
  case object RegVar extends T86Var[Operand.Reg] {
    // penalize if we want to assign a double value into RegVar
    override def getMatchCost(insn: Insn): Int = if(insn.resultTy == IrTy.DoubleTy) 1 else 0
  }

  case object FRegVar extends T86Var[Operand.FReg] {
    // penalize if we want to assign an integer value into FRegVar
    override def getMatchCost(insn: Insn): Int = if(insn.resultTy == IrTy.Int64Ty) 1 else 0
  }

  override val variables: Seq[Var[AsmEmitter[_]]] = Seq(RegVar, FRegVar)

  override def canCastFromTo(from: AsmVar[_], to: AsmVar[_]): Boolean = (from, to) match {
    case _ if from == to => true
    case (RegVar, FRegVar) => true
    case (FRegVar, RegVar) => true
    case _ => false
  }

  override def emitCastFromTo[F, T](value: F, from: AsmVar[F], to: AsmVar[T]): AsmEmitter[T] = (from, to) match {
    case _ if from == to => pure(value.asInstanceOf[T])
    case (RegVar, FRegVar) => (ctx: Context) => ctx.copyToFreshFReg(value.asInstanceOf[Operand.Reg]).asInstanceOf[T]
    case (FRegVar, RegVar) => (ctx: Context) => ctx.copyToFreshReg(value.asInstanceOf[Operand.FReg]).asInstanceOf[T]
    case _ => throw new IllegalArgumentException(s"cannot cast $value from $from to $to")
  }

  def pure[T](value: T): AsmEmitter[T] = (ctx: Context) => value

  def constImm(v: Long): AsmPat[Operand.Imm] = Pat(IImm)
    .filter({ case insn: IImmInsn => insn.value == v }) ^^ { case insn: IImmInsn => pure(Operand.Imm(insn.value)) }

  def constFImm(v: Long): AsmPat[Operand.FImm] = Pat(FImm)
    .filter({ case insn: FImmInsn => insn.value == v }) ^^ { case insn: FImmInsn => pure(Operand.FImm(insn.value)) }

  lazy val imm: AsmPat[Operand.Imm] = (
    Pat(IImm) ^^ { case insn: IImmInsn => pure(Operand.Imm(insn.value)) }
      | Pat(SizeOf) ^^ { case insn: SizeOfInsn => pure(Operand.Imm(insn.varTy.sizeWords)) })

  lazy val regOrImm: AsmPat[Operand] = imm | RegVar

  lazy val fimm: AsmPat[Operand.FImm] = Pat(FImm) ^^ { case insn: FImmInsn => pure(Operand.FImm(insn.value)) }
  lazy val fregOrFimm: AsmPat[Operand] = FRegVar | fimm
  lazy val regOrFregOrFimm: AsmPat[Operand] = RegVar | fregOrFimm

  lazy val memReg: AsmPat[Operand.MemReg] = RegVar ^^ { reg => (ctx: Context) => reg(ctx).mem }
  lazy val memAllocL: AsmPat[Operand.MemRegImm] = Pat(AllocL) ^^ { case insn: AllocLInsn => (ctx: Context) => ctx.resolveAllocL(insn) }
  lazy val memAllocG: AsmPat[Operand.MemImm] = Pat(AllocG) ^^ { case insn: AllocGInsn => (ctx: Context) => ctx.resolveAllocG(insn) }

  /** Memory operand, which can be used as source or desetination of MOV to regular register. */
  lazy val memAddr: AsmPat[Operand] = memReg | memAllocL | memAllocG | getElementPtr
  lazy val memRegOrMemImm: AsmPat[Operand] = memReg | memAllocG

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

  lazy val fcmpInsn: AsmPat[CondJmpOp] = (
    Pat(CmpFEq, FRegVar, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JZ) |
      Pat(CmpFNe, FRegVar, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JNE) |
      Pat(CmpFLt, FRegVar, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JL) |
      Pat(CmpFLe, FRegVar, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JLE) |
      Pat(CmpFGt, FRegVar, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JG) |
      Pat(CmpFGe, FRegVar, fregOrFimm) ^^ emitFCmpAndReturnJmpOp(T86Opcode.JGE))

  lazy val cmpInsn: AsmPat[CondJmpOp] = icmpInsn | fcmpInsn

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

  lazy val call: AsmPat[Operand] = CallInsnPat(RegVar | FRegVar) ^^ { case (insn, args) =>
    (ctx: Context) => {
      args.reverse.foreach(arg => arg(ctx) match {
        case reg: Operand.Reg => ctx.emit(PUSH, reg)
        case freg: Operand.FReg => ctx.emit(FPUSH, freg)
        case _ => throw new UnsupportedOperationException // unreachable
      })
      ctx.emit(CallPrologueMarker)
      ctx.emit(CALL, ctx.getFunLabel(insn.targetFun).toOperand)
      if (args.nonEmpty)
        ctx.emit(ADD, Operand.SP, Operand.Imm(args.size)) // pop arguments
      ctx.emit(CallEpilogueMarker)
      T86Utils.returnValueReg
    }
  }

  lazy val callPtr = CallPtrInsnPat(RegVar, RegVar | FRegVar) ^^ { case (insn, ptr, args) =>
    (ctx: Context) => {
      args.reverse.foreach(arg => arg(ctx) match {
        case reg: Operand.Reg => ctx.emit(PUSH, reg)
        case freg: Operand.FReg => ctx.emit(FPUSH, freg)
        case _ => throw new UnsupportedOperationException // unreachable
      })
      ctx.emit(CallPrologueMarker)
      ctx.emit(CALL, ptr(ctx))
      if (args.nonEmpty)
        ctx.emit(ADD, Operand.SP, Operand.Imm(args.size)) // pop arguments
      ctx.emit(CallEpilogueMarker)
      T86Utils.returnValueReg
    }
  }

  lazy val getElementPtr = (
    Pat(GetElementPtr, RegVar, RegVar) ^^ { case (insn: GetElementPtrInsn, base, index) =>
      (ctx: Context) => {
        if (insn.fieldIndex == 0)
          Operand.MemRegRegScaled(base(ctx), index(ctx), insn.elemTy.sizeWords)
        else {
          val fieldOffset = insn.elemTy.asInstanceOf[IrTy.StructTy].getFieldOffsetWords(insn.fieldIndex)
          Operand.MemRegImmRegScaled(base(ctx), fieldOffset, index(ctx), insn.elemTy.sizeWords)
        }
      }
    } | Pat(GetElementPtr, RegVar, imm) ^^ { case (insn: GetElementPtrInsn, base, index) =>
      (ctx: Context) => {
        val fieldOffset = {
          if (insn.fieldIndex == 0) 0
          else
            insn.elemTy.asInstanceOf[IrTy.StructTy].getFieldOffsetWords(insn.fieldIndex)
        }
        Operand.MemRegImm(base(ctx), index(ctx).value * insn.elemTy.sizeWords + fieldOffset)
      }
    })

  def commutativeBinaryInsn[T](op: IrOpcode, leftPat: Pat[T], rightPat: Pat[T]): Pat[(Insn, T, T)] =
    Pat(op, leftPat, rightPat) | Pat(op, rightPat, leftPat) ^^ { case (insn, left, right) => (insn, right, left) }

  lazy val inc = (
    commutativeBinaryInsn(IAdd, regOrImm, constImm(1)) ^^ { case (_, regOrImm, _) => regOrImm }
      | Pat(ISub, regOrImm, constImm(-1)) ^^ { case (_, regOrImm, _) => regOrImm }
    )

  lazy val dec = (
    commutativeBinaryInsn(IAdd, regOrImm, constImm(-1)) ^^ { case (_, regOrImm, _) => regOrImm }
      | Pat(ISub, regOrImm, constImm(1)) ^^ { case (_, regOrImm, _) => regOrImm }
    )

  lazy val sgnNeg = (
    Pat(ISub, constImm(0), regOrImm) ^^ { case (_, _, regOrImm) => regOrImm }
      | commutativeBinaryInsn(SMul, constImm(-1), regOrImm) ^^ { case (_, _, regOrImm) => regOrImm }
  )

  override lazy val rules: Seq[GenRule[_]] = Seq(

    GenRule(RegVar, Pat(IAdd, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.ADD) cost 1), // prefer INC and DEC over ADD and SUB
    GenRule(RegVar, Pat(ISub, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.SUB) cost 1),
    GenRule(RegVar, Pat(IAnd, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.AND)),
    GenRule(RegVar, Pat(IOr, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.OR)),
    GenRule(RegVar, Pat(IXor, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.XOR)),
    GenRule(RegVar, Pat(IShl, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.LSH)),
    GenRule(RegVar, Pat(IShr, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.RSH)),
    GenRule(RegVar, Pat(UMul, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.MUL)),
    GenRule(RegVar, Pat(UDiv, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.DIV)),
    GenRule(RegVar, Pat(SMul, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.IMUL) cost 1),
    GenRule(RegVar, Pat(SDiv, regOrImm, regOrImm) ^^ emitBinArithInsn(T86Opcode.IDIV)),

    GenRule(RegVar, inc ^^ emitUnary(INC)),
    GenRule(RegVar, dec ^^ emitUnary(DEC)),
    GenRule(RegVar, sgnNeg ^^ emitUnary(NEG)),

    GenRule(FRegVar, Pat(FAdd, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FADD)),
    GenRule(FRegVar, Pat(FSub, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FSUB)),
    GenRule(FRegVar, Pat(FMul, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FMUL)),
    GenRule(FRegVar, Pat(FDiv, regOrFregOrFimm, fregOrFimm) ^^ emitFloatBinArithInsn(T86Opcode.FDIV)),

    // %2 = cmpieq %0, %1
    GenRule(RegVar, cmpInsn ^^ { cmpInsn =>
      (ctx: Context) => {
        val lab = ctx.freshLabel()
        val res = ctx.copyToFreshReg(Operand.Imm(0))
        val jmpOp = cmpInsn(ctx)
        ctx.emit(jmpOp.neg, lab.toOperand)
        // we can't use MOV here, because for simplicity this is in a single basic block and we need res to be live for the entire duration
        // if we used MOV, res would be dead after setting it to 0
        ctx.emit(ADD, res, Operand.Imm(1))
        ctx.emit(lab)
        res
      }
    }),

    // %2 = condbr (cmpieq %0, %1)
    GenRule(RegVar, Pat(CondBr, cmpInsn) ^^ { case (condBrInsn: CondBrInsn, cmpInsn) =>
      (ctx: Context) => {
        emitCondJmp(cmpInsn(ctx), condBrInsn, ctx)
        ctx.freshReg()
      }
    }),

    GenRule(RegVar, Pat(AllocL) ^^ { case insn: AllocLInsn =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(LEA, res, ctx.resolveAllocL(insn))
        res
      }
    }),
    GenRule(RegVar, Pat(AllocG) ^^ { case insn: AllocGInsn =>
      (ctx: Context) => ctx.copyToFreshReg(Operand.Imm(ctx.resolveAllocG(insn).addr))
    }),

    GenRule(RegVar, Pat(Load, memAddr) ^^ { case (_, memAddr) =>
      (ctx: Context) => ctx.copyToFreshReg(memAddr(ctx))
    }),
    GenRule(FRegVar, Pat(Load, memRegOrMemImm) ^^ { case (_, memRegOrMemImm) =>
      (ctx: Context) => ctx.copyToFreshFReg(memRegOrMemImm(ctx))
    }),

    GenRule(RegVar, Pat(LoadArg) ^^ { case insn: LoadArgInsn =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolveLoadArg(insn))
    }),
    GenRule(RegVar, Pat(Store, memAddr, regOrImm | FRegVar) ^^ { case (_, memAddr, value) =>
      (ctx: Context) =>
        ctx.emit(MOV, memAddr(ctx), value(ctx))
        ctx.freshReg()
    }),

    GenRule(RegVar, getElementPtr ^^ (memAddr => (ctx: Context) => {
      val res = ctx.freshReg()
      ctx.emit(LEA, res, memAddr(ctx))
      res
    })),

    GenRule(RegVar, imm ^^ (imm => (ctx: Context) => ctx.copyToFreshReg(imm(ctx)))),
    GenRule(FRegVar, fimm ^^ (fimm => (ctx: Context) => ctx.copyToFreshFReg(fimm(ctx)))),

    GenRule(RegVar, Pat(GetFunPtr) ^^ { case insn: GetFunPtrInsn =>
      (ctx: Context) => ctx.copyToFreshReg(ctx.getFunLabel(insn.targetFun).toOperand)
    }),

    GenRule(RegVar, call ^^ { retValue => (ctx: Context) => ctx.copyToFreshReg(retValue(ctx)) }),
    GenRule(FRegVar, call ^^ { retValue => (ctx: Context) => ctx.copyToFreshFReg(retValue(ctx)) }),

    GenRule(RegVar, callPtr ^^ { retValue => (ctx: Context) => ctx.copyToFreshReg(retValue(ctx)) }),
    GenRule(FRegVar, callPtr ^^ { retValue => (ctx: Context) => ctx.copyToFreshFReg(retValue(ctx)) }),

    GenRule(RegVar, Pat(PutChar, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTCHAR, reg(ctx))
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat(PutNum, RegVar) ^^ { case (insn, reg) =>
      (ctx: Context) => {
        ctx.emit(PUTNUM, reg(ctx))
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat(GetChar) ^^ { insn =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(GETCHAR, res)
        res
      }
    }),

    GenRule(RegVar, Pat(DoubleToSInt64, FRegVar) ^^ { case (_, freg) =>
      (ctx: Context) => {
        val res = ctx.freshReg()
        ctx.emit(NRW, res, freg(ctx))
        res
      }
    }),
    GenRule(RegVar, Pat(BitcastDoubleToInt64, regOrFregOrFimm) ^^ { case (_, value) =>
      (ctx: Context) => ctx.copyToFreshReg(value(ctx))
    }),

    GenRule(FRegVar, castSInt64ToDouble),
    GenRule(FRegVar, Pat(BitcastInt64ToDouble, RegVar) ^^ { case (_, reg) =>
      (ctx: Context) => ctx.copyToFreshFReg(reg(ctx))
    }),
    GenRule(RegVar, Pat(BitcastInt64ToDouble, RegVar) ^^ { case (_, reg) => reg }),

    GenRule(RegVar, PhiInsnPat(RegVar) ^^ { case (insn, _) =>
      // copy on each use, because the phi could reference instruction in the current basic block
      (ctx: Context) => ctx.copyToFreshReg(ctx.resolvePhiReg(RegVar, insn))
    }),
    GenRule(FRegVar, PhiInsnPat(FRegVar) ^^ { case (insn, _) =>
      (ctx: Context) => ctx.copyToFreshFReg(ctx.resolvePhiFReg(FRegVar, insn))
    }),

    GenRule(RegVar, Pat(Ret, regOrImm | FRegVar) ^^ { case (insn, retVal) =>
      (ctx: Context) => {
        ctx.emit(MOV, T86Utils.returnValueReg, retVal(ctx))
        ctx.emit(T86SpecialLabel.FunEpilogueMarker)
        ctx.emit(RET)
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat(RetVoid) ^^ { insn =>
      (ctx: Context) =>
        ctx.emit(T86SpecialLabel.FunEpilogueMarker)
        ctx.emit(RET)
        ctx.freshReg()
    }),
    GenRule(RegVar, Pat(Halt) ^^ { _ =>
      (ctx: Context) => {
        ctx.emit(HALT)
        ctx.freshReg()
      }
    }),
    GenRule(RegVar, Pat(Br) ^^ { case insn: BrInsn =>
      (ctx: Context) => {
        if (!insn.basicBlock.linSucc.contains(insn.succBlock))
          ctx.emit(JMP, ctx.getBasicBlockLabel(insn.succBlock).toOperand)
        ctx.freshReg()
      }
    }),

    GenRule(RegVar, Pat(CondBr, RegVar) ^^ { case (insn: CondBrInsn, reg) =>
      (ctx: Context) => {
        ctx.emit(CMP, reg(ctx), Operand.Imm(0))
        emitCondJmp(JNZ, insn, ctx)
        ctx.freshReg()
      }
    }),

  ).flatMap(expandRule)

  protected def expandRule(rule: GenRule[_]): Iterable[GenRule[_]] = {
    def fregToReg(freg: AsmPat[Operand.FReg]): AsmPat[Operand.Reg] =
      freg ^^ { freg => (ctx: Context) => ctx.copyToFreshReg(freg(ctx)) } cost 1

    def regToFreg(reg: AsmPat[Operand.Reg]): AsmPat[Operand.FReg] =
      reg ^^ { reg => (ctx: Context) => ctx.copyToFreshFReg(reg(ctx)) } cost 1

    val expandedRule: Iterable[GenRule[_]] = rule match {
      case GenRule(FRegVar, rhs: AsmPat[Operand.FReg]@unchecked) =>
        Iterable(rule, GenRule(RegVar, fregToReg(rhs)))

      case GenRule(RegVar, rhs: AsmPat[Operand.Reg]@unchecked) =>
        Iterable(rule, GenRule(FRegVar, regToFreg(rhs)))

      case rule => Iterable.single(rule)
    }

    expandedRule.flatMap(_.flatten)
  }

  /** Copies destination operand into fresh register and emits BinaryOp instruction. */
  protected def emitBinArithInsn(op: T86Opcode.BinaryOp): ((Insn, AsmEmitter[Operand], AsmEmitter[Operand])) => AsmEmitter[Operand.Reg] = {
    case (_, left, right) =>
      (ctx: Context) => {
        val res = ctx.copyToFreshReg(left(ctx))
        ctx.emit(op, res, right(ctx))
        res
      }
  }

  /** Copies destination operand into fresh float register and emits BinaryOp instruction. */
  protected def emitFloatBinArithInsn(op: T86Opcode.BinaryOp): ((Insn, AsmEmitter[Operand], AsmEmitter[Operand])) => AsmEmitter[Operand.FReg] = {
    case (_, left, right) =>
      (ctx: Context) => {
        val res = ctx.copyToFreshFReg(left(ctx))
        ctx.emit(op, res, right(ctx))
        res
      }
  }

  protected def emitCmpAndReturnJmpOp(jmpOp: T86Opcode.CondJmpOp): ((Insn, AsmEmitter[Operand.Reg], AsmEmitter[Operand])) => AsmEmitter[T86Opcode.CondJmpOp] = {
    case (_, left, right) =>
      (ctx: Context) => {
        ctx.emit(CMP, left(ctx), right(ctx))
        jmpOp
      }
  }

  protected def emitFCmpAndReturnJmpOp(jmpOp: T86Opcode.CondJmpOp): ((Insn, AsmEmitter[Operand.FReg], AsmEmitter[Operand])) => AsmEmitter[T86Opcode.CondJmpOp] = {
    case (_, left, right) =>
      (ctx: Context) => {
        ctx.emit(FCMP, left(ctx), right(ctx))
        jmpOp
      }
  }

  protected def emitCondJmp(jmpOp: T86Opcode.CondJmpOp, insn: CondBrInsn, ctx: Context): Unit = {
    if (insn.basicBlock.linSucc.contains(insn.trueBlock)) { // trueBlock is next in order
      ctx.emit(jmpOp.neg, ctx.getBasicBlockLabel(insn.falseBlock).toOperand) // if false, jump, otherwise fall through to trueBlock
    } else if (insn.basicBlock.linSucc.contains(insn.falseBlock)) { // falseBlock is next in order
      ctx.emit(jmpOp, ctx.getBasicBlockLabel(insn.trueBlock).toOperand)
    } else {
      ctx.emit(jmpOp, ctx.getBasicBlockLabel(insn.trueBlock).toOperand)
      ctx.emit(JMP, ctx.getBasicBlockLabel(insn.falseBlock).toOperand)
    }
  }

  protected def emitUnary(op: T86Opcode.UnaryOp)(operand: AsmEmitter[Operand]): AsmEmitter[Operand.Reg] =
    (ctx: Context) => {
      val res = ctx.copyToFreshReg(operand(ctx))
      ctx.emit(op, res)
      res
    }
}
