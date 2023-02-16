package tinycc.backend

import tinycc.backend.t86.Operand.{FReg, Reg}
import tinycc.backend.t86._
import tinycc.common.ir.IrOpcode._
import tinycc.common.ir.{Insn, IrOpcode}

////trait AsmEmitter
////
////trait RewriteRule[R] extends (Insn => (T86Context => R)) {
////}
////
////trait T86Context {
////
////}
////
////object IAddRule extends RewriteRule {
////  override def matches(insn: Insn): Boolean =
////    insn.isInstanceOf[BinaryArithInsn]
////
////  def emit
////}
//
//// rewriterule
//// matches
//
//// MemoryImm ->  ^^ { case ()
//// Imm ->
//// Operand -> MemoryImm | Imm
//// Reg -> IAdd(Operand, Operand) ^^ { case (left, right) => new ADD(left, right) }
//
//object Test {
//
//  trait Context {
//    def emit(insn: T86Insn): Unit
//
//    def freshReg(): Reg
//
//    def copyReg(reg: Reg): Reg
//
//    def freshFReg(): FReg
//
//    def copyFReg(freg: FReg): FReg
//  }
//
//  trait AsmEmitter[+A] extends (Context => A)
//
//  def AsmEmitter[A](f: (Context => A)): AsmEmitter[A] = new AsmEmitter[A] {
//    override def apply(ctx: Context): A = f(ctx)
//  }
//
//  trait Var[T]
//
//  trait RuleRHS
//
//  case class Rule[T](v: Var[T], rhs: Node[T])
//
//  //  lazy val binArithRightOp: RuleRHS =
//  //    Reg |
//
//
//  trait Node[A] {
//    def ^^[B](f: A => B): Node[B] = ???
//  }
//
//  case class BinaryInsnNode[T, U](op: IrOpcode, left: Node[T], right: Node[U]) extends Node[(Insn, T, U)] {
//
//  }
//
//  implicit case class VarNode[T](v: Var[T]) extends Node[T]
//
//  case object RegVar extends Var[AsmEmitter[Operand.Reg]]
//
//  case object FRegVar extends Var[AsmEmitter[Operand.FReg]]
//
//  def binArithHelper(reg1: AsmEmitter[Operand.Reg], reg2: AsmEmitter[Reg], f: (Context, Reg, Reg) => Any): AsmEmitter[Reg] =
//    ctx => {
//      val reg1Copy = ctx.copyReg(reg1(ctx))
//      f(reg1Copy, reg2(ctx))
//      reg1Copy
//    }
//
//  def binArithInsnEmitter(f: (Reg, Operand) => T86Insn): (Insn, AsmEmitter[Reg], AsmEmitter[Operand]) => AsmEmitter[Reg]
//
//
//  (a: (Insn, AsmEmitter[Reg], AsmEmitter[Operand])): AsmEmitter[Reg] = a match {
//    case (_, reg, value) =>
//      AsmEmitter(ctx => {
//        val dest = ctx.copyReg(reg(ctx))
//        ctx.emit(dest, )
//        dest
//      })
//  }
//
//  lazy val rules: Seq[Rule[_]] = Seq(
//    //    Rule(Imm, iimmInsn ^^ { iimmInsn =>
//    //      ctx => Operand.Imm(iimm.value)
//    //    }),
//    //    Rule(Reg, Imm ^^ { imm =>
//    //      ctx => {
//    //        val reg = ctx.fresh()
//    //        ctx.emit(MOV(reg, imm))
//    //        reg
//    //      }
//    //    }),
//    Rule(RegVar, BinaryInsnNode(IAdd, RegVar, RegVar) ^^ binArithInsnEmitter((reg, value) => ADD(reg, value))),
//    Rule(RegVar, BinaryInsnNode(ISub, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(SUB(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(IAnd, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(AND(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(IOr, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(OR(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(IXor, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(XOR(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(IShl, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(LSH(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(IShr, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(RSH(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(UMul, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(MUL(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(UDiv, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(DIV(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(SMul, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(IMUL(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(SDiv, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => ctx.emit(IDIV(ctx.copyReg(reg1(ctx)), reg2(ctx))))
//    }),
//    Rule(RegVar, BinaryInsnNode(FAdd, RegVar, RegVar) ^^ { case (_, reg1, reg2) =>
//      AsmEmitter(ctx => {
//        val freg1 = ctx.freshFReg()
//        val freg2 = ctx.freshFReg()
//        ctx.emit(MOV(freg1, reg1(ctx)))
//        ctx.emit(MOV(freg2, reg2(ctx)))
//        ctx.emit(FADD(freg1, freg2))
//        val res = ctx.freshReg()
//        ctx.emit(MOV(res, freg1))
//        res
//      })
//    }),
//  )
//}