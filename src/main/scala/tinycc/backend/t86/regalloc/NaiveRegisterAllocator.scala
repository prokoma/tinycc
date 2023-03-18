package tinycc.backend.t86.regalloc

import tinycc.backend.t86.T86Opcode.{FPOP, FPUSH, POP, PUSH}
import tinycc.backend.t86._

import scala.collection.mutable

trait GenericNaiveRegisterAllocator[T <: Operand] extends T86GenericRegisterAllocator[T] {
  def transformFun(fun: T86Fun): Unit = {
    // remap all non-machine registers into machine regs
    val regMap = mutable.Map.empty[T, T]
    val availableRegs = mutable.Queue.from(machineRegs)

    fun.flatten.foreach({
      case insn: T86Insn =>
        getInsnDefUse(insn).regs.foreach({ case reg => regMap.getOrElseUpdate(reg, availableRegs.dequeue()) })

      case _ =>
    })

    remapRegistersInFun(fun, regMap.toMap.withDefault(reg => reg)) // default mapping for machine registers

    // insert PUSH and POP insns to backup and restore them in the fun prologue and epilogue
    val usedRegs = regMap.values.toSeq // convert to seq, so order is deterministic
    val backupCode = usedRegs.map({
      case reg: Operand.Reg => T86Insn(PUSH, reg)
      case freg: Operand.FReg => T86Insn(FPUSH, freg)
    })
    val restoreCode = usedRegs.reverse.map({
      case reg: Operand.Reg => T86Insn(POP, reg)
      case freg: Operand.FReg => T86Insn(FPOP, freg)
    })

    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.flatMap({
        case T86SpecialLabel.FunPrologueMarker => T86SpecialLabel.FunPrologueMarker +: backupCode
        case T86SpecialLabel.FunEpilogueMarker => restoreCode :+ T86SpecialLabel.FunEpilogueMarker

        case elem => Seq(elem)
      })
    })
  }
}

class NaiveRegisterAllocator(_machineRegCount: Int, _machineFRegCount: Int) extends T86RegisterAllocator {

  val regRegisterAllocator = new GenericNaiveRegisterAllocator[Operand.Reg] with T86RegRegisterAllocator {
    /** Machine registers are in the range (0, machineRegCount)- */
    override def machineRegCount: Int = _machineRegCount
  }

  val fregRegisterAllocator = new GenericNaiveRegisterAllocator[Operand.FReg] with T86FRegRegisterAllocator {
    /** Machine registers are in the range (0, machineRegCount)- */
    override def machineRegCount: Int = _machineFRegCount
  }

  override def transformProgram(program: T86Program): Unit = {
    program.funs.foreach(transformFun)
  }

  def transformFun(fun: T86Fun): Unit = {
    regRegisterAllocator.transformFun(fun)
    fregRegisterAllocator.transformFun(fun)
  }
}