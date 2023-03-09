package tinycc.backend.t86.regalloc

import tinycc.backend.RegisterAllocator
import tinycc.backend.t86.T86Opcode.{FPOP, FPUSH, POP, PUSH}
import tinycc.backend.t86.{Operand, T86Fun, T86Insn, T86Program, T86SpecialLabel, T86Utils}

import scala.collection.mutable

class NaiveRegisterAllocator(program: T86Program) extends T86RegisterAllocator(program: T86Program) {
  import T86RegisterAllocator._

  override def result(): T86Program = {
    program.funs.foreach(processFun)
    program
  }

  def processFun(fun: T86Fun): Unit = {
    // remap all non-machine registers into machine regs
    val regMap = mutable.Map.empty[Temp, Temp]

    val availableRegs = mutable.Queue.from(T86Utils.machineRegs)
    val availableFRegs = mutable.Queue.from(T86Utils.machineFRegs)

    fun.flatten.foreach({
      case insn: T86Insn => {
        val DefUse(defines, uses) = getInsnDefUse(insn)
        (defines ++ uses).foreach({
          case temp@Temp(reg: Operand.Reg) if !T86Utils.machineRegs.contains(reg) && !T86Utils.specialRegs.contains(reg) =>
            regMap.getOrElseUpdate(temp, availableRegs.dequeue())

          case temp@Temp(freg: Operand.FReg) if !T86Utils.machineFRegs.contains(freg) =>
            regMap.getOrElseUpdate(temp, availableFRegs.dequeue())

          case _ =>
        })
      }

      case _ =>
    })

    remapRegistersInFun(fun, regMap.toMap.withDefault(reg => reg)) // default mapping for machine and special registers

    // insert PUSH and POP insns to backup and restore them in the fun prologue and epilogue
    val usedRegs = regMap.values.toSeq // convert to seq, so order is deterministic
    val backupCode = usedRegs.collect({
      case Temp(reg: Operand.Reg) => T86Insn(PUSH, reg)
      case Temp(freg: Operand.FReg) => T86Insn(FPUSH, freg)
    })
    val restoreCode = usedRegs.reverse.collect({
      case Temp(reg: Operand.Reg) => T86Insn(POP, reg)
      case Temp(freg: Operand.FReg) => T86Insn(FPOP, freg)
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