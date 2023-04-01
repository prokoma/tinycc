package tinycc.backend.t86.regalloc

import tinycc.backend.t86._

import scala.collection.mutable

trait GenericNaiveRegisterAllocator[T <: Operand] extends T86GenericRegisterAllocator[T] {
  def pushOpcode: T86Opcode.UnaryOp

  def popOpcode: T86Opcode.UnaryOp

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
    val backupCode = usedRegs.map(reg => T86Insn(pushOpcode, reg))
    val restoreCode = usedRegs.reverse.map(reg => T86Insn(popOpcode, reg))

    fun.basicBlocks.foreach(bb => {
      bb.body = bb.body.flatMap({
        case T86SpecialLabel.FunPrologueMarker => T86SpecialLabel.FunPrologueMarker +: backupCode
        case T86SpecialLabel.FunEpilogueMarker => restoreCode :+ T86SpecialLabel.FunEpilogueMarker

        case elem => Seq(elem)
      })
    })
  }
}

/** A dummy register allocator, which remaps all temporaries into the range <0,machineRegCount).
 * Backups and restores all machine registers in function prologue and epilogue. */
class NaiveRegisterAllocator(_machineRegCount: Int, _machineFRegCount: Int) extends T86RegisterAllocator {

  val regRegisterAllocator = new GenericNaiveRegisterAllocator[Operand.Reg] with T86RegRegisterAllocator {
    /** Machine registers are in the range (0, machineRegCount)- */
    override def machineRegCount: Int = _machineRegCount

    override def pushOpcode: T86Opcode.UnaryOp = T86Opcode.PUSH

    override def popOpcode: T86Opcode.UnaryOp = T86Opcode.POP
  }

  val fregRegisterAllocator = new GenericNaiveRegisterAllocator[Operand.FReg] with T86FRegRegisterAllocator {
    /** Machine registers are in the range (0, machineRegCount)- */
    override def machineRegCount: Int = _machineFRegCount

    override def pushOpcode: T86Opcode.UnaryOp = T86Opcode.FPUSH

    override def popOpcode: T86Opcode.UnaryOp = T86Opcode.FPOP
  }

  override def transformProgram(program: T86Program): Unit = {
    program.funs.foreach(transformFun)
  }

  def transformFun(fun: T86Fun): Unit = {
    regRegisterAllocator.transformFun(fun)
    fregRegisterAllocator.transformFun(fun)
  }
}