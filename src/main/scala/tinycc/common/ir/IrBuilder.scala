package tinycc.common.ir

class IrBuilder {
  var funOption: Option[IrFunction] = None

  def fun: IrFunction = funOption.get

  var bbOption: Option[BasicBlock] = None

  def bb: BasicBlock = bbOption.get

  def insert(bb: BasicBlock): BasicBlock

  def insert(bb: IrFunction => BasicBlock): BasicBlock

  def insert[T <: IrInsn](insn: T): T

  def insert[T <: IrInsn](insn: BasicBlock => T): T
}

object IrBuilder {
  def apply()
}