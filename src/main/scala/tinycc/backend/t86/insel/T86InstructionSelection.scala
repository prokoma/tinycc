package tinycc.backend.t86.insel

import tinycc.backend.InstructionSelection
import tinycc.backend.t86.{T86Listing, T86Program}
import tinycc.common.ir._

abstract class T86InstructionSelection(program: IrProgram) extends InstructionSelection[T86Program](program)

object T86InstructionSelection {
  def apply(prog: IrProgram): T86InstructionSelection = new MaximalMunchT86InstructionSelection(prog)
}