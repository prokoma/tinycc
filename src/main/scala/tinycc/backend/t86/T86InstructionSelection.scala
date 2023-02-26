package tinycc.backend.t86

import tinycc.backend.InstructionSelection
import tinycc.common.ir._

abstract class T86InstructionSelection(program: IrProgram) extends InstructionSelection[T86Program](program)

object T86InstructionSelection {
  def apply(prog: IrProgram): T86InstructionSelection = new MaximalMunchT86InstructionSelection(prog)
}