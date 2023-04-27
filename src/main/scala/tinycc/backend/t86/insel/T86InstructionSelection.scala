package tinycc.backend.t86.insel

import tinycc.backend.insel.{InstructionSelection, MaximalMunch}
import tinycc.backend.t86.T86Program
import tinycc.common.ir._

abstract class T86InstructionSelection(program: IrProgram) extends InstructionSelection[T86Program](program)

object T86InstructionSelection {
  def apply(prog: IrProgram): T86InstructionSelection = new T86TilingInstructionSelection(prog) with GenRules with MaximalMunch
}