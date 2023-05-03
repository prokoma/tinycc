package tinycc.backend.t86.insel

import tinycc.backend.insel.{InstructionSelection, MaximalMunch}
import tinycc.backend.t86.T86Program
import tinycc.common.ir._

abstract class T86InstructionSelection(program: IrProgram) extends InstructionSelection[T86Program](program)

/** A factory for tiny86 instruction selection, which by defaults uses tiling instruction selection with the standard [[GenRules]]
 *  ruleset and maximal munch algorithm. */
object T86InstructionSelection {
  def apply(prog: IrProgram): T86InstructionSelection = new T86TilingInstructionSelection(prog) with GenRules with MaximalMunch
}