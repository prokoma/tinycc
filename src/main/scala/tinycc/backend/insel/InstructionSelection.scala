package tinycc.backend.insel

import tinycc.common.ir.IrProgram
import tinycc.util.Logging

/** A generic interface implemented by all target-specific instruction selectors.
 * @tparam T the type of the target code representation
 */
abstract class InstructionSelection[T](program: IrProgram) extends Logging {
  def result(): T
}