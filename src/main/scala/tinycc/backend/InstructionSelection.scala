package tinycc.backend

import tinycc.common.ir.IrProgram
import tinycc.util.Logging

abstract class InstructionSelection[T](program: IrProgram) extends Logging {
  def result(): T
}