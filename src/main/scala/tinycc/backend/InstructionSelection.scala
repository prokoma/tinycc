package tinycc.backend

import tinycc.common.ir.IrProgram

abstract class InstructionSelection[T](program: IrProgram) {
  def result: Either[BackendException, T]
}