package tinycc.backend.t86

import tinycc.backend.BackendException

import scala.collection.mutable

/** Resolves labels to instruction addresses. */
class T86LabelProcessor(program: T86Program) {
  protected val labelAddressMap: mutable.Map[Symbol, Long] = mutable.Map.empty

  protected def resolveLabelsInOperand(operand: Operand): (Operand, List[T86Comment]) = operand match {
    case Operand.Label(symbol) =>
      val addr = labelAddressMap.getOrElse(symbol, throw new BackendException(s"Undefined label $symbol"))
      (Operand.Imm(addr), List(T86Comment(s"${symbol.name} -> $addr")))

    case operand => (operand, Nil)
  }

  lazy val result: Either[BackendException, T86Program] = {
    var address = 0
    program.foreach({
      case _: T86Insn =>
        address += 1

      case T86Label(symbol) =>
        if(labelAddressMap.contains(symbol))
          throw new BackendException(s"Duplicate label $symbol")
        labelAddressMap(symbol) = address

      case _ =>
    })

    val newProgram = Seq.newBuilder[T86ProgramElement]
    program.foreach({
      case insn: NullaryT86Insn => newProgram += insn

      case UnaryT86Insn(op, operand0) =>
        val (newOperand0, comments0) = resolveLabelsInOperand(operand0)
        newProgram ++= comments0
        newProgram += UnaryT86Insn(op, newOperand0)

      case BinaryT86Insn(op, operand0, operand1) =>
        val (newOperand0, comments0) = resolveLabelsInOperand(operand0)
        val (newOperand1, comments1) = resolveLabelsInOperand(operand1)
        newProgram ++= comments0
        newProgram ++= comments1
        newProgram += BinaryT86Insn(op, newOperand0, newOperand1)

      case T86Label(symbol) =>
        newProgram += T86Comment(s"${symbol.name}:")

      case elem =>
        newProgram += elem
    })

    Right(newProgram.result)
  }
}
