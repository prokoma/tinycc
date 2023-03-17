package tinycc.backend.t86

import tinycc.backend.BackendException

import scala.collection.mutable

/** Resolves labels to instruction addresses. */
class T86LabelProcessor(listing: T86Listing) {
  protected val labelAddressMap: mutable.Map[Symbol, Long] = mutable.Map.empty

  protected def resolveLabelsInOperand(operand: Operand): (Operand, List[T86Comment]) = operand match {
    case Operand.Label(symbol) =>
      val addr = labelAddressMap.getOrElse(symbol, throw new BackendException(s"Undefined label $symbol"))
      (Operand.Imm(addr), List(T86Comment(s"$addr -> ${symbol.name}")))

    case operand => (operand, Nil)
  }

  def result(): T86Listing = {
    var address = 0
    listing.foreach({
      case _: T86Insn =>
        address += 1

      case T86Label(symbol) =>
        if (labelAddressMap.contains(symbol))
          throw new BackendException(s"Duplicate label $symbol")
        labelAddressMap(symbol) = address

      case _ =>
    })

    val newProgram = Seq.newBuilder[T86ListingElement]
    listing.foreach({
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
        newProgram += T86Comment(s"${symbol.name}:", false)

      case elem =>
        newProgram += elem
    })

    newProgram.result()
  }
}

object T86LabelProcessor {
  def computeAddresses(listing: T86Listing): Seq[(Long, T86ListingElement)] = {
    val addrMap = mutable.Map.empty[Symbol, Long].withDefaultValue(0)
    var curSection = Symbol("")

    listing.map({
      case elem@T86SectionLabel(name) =>
        curSection = name
        (addrMap(curSection), elem)

      case insn: T86Insn =>
        addrMap(curSection) += 1
        (addrMap(curSection) - 1, insn)

      case dw@T86DataWord(value, rep) =>
        addrMap(curSection) += rep
        (addrMap(curSection) - rep, dw)

      case elem => (addrMap(curSection), elem)
    })
  }
}