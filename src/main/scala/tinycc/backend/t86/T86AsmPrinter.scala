package tinycc.backend.t86

import tinycc.util.{IndentPrinter, IndentWriter, IterableForeachSep}

class T86AsmPrinter extends IndentPrinter[T86Listing] {
  override def print(obj: T86Listing, out: IndentWriter): Unit = {
    out.indent()
    obj.foreach(printElement(_, out))
  }

  private def printElement(el: T86ListingElement, out: IndentWriter): Unit = el match {
    case T86Comment(value) =>
      out.write(s"# $value\n")

    case el: T86SpecialLabel =>
      out.write(s"# $el\n")

    case T86SectionLabel(symbol) =>
      out.dedent()
      out.write("." + symbol.name + "\n")
      out.indent()

    case T86Label(symbol) =>
      out.dedent()
      out.write(symbol.name + ":\n")
      out.indent()

    case insn: T86Insn =>
      printInsn(insn, out)
      out.write("\n")
  }

  private def printInsn(insn: T86Insn, out: IndentWriter): Unit = {
    out.write(insn.op.toString)
    if (insn.operands.nonEmpty) {
      out.write(" ")
      insn.operands.foreachSep(operand => out.write(operand.toString), out.write(", "))
    }
  }
}
