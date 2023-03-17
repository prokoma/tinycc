package tinycc.backend.t86

import tinycc.util.{IndentPrinter, IndentWriter, IterableForeachSep}

class T86AsmPrinter extends IndentPrinter[T86Listing] {
  override def print(obj: T86Listing, out: IndentWriter): Unit = {
//    out.indent()
    T86LabelProcessor.computeAddresses(obj).foreach({ case (addr, el) => printElement(addr, el, out) })
  }

  private def printElement(addr: Long, el: T86ListingElement, out: IndentWriter): Unit = el match {
    case T86Comment(value, indent) =>
      if(indent)
        out.write(s"#     $value\n")
      else
        out.write(s"# $value\n")

    case el: T86SpecialLabel =>
      out.write(s"# $el\n")

    case T86SectionLabel(symbol) =>
//      out.dedent()
      out.write("." + symbol.name)
      out.nl()
//      out.indent()

    case T86Label(symbol) =>
//      out.dedent()
      out.write(symbol.name + ":")
//      out.indent()

    case T86DataWord(value, rep) =>
      out.write(f"$addr%4d  DW $value")
      if(rep > 1)
        out.write(s" * $rep")
      out.nl()

    case insn: T86Insn =>
      printInsn(addr, insn, out)
      out.nl()
  }

  private def printInsn(addr: Long, insn: T86Insn, out: IndentWriter): Unit = {
    out.write(f"$addr%4d  ")

    out.write(insn.op.toString)
    if (insn.operands.nonEmpty) {
      out.write(" ")
      insn.operands.foreachSep(operand => out.write(operand.toString), out.write(", "))
    }
  }
}
