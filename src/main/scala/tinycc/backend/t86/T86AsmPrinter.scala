package tinycc.backend.t86

import tinycc.util.{IndentPrinter, IndentWriter, IterableForeachSep}

class T86AsmPrinter extends IndentPrinter[T86Program] {
  override def print(obj: T86Program, out: IndentWriter): Unit = {
    out.write(".text\n")
    out.indent()

    obj.foreach({
      case T86Label(symbol) =>
        out.dedent()
        out.write(symbol + ":\n")
        out.indent()

      case insn: T86Insn =>
        printInsn(insn, out)
        out.write("\n")
    })
  }

  private def printInsn(insn: T86Insn, out: IndentWriter): Unit = {
    out.write(insn.op.toString)
    if (insn.operands.nonEmpty) {
      out.write(" ")
      insn.operands.foreachSep(operand => out.write(operand.toString), out.write(", "))
    }
  }
}