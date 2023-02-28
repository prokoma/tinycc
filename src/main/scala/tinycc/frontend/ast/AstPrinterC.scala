package tinycc.frontend.ast
import tinycc.util.IndentWriter

class AstPrinterC extends AstPrinter {
  override def print(node: AstNode, out: IndentWriter): Unit = node match {
    case node: AstCast =>
      out.write("((")
      print(node.newTy, out)
      out.write(")(")
      print(node.expr, out)
      out.write("))")

    case node => super.print(node, out)
  }
}
