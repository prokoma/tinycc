package tinycc.frontend.ast

import tinycc.frontend.parser.Symbols
import tinycc.util.{IndentWriter, IterableForeachSep}

class AstPrinterC extends AstPrinter {
  override def printNode(node: AstNode, isStmt: Boolean, out: IndentWriter): Unit = {
    def semicolon(): Unit = if (isStmt) out.write(";")

    node match {
      case node: AstFunDecl if node.symbol.name == "main" =>
        super.printType(node.returnTy, out) // preserve int return type of main
        out.write(s" ${node.symbol.name}(")
        node.args.foreachSep({ case (ty, symbol) =>
          printAsExpr(ty, out)
          out.write(s" ${symbol.name}")
        }, out.write(", "))
        out.write(")")
        node.body match {
          case Some(body) =>
            out.write(" ")
            printAsStmt(body, out)

          case None => semicolon()
        }

      case node: AstCast =>
        out.write("((")
        printAsExpr(node.newTy, out)
        out.write(")(")
        printAsExpr(node.expr, out)
        out.write("))")
        semicolon()

      case node => super.printNode(node, isStmt, out)
    }
  }

  override protected def printType(node: AstType, out: IndentWriter): Unit = node match {
    case node: AstNamedType if node.symbol == Symbols.kwInt =>
      out.write("int64_t")

    case node => super.printType(node, out)
  }
}
