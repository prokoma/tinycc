package tinycc.frontend.ast

import tinycc.frontend.parser.Symbols
import tinycc.util.{IndentWriter, IterableForeachSep}

class AstPrinterC extends AstPrinter {
  private var structTypes: Set[Symbol] = Set.empty

  override def print(node: AstNode, out: IndentWriter): Unit = {
    structTypes = Set.empty
    super.print(node, out)
  }

  override def printNode(node: AstNode, isStmt: Boolean, out: IndentWriter): Unit = {
    def semicolon: Unit = if (isStmt) out.write(";")

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

          case None => semicolon
        }

      case node: AstSequence =>
        if(isStmt && node.body.exists(_.isInstanceOf[AstVarDecl]))
          node.body.foreach(printAsStmt(_, out))
        else
          super.printNode(node, isStmt, out)

      case node: AstStructDecl =>
        structTypes += node.symbol
        super.printNode(node, isStmt, out)

      case node: AstCast =>
        out.write("((")
        printAsExpr(node.newTy, out)
        out.write(")(")
        printAsExpr(node.expr, out)
        out.write("))")
        semicolon

      case node => super.printNode(node, isStmt, out)
    }
  }

  override protected def printType(node: AstType, out: IndentWriter): Unit = node match {
    case node: AstNamedType if node.symbol == Symbols.kwInt =>
      out.write("int64_t")

    case node: AstNamedType if structTypes.contains(node.symbol) =>
      out.write(s"struct ${node.symbol.name}")

    case node => super.printType(node, out)
  }
}
