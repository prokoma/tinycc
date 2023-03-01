package tinycc.frontend.ast

import tinycc.util.{IndentPrinter, IndentWriter, IterableForeachSep}

// TODO: handle operator precedence
class AstPrinter extends IndentPrinter[AstNode] {
  override def print(node: AstNode, out: IndentWriter): Unit = printAsExpr(node, out)

  protected def printAsExpr(node: AstNode, out: IndentWriter): Unit = printNode(node, isStmt = false, out)

  protected def printAsStmt(node: AstNode, out: IndentWriter): Unit = printNode(node, isStmt = true, out)

  protected def printNode(node: AstNode, isStmt: Boolean, out: IndentWriter): Unit = {
    def semicolon: Unit = if (isStmt) out.write(";")

    node match {
      case node: AstType => printType(node, out); semicolon

      case node: AstInteger => out.write(node.value.toString); semicolon
      case node: AstDouble => out.write(node.value.toString); semicolon
      case node: AstChar => printStringLiteral(node.value.toString, '\'', out); semicolon
      case node: AstString => printStringLiteral(node.value, '\"', out); semicolon
      case node: AstIdentifier => out.write(node.symbol.name); semicolon

      case node: AstBinaryOp =>
        out.write("(")
        printAsExpr(node.left, out)
        out.write(s" ${node.op.name} ")
        printAsExpr(node.right, out)
        out.write(")")
        semicolon

      case node: AstAssignment =>
        printAsExpr(node.lvalue, out)
        out.write(s" = ")
        printAsExpr(node.value, out)
        semicolon

      case node: AstUnaryOp =>
        out.write(node.op.name)
        printAsExpr(node.expr, out)
        semicolon

      case node: AstUnaryPostOp =>
        printAsExpr(node.expr, out)
        out.write(node.op.name)
        semicolon

      case node: AstAddress =>
        out.write("&")
        printAsExpr(node.expr, out)
        semicolon

      case node: AstDeref =>
        out.write("*")
        printAsExpr(node.expr, out)
        semicolon

      case node: AstIndex =>
        out.write("&")
        printAsExpr(node.expr, out)
        semicolon

      case node: AstMember =>
        printAsExpr(node.expr, out)
        out.write(s".${node.member.name}")
        semicolon

      case node: AstMemberPtr =>
        printAsExpr(node.expr, out)
        out.write(s"->${node.member.name}")
        semicolon

      case node: AstCall =>
        printAsExpr(node.expr, out)
        out.write("(")
        node.args.foreachSep(printAsExpr(_, out), out.write(", "))
        out.write(")")
        semicolon

      case node: AstCast =>
        out.write("cast<")
        printAsExpr(node.newTy, out)
        out.write(">(")
        printAsExpr(node.expr, out)
        out.write(")")
        semicolon

      case node: AstRead =>
        out.write("scan()")
        semicolon

      case node: AstDecl =>
        printDecl(node, isStmt, out)

      case node: AstSequence =>
        node.body.foreachSep(printAsExpr(_, out), out.write(", "))
        semicolon

      case node: AstBlock =>
        out.write("{")
        out.withIndent({
          node.body.foreachSep(printAsStmt(_, out), out.nl())
        })
        out.write("}")

      case node: AstProgram =>
        node.body.foreachSep(printAsStmt(_, out), out.nl())

      case node: AstIf =>
        out.write("if (")
        printAsExpr(node.guard, out)
        out.write(")")
        printAsStmt(node.trueCase, out)
        node.falseCase.foreach(falseCase => {
          out.write("else")
          printAsStmt(falseCase, out)
        })

      case node: AstSwitch =>
        out.write("switch(")
        printAsExpr(node.guard, out)
        out.write(") {")
        out.withIndent({
          node.cases.foreach({ case (value, body) =>
            out.write(s"case ${value}:")
            printAsStmt(body, out)
          })
          node.defaultCase.foreach(body => {
            out.write(s"default:")
            printAsStmt(body, out)
          })
        })
        out.write("}")

      case node: AstWhile =>
        out.write("while (")
        printAsExpr(node.guard, out)
        out.write(")")
        printAsStmt(node.body, out)

      case node: AstDoWhile =>
        out.write("do ")
        printAsStmt(node.body, out)
        out.write("while (")
        printAsExpr(node.guard, out)
        out.write(")")
        semicolon

      case node: AstFor =>
        out.write("for (")
        node.init.foreach(init => printAsExpr(init, out))
        out.write(";")
        node.guard.foreach(guard => {
          out.write(" ")
          printAsExpr(guard, out)
        })
        out.write(";")
        node.increment.foreach(increment => {
          out.write(" ")
          printAsExpr(increment, out)
        })
        out.write(")")
        printAsStmt(node.body, out)

      case _: AstBreak => out.write("break"); semicolon
      case _: AstContinue => out.write("continue;"); semicolon

      case node: AstReturn =>
        out.write("return")
        node.expr.foreach(expr => {
          out.write(" ")
          printAsExpr(expr, out)
        })
        semicolon

      case node: AstWrite =>
        out.write("print(")
        printAsExpr(node.expr, out)
        out.write(")")
        semicolon

      case node: AstWriteNum =>
        out.write("printnum(")
        printAsExpr(node.expr, out)
        out.write(")")
        semicolon
    }
  }

  protected def printDecl(node: AstDecl, isStmt: Boolean, out: IndentWriter): Unit = {
    def semicolon: Unit = if (isStmt) out.write(";")

    node match {
      case node: AstVarDecl =>
        printAsExpr(node.varTy, out)
        out.write(s" ${node.symbol.name}")
        node.value.foreach(value => {
          out.write(" = ")
          printAsExpr(value, out)
        })
        semicolon

      case node: AstFunDecl =>
        printAsExpr(node.returnTy, out)
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

      case node: AstStructDecl =>
        out.write(s"struct ${node.symbol.name}")
        node.fields.foreach(fields => {
          out.write("{")
          out.withIndent(fields.foreach({ case (ty, symbol) =>
            printAsExpr(ty, out)
            out.write(s" ${symbol.name};")
          }), out.nl())
          out.write("}")
        })
        semicolon

      case node: AstFunPtrDecl =>
        out.write(s"typedef ")
        printAsExpr(node.returnTy, out)
        out.write(s"(*${node.symbol.name})")
        node.argTys.foreachSep(printAsExpr(_, out), out.write(", "))
        out.write(")")
        semicolon
    }
  }

  protected def printType(node: AstType, out: IndentWriter): Unit = node match {
    case node: AstPointerType =>
      printType(node.base, out)
      out.write("*")

    case node: AstArrayType =>
      printType(node.base, out)
      out.write("[]")

    case node: AstNamedType => out.write(node.symbol.name)
  }

  protected def printStringLiteral(str: String, quote: Char, out: IndentWriter): Unit = {
    out.write(quote)
    for (c <- str) c match {
      case '\r' => out.write("\\r")
      case '\n' => out.write("\\n")
      case '\t' => out.write("\\t")
      case '\\' => out.write("\\\\")
      case '"' => out.write("\\\"")
      case '\'' => out.write("\\\'")
      case c => out.write(c)
    }
    out.write(quote)
  }
}
