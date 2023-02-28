package tinycc.frontend.ast

import tinycc.util.{IndentPrinter, IndentWriter, IterableForeachSep}

// TODO: handle operator precedence
class AstPrinter extends IndentPrinter[AstNode] {
  override def print(node: AstNode, out: IndentWriter): Unit = node match {
    case node: AstProgram =>
      node.body.foreachSep(print(_, out), out.nl())

    case node: AstType => printType(node, out)

    case node: AstInteger => out.write(node.value.toString)
    case node: AstDouble => out.write(node.value.toString)
    case node: AstChar => out.write(s"'${node.value}'")
    case node: AstString => out.write(s"\"${node.value}\"")
    case node: AstIdentifier => out.write(node.symbol.name)

    case node: AstSequence =>
      node.body.foreachSep(print(_, out), out.write(", "))

    case node: AstBinaryOp =>
      print(node.left, out)
      out.write(s" ${node.op.name} ")
      print(node.right, out)

    case node: AstAssignment =>
      print(node.lvalue, out)
      out.write(s" = ")
      print(node.value, out)

    case node: AstUnaryOp =>
      out.write(node.op.name)
      print(node.expr, out)

    case node: AstUnaryPostOp =>
      print(node.expr, out)
      out.write(node.op.name)

    case node: AstAddress =>
      out.write("&")
      print(node.expr, out)

    case node: AstDeref =>
      out.write("*")
      print(node.expr, out)

    case node: AstIndex =>
      out.write("&")
      print(node.expr, out)

    case node: AstMember =>
      print(node.expr, out)
      out.write(s".${node.member.name}")

    case node: AstMemberPtr =>
      print(node.expr, out)
      out.write(s"->${node.member.name}")

    case node: AstCall =>
      print(node.expr, out)
      out.write("(")
      node.args.foreachSep(print(_, out), out.write(", "))
      out.write(")")

    case node: AstCast =>
      out.write("cast<")
      print(node.newTy, out)
      out.write(">(")
      print(node.expr, out)
      out.write(")")

    case node: AstRead =>
      out.write("scan()")

    case node =>
      printStmt(node, out)
  }

  protected def printStmt(node: AstNode, out: IndentWriter): Unit = node match {
    case node: AstDecl =>
      printDeclStmt(node, out)

    case node: AstSequence =>
      node.body.foreachSep(print(_, out), out.write(", "))

    case node: AstBlock =>
      out.write("{")
      out.withIndent({
        node.body.foreachSep(printStmt(_, out), out.nl())
      })
      out.write("}")

    case node: AstIf =>
      out.write("if (")
      print(node.guard, out)
      out.write(")")
      printStmt(node.trueCase, out)
      node.falseCase.foreach(falseCase => {
        out.write("else")
        printStmt(falseCase, out)
      })

    case node: AstSwitch =>
      out.write("switch(")
      print(node.guard, out)
      out.write(") {")
      out.withIndent({
        node.cases.foreach({ case (value, body) =>
          out.write(s"case ${value}:")
          print(body, out)
        })
        node.defaultCase.foreach(body => {
          out.write(s"default:")
          print(body, out)
        })
      })
      out.write("}")

    case node: AstWhile =>
      out.write("while (")
      print(node.guard, out)
      out.write(")")
      print(node.body, out)

    case node: AstDoWhile =>
      out.write("do ")
      print(node.body, out)
      out.write("while (")
      print(node.guard, out)
      out.write(");")

    case node: AstFor =>
      out.write("for (")
      node.init.foreach(init => print(init, out))
      out.write(";")
      node.guard.foreach(guard => {
        out.write(" ")
        print(guard, out)
      })
      out.write(";")
      node.increment.foreach(increment => {
        out.write(" ")
        print(increment, out)
      })
      out.write(")")
      printStmt(node.body, out)

    case _: AstBreak => out.write("break;")
    case _: AstContinue => out.write("continue;")

    case node: AstReturn =>
      out.write("return")
      node.expr.foreach(expr => {
        out.write(" ")
        print(expr, out)
      })
      out.write(";")

    case node: AstWrite =>
      out.write("print(")
      print(node.expr, out)
      out.write(");")

    case node =>
      print(node, out)
      out.write(";")
  }

  protected def printDeclStmt(node: AstDecl, out: IndentWriter): Unit = node match {
    case node: AstVarDecl =>
      print(node.varTy, out)
      out.write(s" ${node.symbol.name}")
      node.value.foreach(value => {
        out.write(" = ")
        print(value, out)
      })
      out.write(";")

    case node: AstFunDecl =>
      print(node.returnTy, out)
      out.write(s" ${node.symbol.name}(")
      node.args.foreachSep({ case (ty, symbol) =>
        print(ty, out)
        out.write(s" ${symbol.name}")
      }, out.write(", "))
      out.write(")")
      node.body match {
        case Some(body) =>
          out.write(" ")
          print(body, out)
        case None =>
          out.write(";")
      }

    case node: AstStructDecl =>
      out.write(s"struct ${node.symbol.name}")
      node.fields.foreach(fields => {
        out.write("{")
        out.withIndent(fields.foreach({ case (ty, symbol) =>
          print(ty, out)
          out.write(s" ${symbol.name};")
        }), out.nl())
        out.write("}")
      })
      out.write(";")

    case node: AstFunPtrDecl =>
      out.write(s"typedef ")
      print(node.returnTy, out)
      out.write(s"(*${node.symbol.name})")
      node.argTys.foreachSep(print(_, out), out.write(", "))
      out.write(");")
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
}
