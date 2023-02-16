package tinycc.cli

import tinycc.frontend
import tinycc.frontend.TinyCParser
import tinycc.frontend.analysis.{SemanticAnalysis, TypeAnalysis}

import java.nio.file.{Files, Path}

trait Action extends Product with Serializable {
  def execute(): Unit
}

object Action {
  case class Compile(file: Path) extends Action {
    override def execute(): Unit = {
      val str = Files.readString(file)
      val ast = TinyCParser.parseProgram(str)
      val decls = new SemanticAnalysis(ast).result.right.get
      val typeMap = new TypeAnalysis(ast, decls).result.right.get
      val irProg = new frontend.Compiler(ast, decls, typeMap).result
      Console.println(irProg)
    }
  }
}
