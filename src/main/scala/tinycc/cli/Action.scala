package tinycc.cli

import tinycc.frontend.analysis.{SemanticAnalysis, TypeAnalysis}
import tinycc.frontend.{TinyCCompiler, TinyCParser}

import java.nio.file.{Files, Path}

trait Action extends Product with Serializable {
  def execute(): Unit
}

object Action {
  case class Compile(file: Path) extends Action {
    override def execute(): Unit = {
      val str = Files.readString(file)
      val reporter = new Reporter(str, Some(file.getFileName.toString))

      val t = for(
        ast <- TinyCParser.parseProgram(str);
        decls <- new SemanticAnalysis(ast).result;
        typeMap <- new TypeAnalysis(ast, decls).result;
        irProg <- new TinyCCompiler(ast, decls, typeMap).result
      ) yield irProg

      t match {
        case Left(ex) =>
          Console.err.println(ex.format(reporter))

        case Right(irProg) =>
          Console.out.print(irProg)
      }
    }
  }
}
