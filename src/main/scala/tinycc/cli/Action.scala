package tinycc.cli

import tinycc.backend.BasicBlockScheduling
import tinycc.backend.t86.{T86AsmPrinter, T86InstructionSelection}
import tinycc.common.ir.IrPrinter
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

      Console.out.print(str)

      t match {
        case Left(ex) =>
          Console.err.println(ex.format(reporter))

        case Right(irProg) =>
          Console.out.print(new IrPrinter().printToString(irProg))

          (new BasicBlockScheduling).transformProgram(irProg)
          val t86Asm = T86InstructionSelection(irProg).result.toTry.get

          Console.out.print(new T86AsmPrinter().printToString(t86Asm))
      }
    }
  }

//  case class Run(file: Path) extends Action {
//    override def execute(): Unit = {
//      val str = Files.readString(file)
//      val reporter = new Reporter(str, Some(file.getFileName.toString))
//
//      val t = for (
//        ast <- TinyCParser.parseProgram(str);
//        decls <- new SemanticAnalysis(ast).result;
//        typeMap <- new TypeAnalysis(ast, decls).result;
//        irProg <- new TinyCCompiler(ast, decls, typeMap).result;
//        t86Asm <- T86InstructionSelection(irProg).result
//      ) yield (irProg, t86Asm)
//
//      t match {
//        case Left(ex) =>
//          Console.err.println(ex.format(reporter))
//
//        case Right((irProg, t86Asm)) =>
//          Console.out.print(new IrPrinter().printToString(irProg))
//          Console.out.print(new T86AsmPrinter().printToString(t86Asm))
//      }
//    }
//  }
}
