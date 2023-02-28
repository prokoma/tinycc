package tinycc.cli

import tinycc.backend.BasicBlockScheduling
import tinycc.backend.t86.{T86AsmPrinter, T86InstructionSelection}
import tinycc.common.ir.IrPrinter
import tinycc.frontend.analysis.{SemanticAnalysis, TypeAnalysis}
import tinycc.frontend.ast.{AstPrinter, AstPrinterC}
import tinycc.frontend.{TinyCCompiler, TinyCParser}

import java.io.PrintStream
import java.nio.file.{Files, Path}

trait Action extends Product with Serializable {
  def execute(): Unit
}

trait ConsoleOrFileOutput {
  def outFile: Option[Path]

  def withPrintStream(f: PrintStream => Any): Unit = outFile match {
    case Some(path) =>
      util.Using(new PrintStream(Files.newOutputStream(path)))(f)

    case None =>
      f(Console.out)
  }
}

object Action {
  case class TranspileToC(file: Path, outFile: Option[Path] = None, prefix: Seq[String] = Seq.empty) extends Action with ConsoleOrFileOutput {
    override def execute(): Unit = {
      val str = Files.readString(file)
      val reporter = new Reporter(str, Some(file.getFileName.toString))

      withPrintStream(out => {
        val chain = for {
          ast <- TinyCParser.parseProgram(str)
        } yield {
          prefix.foreach(out.println)
          out.println(new AstPrinterC().printToString(ast))
        }

        chain match {
          case Left(ex) =>
            Console.err.println(ex.format(reporter))

          case Right(_) =>
        }
      })
    }
  }

  case class Compile(file: Path) extends Action {
    override def execute(): Unit = {
      val str = Files.readString(file)
      val reporter = new Reporter(str, Some(file.getFileName.toString))

      val chain = for {
        ast <- TinyCParser.parseProgram(str)
        _ = Console.out.println(new AstPrinter().printToString(ast))

        decls <- new SemanticAnalysis(ast).result
        typeMap <- new TypeAnalysis(ast, decls).result
        irProg <- new TinyCCompiler(ast, decls, typeMap).result
        _ = Console.out.println(new IrPrinter().printToString(irProg))

        _ = (new BasicBlockScheduling).transformProgram(irProg)
        t86Asm <- T86InstructionSelection(irProg).result
        _ = Console.out.print(new T86AsmPrinter().printToString(t86Asm))
      } yield ()

      chain match {
        case Left(ex) =>
          Console.err.println(ex.format(reporter))

        case Right(_) =>
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
