package tinycc.util

import java.nio.file.{Files, Path}
import java.util.stream.Stream
import scala.jdk.CollectionConverters.IteratorHasAsScala

object Testing {
  val examplesDir: Path = Path.of("examples")

  lazy val t86CliBinary: Path = {
    val t86Dir = Path.of("t86/build/t86-cli")
    Seq(t86Dir.resolve("t86-cli"), t86Dir.resolve("t86-cli.exe")).find(Files.exists(_)).getOrElse(
      throw new RuntimeException(s"Failed to locate t86-cli binary in ${t86Dir.toAbsolutePath.toString}"))
  }

  private def listFilesRecursive(dir: Path): Stream[Path] = Files.list(dir).flatMap(file =>
    if (Files.isDirectory(file)) listFilesRecursive(file) else Stream.of(file))

  lazy val exampleSources: Seq[Path] = listFilesRecursive(examplesDir).filter(_.getFileName.toString.endsWith(".c")).iterator().asScala.toSeq

  private def extractData(source: String, basePath: Path, char: Char): String = {
    val sb = new StringBuilder
    source.linesIterator.foreach(line => {
      val includePrefix = s"// $char! "
      val literalPrefix = s"// $char "
      if (line.startsWith(includePrefix)) {
        sb ++= Files.readString(basePath.resolve(line.drop(includePrefix.length)))
      } else if (line.startsWith(literalPrefix)) {
        sb ++= line.drop(literalPrefix.length) + "\n"
      }
    })
    sb.result()
  }

  def extractInData(source: String, basePath: Path): String = extractData(source, basePath, '<')

  def extractOutData(source: String, basePath: Path): String = extractData(source, basePath, '>')
}
