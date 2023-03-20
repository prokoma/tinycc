package tinycc.util

import java.nio.file.{Files, Path}
import java.util.stream.Stream
import scala.jdk.CollectionConverters.IteratorHasAsScala

object Testing {
  val examplesDir: Path = Path.of("examples")

  private def listFilesRecursive(dir: Path): Stream[Path] = Files.list(dir).flatMap(file =>
    if (Files.isDirectory(file)) listFilesRecursive(file) else Stream.of(file))

  lazy val exampleSources: Seq[Path] = listFilesRecursive(examplesDir).filter(_.getFileName.toString.endsWith(".c")).iterator().asScala.toSeq
}
