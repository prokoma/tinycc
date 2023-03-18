package tinycc.cli

object Main extends App {
  try {
    val action = CliParser.parseArgs(args.toSeq)

    action.execute()
  } catch {
    case ex: CliException =>
      Console.err.println(ex.getMessage)
      sys.exit(1)
  }
}
