package tinycc.cli

object Main extends App {
  try {
    val action = CliParser.parseArgs(args)

    action.execute()
  } catch {
    case ex: CliException =>
      Console.err.println(ex.getMessage)
      System.exit(1)
  }
}
