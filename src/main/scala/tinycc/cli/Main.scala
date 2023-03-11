package tinycc.cli

import tinycc.cli.Action.Help

object Main extends App {
  try {
    val action = CliParser.parseArgs(args)

    action.execute()
  } catch {
    case ex: CliException =>
      Console.err.println(ex.getMessage)
      sys.exit(1)
  }
}
