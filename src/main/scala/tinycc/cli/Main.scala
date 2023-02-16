package tinycc.cli

object Main extends App {
  val action = CliParser.parseArgs(args)

  action.execute()
}
