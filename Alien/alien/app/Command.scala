package alien.app

import alien.Game

enum Command(literal: String):
  case Help extends Command("help")
  case Quit extends Command("quit")
  case Player extends Command("player")
  case Room extends Command("room")

  // TODO(linh): fill in help text
  def execute(game: Game): Unit =
    this match
      case Help => println("<help text here>")
      case Quit => println("bye."); System.exit(0)
      case Player => println(game.player)
      case Room => println(game.playerRoom)
end Command

object Command:
  def parse(input: String): Option[Command] =
    val args = input.split(' ')
    args.headOption.map(_.toLowerCase).flatMap({
      case "help" => Some(Command.Help)
      case "quit" => Some(Command.Quit)
      case "player" => Some(Command.Player)
      case "room" => Some(Command.Room)
      case _ => None
    })
end Command