package alien.app

import alien.Game

import o1.*

enum Command(literal: String):
  case Help extends Command("help")
  case Quit extends Command("quit")
  case Player extends Command("player")
  case Room extends Command("room")
  case Move(dir: CompassDir) extends Command("move")

  // TODO(linh): fill in help text
  def execute(game: Game): Unit =
    this match
      case Help => println("<help text here>")
      case Quit => println("Bye."); System.exit(0)
      case Player => println(game.player)
      case Room => println(game.playerRoom)
      case Move(dir) => println(
        if game.move(dir) then s"You move $dir."
        else s"You try to move $dir, but it's blocked off."
      )
end Command

object Command:
  def parse(input: String): Option[Command] =
    val args = input.split(' ')
    args.headOption.map(_.toLowerCase).flatMap({
      case "help" => Some(Command.Help)
      case "quit" => Some(Command.Quit)
      case "player" => Some(Command.Player)
      case "room" => Some(Command.Room)
      case "move" => args.tail.headOption.map(_.toLowerCase).flatMap({
        case "north" => Some(CompassDir.North)
        case "east" => Some(CompassDir.East)
        case "south" => Some(CompassDir.South)
        case "west" => Some(CompassDir.West)
        case _ => None
      }).map(Command.Move.apply)
      case _ => None
    })
end Command