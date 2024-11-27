package alien.app

import alien.Game

import o1.*

enum Command(literal: String):
  case Help extends Command("help")
  case Quit extends Command("quit")
  case Player extends Command("player")
  case Room extends Command("room")
  case Move(dir: CompassDir) extends Command("move")
  case Take(idx: Int) extends Command("take")
  case Drop(idx: Int) extends Command("drop")
  case Use(idx: Int) extends Command("use")
  case EllenRipley extends Command("ellenripley")

  def execute(game: Game): Unit =
    this match
      case Help => println("Available commands: quit, player, room, move <direction>, take <index>, drop <index>, use <index>")
      case Quit => println("Bye."); System.exit(0)
      case Player => println(game.player)
      case Room => println(game.playerRoom)
      case Move(dir) => println(
        if game.move(dir) then
          game.endTurn()
          s"You move ${dir.toString.toLowerCase} into the ${game.playerRoom.name}."
        else s"You try to move $dir, but it's blocked off."
      )
      case Take(idx) => println(
        if game.take(idx) then s"You take the ${game.player.inventory.last.name}."
        else "The room doesn't have this item."
      )
      case Drop(idx) => println(
        if game.drop(idx) then s"You drop the ${game.playerRoom.items.last.name}."
        else "You don't have this item."
      )
      case Use(idx) => game.use(idx)
      case EllenRipley =>
        game.player = alien.Player("Ellen Ripley", Int.MaxValue)
        println("You are now Ellen Ripley.")
        println("The main character. The heroine. HER.")
        println("You're not in danger. You ARE the danger.")
        println("Explore your powers.")
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
      case "take" => args.tail.headOption
        .flatMap(_.toIntOption)
        .map(Command.Take.apply)
      case "drop" => args.tail.headOption
        .flatMap(_.toIntOption)
        .map(Command.Drop.apply)
      case "use" => args.tail.headOption
        .flatMap(_.toIntOption)
        .map(Command.Use.apply)
      case "ellenripley" => Some(Command.EllenRipley)
      case _ => None
    })
end Command