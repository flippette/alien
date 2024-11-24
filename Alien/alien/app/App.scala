package alien.app

import alien.Game

import scala.io.StdIn

@main def main() =
  val game = Game()

  while !game.isWon && !game.isLost do
    Command.parse(StdIn.readLine("> ")) match
      case Some(cmd) => cmd.execute(game)
      case None => println("Invalid command")

  println(
    if game.isWon then
      "You escape from the spaceship, hoping you never face the alien again."
    else
      "You died. <Dark Souls swoosh>"
  )

  System.exit(0)