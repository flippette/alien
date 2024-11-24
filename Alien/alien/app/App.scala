package alien.app

import alien.Game

import scala.io.StdIn

@main def main() =
  val game = Game()

  while !game.isWon && !game.isLost do
    Command.parse(StdIn.readLine("> ")) match
      case Some(cmd) => cmd.execute(game)
      case None => println("invalid command")