package alien.app

import alien.Game

import scala.io.StdIn
import scala.util.Random

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

def typeln(s: String): Unit =
  def sleepRandom(): Unit = Thread.sleep(Random.nextInt(250) + 100)
  s.foreach(ch =>
    print(ch)
    sleepRandom())
  println()