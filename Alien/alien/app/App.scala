package alien.app

import alien.Game

import scala.io.StdIn
import scala.util.Random

@main def main(): Unit =
  val game = Game()

  typeln("You awake on the medical bay of the space tug Narcissus after being put in cryogenic sleep.")
  typeln("It has been a long sleep, and you are puzzled to find no one on the now-derelict ship.")
  typeln("From the captain's log you learnt your ship has been ravaged by rogue androids and an aggressive Xenomorph.")
  typeln("You are the last of the crew to remain.")
  typeln("The escape pod is your only chance of survival.")
  typeln("Outwit your enemies and escape.")

  while !game.isWon && !game.isLost do
    Command.parse(StdIn.readLine("> ")) match
      case Some(cmd) => cmd.execute(game)
      case None => println("Invalid command. See 'help' for a list of commands.")

  typeln(
    if game.isWon then
      "You escape from the spaceship, hoping you never face the alien again. Or will you?"
    else
      "You died. Space is cruel. No one can hear you scream in the vastness of space. <Dark Souls swoosh>"
  )

def typeln(s: String): Unit =
  def sleepRandom(): Unit = Thread.sleep(Random.nextInt(30) + 10)
  s.foreach(ch =>
    print(ch)
    sleepRandom())
  println()