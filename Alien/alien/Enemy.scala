package alien

import scala.util.Random
import scala.math.max

class Enemy(val name: String, val damage: Int, val hurtDescription: String, val killDescription: String):
  def takeTurn(game: Game): Unit =
    val ownPath = game.enemies(this)
    game.traverse(game.enemies(this).toVector) match
      case Some(room) =>
        // hurt player if they happen to walk at us
        if room == game.playerRoom then
          game.player.hurt(this.damage, this)

        // walk forward in some direction and hurt the player there
        val exits = room.exits.iterator.toVector
        val rand = Random.nextInt(max(exits.size - 1, 1))
        val (dir, moveTo) = exits(rand) match
          case (_, room) if room.name == "escape pod" => exits(rand + 1)
          case tup => tup
        ownPath.append(dir)
        if game.traverse(ownPath.toVector) == game.playerRoom then
          game.player.hurt(this.damage, this)
        game.endTurn()
      case None =>
        println("Invariant violated: enemy path does not point to a valid room")
        System.exit(1)

  override def toString: String = this.name
end Enemy