package alien

import scala.util.Random

class Enemy(val name: String, val damage: Int, val killDescription: String):
  def takeTurn(game: Game): Unit =
    val ownPath = game.enemies(this)
    game.traverse(game.enemies(this).toVector) match
      case Some(room) =>
        // hurt player if they happen to walk at us
        if room == game.playerRoom then
          println(s"${this.name.capitalize} hurts you for ${game.player.hurt(this.damage)} health.")

        // walk forward in some direction and hurt the player there
        val exits = room.exits
        val rand = Random.nextInt(exits.size)
        val (dir, moveTo) = (exits.keys.toVector(rand), exits.values.toVector(rand))
        ownPath.append(dir)
        if game.traverse(ownPath.toVector) == game.playerRoom then
          println(s"${this.name.capitalize} hurts you for ${game.player.hurt(this.damage)} health.")
        game.endTurn()
      case None =>
        println("Invariant violated: enemy path does not point to a valid room")
        System.exit(1)

  override def toString: String = this.name
end Enemy