package alien

trait Item(val name: String):
  def use(game: Game): String = "You twiddle your thumbs, wondering what it is."
  override def toString: String = "A strange, alien *ba dum tsss* item."
end Item

class Food(name: String, description: String, val nutrition: Int)
  extends Item(name):
  override def use(game: Game): String =
    s"You consume the ${this.name}, ${
      game.player.heal(this.nutrition) match
        case 0 => "but you were already full and didn't recover any health."
        case healed => s"and gain ${this.nutrition} health."
    }"
  override def toString: String =
    s"${this.description}. Consuming this will restore ${this.nutrition} health."
end Food

class MotionTracker(private var batteryLife: Int)
  extends Item("motion tracker"):
  override def toString: String =
    s"A motion tracker, usable for ${this.batteryLife} turns."
end MotionTracker