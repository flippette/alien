package alien

trait Item(val name: String, val weight: Float):
  def use(game: Game): String = "You twiddle your thumbs, wondering what it is."
  override def toString: String = "A strange, alien *ba dum tsss* item."
end Item

class Food(name: String, weight: Float, description: String, val nutrition: Int)
  extends Item(name, weight):
  override def use(game: Game): String =
    s"You consume the ${this.name}, ${
      game.player.heal(this.nutrition) match
        case 0 => "but you were already full and didn't recover any health."
        case healed => s"and gain ${this.nutrition} health."
    }"
  override def toString: String =
      s"${this.description}. Consuming this will restore ${this.nutrition} HP."
end Food

class MotionTracker(private var batteryLife: Int)
  extends Item("motion tracker", 2.0):
  override def toString: String =
    s"A motion tracker, usable for ${this.batteryLife} turns."
end MotionTracker