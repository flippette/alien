package alien

trait Item(val name: String):
  def use(game: Game): String = "You twiddle your thumbs, wondering what it is."
  def expired: Boolean = true
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

class Cctv(private var batteryLife: Int) extends Item("CCTV monitor"):
  override def use(game: Game): String =
    this.batteryLife match
      case 0 => "The CCTV monitor has run out of battery."
      case _ =>
        this.batteryLife -= 1
        s"You view the CCTV footage and locate enemies in these rooms: ${
          game.enemies.values
            .map(_.toVector)
            .flatMap(game.traverse)
            .map(_.name)
            .mkString(", ")
            .trim
        }.\n" +
          (if this.expired then
            "The monitor has run out of battery."
          else
            s"The device can still be used for ${this.batteryLife} turns.")

  override def expired: Boolean = this.batteryLife == 0

  override def toString: String =
    "This is a tablet that can access CCTV footage on ship facilities.\n" +
      s"It has enough battery for ${this.batteryLife} more turns."
end Cctv