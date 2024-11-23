package alien

trait Item(val name: String, val weight: Float):
  override def toString: String = "A strange, alien *ba dum tsss* item."
end Item

class Food(name: String, weight: Float, description: String, val nutrition: Int)
  extends Item(name, weight):
  override def toString: String =
      s"A ${this.name}. Consuming this will restore ${this.nutrition} HP."
end Food

class MotionTracker(private var batteryLife: Int)
  extends Item("motion tracker", 2.0):
  override def toString: String =
    s"A motion tracker, usable for ${this.batteryLife} turns."
end MotionTracker