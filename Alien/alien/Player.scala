package alien

import scala.collection.mutable
import scala.math.{min, max}

class Player(val maxHealth: Int = 100):
  private var _health: Int = this.maxHealth
  private val _inventory: mutable.ArrayBuffer[Item] = mutable.ArrayBuffer.empty

  def health: Int = this._health
  def heal(amount: Int): Int =
    val oldHealth = this._health
    this._health = min(this.maxHealth, oldHealth + amount)
    this._health - oldHealth
  def hurt(amount: Int): Int =
    val oldHealth = this._health
    this._health = max(0, oldHealth - amount)
    oldHealth - this._health

  def inventory: Vector[Item] = this._inventory.toVector
  def takeItem(item: Item): Unit = this._inventory.append(item)
  def dropItem(idx: Int): Option[Item] =
    this._inventory.lift(idx) match
      case Some(item) => this._inventory.remove(idx); Some(item)
      case None => None

  override def toString: String =
    s"Player:" +
      (if this.health > 50 then
        s" You are relatively healthy." 
      else 
        s" You are injured. You should patch up your wounds before carrying on.") +
      s" Your items: ${
        if this._inventory.isEmpty then "nothing"
        else this._inventory.map(_.name).mkString(", ").trim
      }"
end Player