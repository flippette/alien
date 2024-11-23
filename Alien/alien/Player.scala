package alien

import scala.collection.mutable.Buffer
import scala.math.{min, max}

class Player(val maxHealth: Int = 100,
             val speed: Int = 1,
             val baseWeight: Float = 1):
  private var _health: Int = this.maxHealth
  private val _inventory: Buffer[Item] = Buffer.empty

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
  def pickUpItem(item: Item): Unit = this._inventory.append(item)
  def dropItem(idx: Int): Option[Item] =
    val item = this._inventory.lift(idx)
    this._inventory.remove(idx)
    item

  def weight: Float = this.baseWeight + this._inventory.map(_.weight).sum
end Player