package alien

import o1.*

import scala.collection.mutable

class Game:
  var player: Player = Player()
  val enemies: mutable.HashMap[Enemy, mutable.ArrayBuffer[CompassDir]] = mutable.HashMap(
    Enemy(
      "alien",
      100,
      "You have been killed by the Xenomorph. Its horrifying inner jaw was the last thing you saw before getting your chest pried open.",
    ) -> mutable.ArrayBuffer(South, East, East, East, South),
    Enemy(
      "android",
      25,
      "You have been killed by an android. You have been betrayed by that which was made to serve you."
    ) -> mutable.ArrayBuffer(South, South, East),
  )
  private val _map: Room = Room.map
  // Path to the player from the starting room.
  // Invariant: must lead to a valid Room when traversed from the starting room.
  private val _playerPos: mutable.ArrayBuffer[CompassDir] =
    mutable.ArrayBuffer.empty
  // 0 is for the player, 1+ is for enemies
  private var currentTurn: Int = 0

  def debug(): Unit =
    println(s"Player: ${this.player} @ ${this._playerPos}")
    println(s"Enemies: ${this.enemies}")

  def traverse(path: Vector[CompassDir]): Option[Room] = this._map.traverse(path)

  def playerRoom: Room = this.traverse(this._playerPos.toVector) match
    case Some(room) => room
    case None => throw RuntimeException(
      "invariant violated: Game._playerPos does not lead to a Room")

  def isWon: Boolean =
    this.player.health > 0 && this.playerRoom.name == "escape pod"
  def isLost: Boolean = this.player.health == 0

  // Have the player take an item from the room they're in, returning whether
  // the item was taken.
  def take(idx: Int): Boolean =
    this.playerRoom.removeItem(idx).map(this.player.takeItem).isDefined
  // Have the player drop an item into the room they're in, returning whether
  // the item was dropped.
  def drop(idx: Int): Boolean =
    this.player.dropItem(idx).map(this.playerRoom.addItem).isDefined
  // Have the player use an item, returning whether the item was used.
  def use(idx: Int): Boolean =
    this.player.dropItem(idx).map(item => println(item.use(this))).isDefined

  // Try to move the player in a direction, returning whether the player
  // actually moved.
  def move(dir: CompassDir): Boolean =
    if this._playerPos.lastOption.exists(_.opposite == dir) then
      this._playerPos.dropRightInPlace(1); true
    else this.playerRoom.traverse(dir).map(_ => this._playerPos += dir).isDefined

  // End the current entity's turn.
  def endTurn(): Unit =
    this.currentTurn =
      if this.currentTurn == this.enemies.size then 0
      else this.currentTurn + 1

  // Have the next entity in line take its turn.
  // Returns whether it's the player's turn.
  def takeTurn(): Boolean =
    this.currentTurn match
      case 0 => true // Players already take their turn through input
      case idx => this.enemies.keys.toVector(idx - 1).takeTurn(this); false
end Game