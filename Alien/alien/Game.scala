package alien

import o1.*

import scala.collection.mutable

class Game:
  val player: Player = Player()
  private val _map: Room = Room.map
  // Path to the player from the starting room.
  // Invariant: must lead to a valid Room when traversed from the starting room.
  private val _playerPos: mutable.ArrayBuffer[CompassDir] =
    mutable.ArrayBuffer.empty

  def playerRoom: Room = this._map.traverse(this._playerPos.toVector) match
    case Some(room) => room
    case None => throw RuntimeException(
      "invariant violated: Game._playerPos does not lead to a Room")

  def isWon: Boolean =
    this.player.health > 0 && this.playerRoom.name == "escape pod"
  def isLost: Boolean = this.player.health == 0

  // Have the player take an item from the room they're in, returning whether
  // the item was taken.
  def take(idx: Int): Boolean =
    this.playerRoom.removeItem(idx).map(this.player.takeItem(_)).isDefined
  // Have the player drop an item into the room they're in, returning whether
  // the item was dropped.
  def drop(idx: Int): Boolean =
    this.player.dropItem(idx).map(this.playerRoom.addItem(_)).isDefined
  // Have the player use an item, returning whether the item was used.
  def use(idx: Int): Boolean =
    this.player.dropItem(idx).map(item => println(item.use(this))).isDefined

  // Try to move the player in a direction, returning whether the player
  // actually moved.
  def move(dir: CompassDir): Boolean =
    if this._playerPos.lastOption.map(_.opposite == dir) == Some(true) then
      this._playerPos.dropRightInPlace(1); true
    else this.playerRoom.traverse(dir).map(_ => this._playerPos += dir).isDefined
end Game