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

  // Have the player pick up an item from the room they're in, returning whether
  // the item was picked up.
  def pickUp(idx: Int): Boolean =
    this.playerRoom.removeItem(idx).map(this.player.pickUpItem(_)).isDefined
  // Have the player drop an item into the room they're in, returning whether
  // the item was dropped.
  def drop(idx: Int): Boolean =
    this.player.dropItem(idx).map(this.playerRoom.addItem(_)).isDefined
end Game