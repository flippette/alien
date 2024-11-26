package alien

import o1.*

import scala.collection.mutable

class Room(val name: String,
           withItems: Vector[Item] = Vector.empty,
           withExits: Map[CompassDir, Room] = Map.empty):
  private val _items: mutable.ArrayBuffer[Item] =
    mutable.ArrayBuffer.empty ++ withItems
  // FIXME: inefficient
  private val _exits: mutable.HashMap[CompassDir, Room] =
    mutable.HashMap.empty ++ withExits

  def items: Vector[Item] = this._items.toVector
  def addItem(item: Item): Unit = this._items.append(item)
  def removeItem(idx: Int): Option[Item] =
    this._items.lift(idx) match
      case Some(item) => this._items.remove(idx); Some(item)
      case None => None

  def exits: Map[CompassDir, Room] = this._exits.toMap
  def addExit(dir: CompassDir, room: Room): Unit = this._exits += dir -> room

  // Traverse along a path, starting from this room, and returning the room
  // at the end.
  def traverse(path: Vector[CompassDir]): Option[Room] =
    path.headOption.map(this._exits.get) match
      case None => Some(this)
      case Some(None) => None
      case Some(Some(room)) => room.traverse(path.tail)
  def traverse(dir: CompassDir): Option[Room] = this.traverse(Vector(dir))

  // Builder helper for adding an exit to a room.
  // Modifies both rooms to point to each other, and returns the current room.
  //
  // If either room already points to another room in the direction connecting
  // the 2, the pointed-to rooms will be overwritten without being patched.
  // Example:
  //   A -(west)-> B, C -(east)-> D
  //   A <-(east)- B, C <-(west)- D
  // then A.withExit(West, C) will return
  //   A -(west)-> C, C -(east)-> A
  //   A <-(east)- B, C <-(west)- D
  // B and D are now **dangling**.
  def withExit(dir: CompassDir, room: Room): Room =
    this._exits.update(dir, room)
    room._exits.update(dir.opposite, this)
    this
  // Builder helper for adding an item to a room.
  // Modifies the current room to contain this item, and returns it.
  def withItem(item: Item): Room =
    this._items += item
    this

  override def toString: String =
    s"${this.name.capitalize};" +
      s" items: ${
        if this._items.isEmpty then "<none>"
        else this._items.map(_.name).mkString(", ").trim};" +
      s" exits: ${
        if this._exits.isEmpty then "<none>"
        else this._exits.keys.mkString(", ").trim.toLowerCase
      }"
end Room

object Room:
  // Return the built-in, hard-coded game map as the starting room.
  def map: Room =
    val hallway = Room("hallway")
      .withItem(Food("apple", 1, "A bright red apple", 25))
    val pyro = Room("pyrotechnics")
    val armory = Room("armory")
    val labs = Room("laboratories")
      .withItem(MotionTracker(2))
    val mess = Room("mess hall")
    val cc = Room("command center")
    val oq = Room("officers' quarters")
    val pod = Room("escape pod")
    val sus = Room("vents")
    val store = Room("storage")
      .withItem(Food("painkillers", 1, "some painkiller tablets", 25))
      .withItem(Food("loaf of bread", 2, "A supple load of bread", 50))
    val dh = Room("dining hall")
      .withItem(Food("leftover steak", 5, "leftover albeit juicy steak", 50))
    val quarters = Room("living quarters")
    Room("medical bay")
      .withExit(South, hallway
        .withExit(West, pyro)
        .withExit(East, armory
          .withExit(East, labs
            .withExit(East, mess
              .withExit(South, cc
                .withExit(South, oq
                  .withExit(South, pod))))))
        .withExit(South, dh
          .withExit(East, quarters)
          .withExit(South, store
            .withExit(South, sus
              .withExit(East, pod)))))
end Room