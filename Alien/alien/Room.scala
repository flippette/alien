package alien

import o1.*

import scala.collection.mutable

class Room(val name: String,
           description: String,
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
  private def withExit(dir: CompassDir, room: Room): Room =
    this._exits.update(dir, room)
    room._exits.update(dir.opposite, this)
    this
  // Builder helper for adding an item to a room.
  // Modifies the current room to contain this item, and returns it.
  private def withItem(item: Item): Room =
    this._items += item
    this

  override def toString: String =
    s"${this.description}\n" +
      s"You find around you: ${
        if this._items.isEmpty then "no items"
        else this._items.map(_.name).mkString(", ").trim}.\n" +
      s"You see ${
        if this._exits.isEmpty then "no exits."
        else s"exits to the ${this._exits.iterator.map((dir, room) => s"$dir (${room.name})").mkString(", ").trim.toLowerCase}."
      }"
end Room

object Room:
  // Return the built-in, hard-coded game map as the starting room.
  def map: Room =
    val hallway = Room("hallway", "A dimly lit hallway. It is rather eerie.")
      .withItem(Food("apple", "A bright red apple", 25))
    val pyro = Room("pyrotechnics", "Pyro labs, you see broken chemical equipment.")
    val armory = Room("armory", "Armory. There are no guns to be found, all have been used, in vain.")
    val labs = Room("laboratories", "Laboratories. High-tech equipment is developed here. It is all broken now though.")
      .withItem(Cctv(4))
    val mess = Room("mess hall", "Mess hall. This is where you would hang out with your crew mates, if they were alive.")
    val cc = Room("command center", "Command center. Where the ship is piloted from. Nowhere to fly to but safety now.")
    val oq = Room("officers' quarters", "Officers' quarters. You find some mangled sergeants. Disgusted as you are, you try not to make a sound.")
    val pod = Room("escape pod", "Safety, as you collect a well-earned sigh of relief.")
    val sus = Room("vents", "Ventilation air ducts, they lead to the escape pod.")
    val store = Room("storage", "Storage area, where longer shelf-life items are kept.")
      .withItem(Food("painkillers", "Some painkiller tablets", 25))
      .withItem(Food("loaf of bread", "A supple loaf of bread", 50))
    val dh = Room("dining hall", "Dining hall, where delicious food is kept. Maybe there is food stored in the freezer.")
      .withItem(Food("leftover steak", "Leftover albeit juicy steak", 50))
    val quarters = Room("living quarters", "Living quarters. Only you are not among the dead piled up here. Yet.")
    Room("medical bay", "The medical bay, where cryogenic sleep chambers are kept.")
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