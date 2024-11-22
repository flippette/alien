package o1.world

import o1.smile.infrastructure.*
import o1.smile.modeling.Bounds as SmileBounds
import o1.util.nice.number.*

/** A `Bounds` represents the dimensions of a rectangular area in two-dimensional space.
  *
  * `Bounds` objects are immutable. They assume a space in which x coordinates increase rightwards
  * and y coordinates downwards.
  *
  * This class has an alias in the top-level package [[o1]], so it’s accessible to students simply
  * via `import o1.*`.
  *
  * @param left    the x coordinate of the left edge of the `Bounds`
  * @param top     the y coordinate of the top edge of the `Bounds`
  * @param width   the width of the `Bounds` (the distance from its left edge to its right edge)
  * @param height  the height of the `Bounds` (the distance from its top edge to its bottom edge) */
final case class Bounds(val left: Double, val top: Double, val width: Double, val height: Double) derives CanEqual:

  /** the coordinates of the top left-hand corner of the `Bounds` */
  lazy val pos: Pos = Pos(left, top)
  /** the x coordinate of the right edge of the `Bounds` */
  lazy val right: Double = left + width
  /** the y coordinate of the bottom edge of the `Bounds` */
  lazy val bottom: Double = top + height

  /** Determines if the given pair of coordinates is within the bounds. For that to be the case, the
    * given `x` needs to be between the left edge (inclusive) and the right edge (exclusive) and
    * the given `y` needs to be between the top edge (inclusive) and the bottom edge (exclusive). */
  def contains(x: Double, y: Double): Boolean = x.isBetween(left, right) && y.isBetween(top, bottom)

  /** Determines if the given [[Pos]] is within the bounds. For that to be the case, the `Pos`
    * object’s `x` needs to be between the left edge (inclusive) and the right edge (exclusive)
    * and its `y` needs to be between the top edge (inclusive) and the bottom edge (exclusive). */
  def contains(pos: Pos): Boolean = this.contains(pos.x, pos.y)

  /** Returns a textual description of the bounds. */
  override def toString = s"($left,$top):w=$width,h=$height"

  private[o1] def toSmileBounds: SmileBounds =
    SmileBounds(upperLeftCorner = Pos(this.left, this.top), lowerRightCorner = Pos(this.right, this.bottom))

end Bounds


/** A companion object for [[Bounds class `Bounds`]]. Provides a factory method. */
object Bounds:

  /** Creates a [[Bounds]] that has its top left-hand corner is at the given [[Pos]]. */
  def apply(topLeft: Pos, width: Double, height: Double) =
    new Bounds(topLeft.x, topLeft.y, width, height)

  /** Creates a [[Bounds]] that has its corners at the given [[Pos]]es. */
  def apply(topLeft: Pos, bottomRight: Pos) =
    new Bounds(topLeft.x, topLeft.y, bottomRight.x - topLeft.x, bottomRight.y - topLeft.y)

end Bounds

