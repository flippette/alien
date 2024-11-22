package o1.smile.pictures

import o1.gui.Color
import o1.gui.colors.Transparent
import o1.smile.modeling.Bounds
import o1.world.Pos

/** Represents a graphical point with a specific position and color.
  * @param pos    the point’s position
  * @param color  the point’s color */
sealed class Point(pos: Pos, val color: Color) extends VectorGraphic:

  override lazy val boundary: Bounds = Bounds(pos, pos)

  override def copy(newPosition: Pos): Point =
    new Point(newPosition, this.color)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Point =
    this

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double, relativityPoint: Pos): Point =
    copy(newPosition = this.position.scaleBy(horizontalFactor, verticalFactor, relativityPoint))

  override def rotateAround(pivot: Pos, degrees: Double): Point =
    copy(newPosition = this.position.rotateAround(pivot, degrees))

  override def rotateAroundOrigin(degrees: Double): Point =
    copy(newPosition = this.position.rotateAroundOrigin(degrees))

end Point


/** Represents a named reference point in a graphical context.
  * Such points can be used as pivots for rotations, for example.
  * @param pos   the position of the reference point
  * @param name  the name associated with the point */
class ReferencePoint(pos: Pos, val name: String) extends Point(pos, Transparent):

  /** Creates a copy of this reference point positioned at the specified coordinates.
    * @param newPosition  the new position for the copied reference point
    * @return a new instance of `ReferencePoint` at the specified position with the same name */
  override def copy(newPosition: Pos): ReferencePoint =
    ReferencePoint(newPosition, name)

end ReferencePoint
