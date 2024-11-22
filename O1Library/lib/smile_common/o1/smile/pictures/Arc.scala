package o1.smile.pictures

import o1.smile.Settings.*
import o1.smile.modeling.*
import o1.gui.{Fill, Stroke}
import o1.world.Pos

/** Represents an arc, a part of a circle or ellipse, defined by its center position, width, height,
  * start angle, arc angle, and optional fill and line styles.
  * @param center            The center point of the arc.
  * @param width          The width of the arc in pixels.
  * @param height         The height of the arc in pixels.
  * @param startAngle     The start angle of the arc in degrees.
  * @param arcAngle       The angular extent of the arc in degrees.
  * @param rotationAngle  The rotation angle of the arc in degrees, affecting its orientation.
  * @param fill           Optional fill style for the arc.
  * @param line           Optional line style defining the arc's outline appearance. */
class Arc(center: Pos = DefaultPosition, val width: Double, val height: Double, val startAngle: Double = Angle.Zero.inDegrees,
           val arcAngle: Double = Angle.FullAngleInDegrees, val rotationAngle: Double = DefaultRotationAngleInDegrees,
           override val fill: Fill, override val line: Stroke) extends VectorGraphic:

  override lazy val position: Pos = center

  private lazy val corners: Seq[Pos] =
    // LATER: lineRadius
    val halfWidth  = width / 2.0
    val halfHeight = height / 2.0
    Seq(rotate(position.add(-halfWidth, -halfHeight), rotationAngle),
        rotate(position.add( halfWidth, -halfHeight), rotationAngle),
        rotate(position.add( halfWidth,  halfHeight), rotationAngle),
        rotate(position.add(-halfWidth,  halfHeight), rotationAngle))

  /** Checks if the arc represents a complete cycle (360 degrees) or more.
    */
  lazy val isFullCycle: Boolean = arcAngle.abs >= Angle.FullAngleInDegrees

  /** Determines if the arc represents a perfect circle, based on its width and height.
    */
  lazy val isCircle: Boolean = isFullCycle && (width == height)

  /** Transformed upper left corner of this [[Arc]]. */
  lazy val upperLeftCorner: Pos = corners.head

  /** Transformed upper right corner of this [[Arc]]. */
  lazy val upperRightCorner: Pos = corners.tail.head

  /** Transformed lower right corner of this [[Arc]]. */
  lazy val lowerRightCorner: Pos = corners.tail.tail.head

  /** Transformed lower left corner of this [[Arc]]. */
  lazy val lowerLeftCorner: Pos = corners.tail.tail.tail.head

  /** Transformed boundary of this [[Arc]]. */
  override lazy val boundary: Bounds = Bounds(upperLeftCorner, lowerRightCorner)

  override def copy(newPosition: Pos): Arc =
    new Arc(newPosition, width, height, startAngle, arcAngle, rotationAngle, fill, line)

  private def internalCopy(newPosition: Pos = position, newWidth: Double = width, newHeight: Double = height,
                           newStartAngle: Double = startAngle, newArcAngle: Double = arcAngle, newRotationAngle: Double = rotationAngle,
                           newFill: Fill = fill, newStroke: Stroke = line): Arc =
    val limitedArcAngle = newArcAngle.min(Angle.FullAngleInDegrees).max(-Angle.FullAngleInDegrees)
    new Arc(newPosition, newWidth, newHeight, newStartAngle, limitedArcAngle, newRotationAngle, newFill, newStroke)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Arc =
    internalCopy(newWidth = horizontalFactor * width, newHeight = verticalFactor * height)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double, relativityPoint: Pos): Arc =
    val scaledPosition = scale(position, horizontalFactor, verticalFactor, relativityPoint)
    internalCopy(newPosition = scaledPosition, newWidth = horizontalFactor * width, newHeight = verticalFactor * height)

  private def decideNewRotationAngleFor(newRotationAngle: Double): Double =
    if isCircle then Angle.Zero.inDegrees else rotationAngle + newRotationAngle
    // If this arc represents a circle, rotating it should not have any effect on
    // its appearance, and thus the rotation angle can (and must) be zero all the
    // time. (The position can, of course, change if the rotation is not performed
    // around the arc's center point, but that is irrelevant here.)

  override def rotateAround(pivot: Pos, degrees: Double): Arc =
    internalCopy(newPosition = rotate(position, degrees, pivot), newRotationAngle = decideNewRotationAngleFor(degrees))

  override def rotateAroundOrigin(degrees: Double): Arc =
    internalCopy(newPosition = rotate(position, degrees), newRotationAngle = decideNewRotationAngleFor(degrees))

end Arc