package o1.smile.pictures


import o1.smile.modeling.*
import o1.gui.{Fill, Stroke}
import o1.world.Pos

/** Represents a polygon defined by a sequence of points, with optional fill and line styles.
  * @param pos         The position of the polygon. This usually represents the centroid or a specific anchor point.
  * @param points      The sequence of points defining the vertices of the polygon.
  * @param fill   Optional fill style for the interior of the polygon.
  * @param line Optional line style for the outline of the polygon. */
class Polygon(center: Pos, val points: Seq[Pos], override val fill: Fill, override val line: Stroke) extends VectorGraphic:

  override def copy(newPosition: Pos): Polygon =
    new Polygon(newPosition, points, fill, line)

  private def internalCopy(newCenter: Pos = position, newPoints: Seq[Pos] = points, newFill: Fill = fill,
                           newStroke: Stroke = line): Polygon =
    Polygon(newCenter, newPoints, newFill, newStroke)

  override lazy val position: Pos = center

  private val contentBoundary: Bounds =
    BoundaryCalculator.fromPositions(points)

  lazy val boundingBoxCorners: Seq[Pos] =
    val radius = this.line.radius
    val ulX = contentBoundary.upperLeftCorner.x  - radius
    val ulY = contentBoundary.upperLeftCorner.y  - radius
    val lrX = contentBoundary.lowerRightCorner.x + radius
    val lrY = contentBoundary.lowerRightCorner.y + radius
    Seq(position + contentBoundary.upperLeftCorner.subtract(radius, radius),
        position.add(lrX, ulY),
        position + contentBoundary.lowerRightCorner.add(radius, radius),
        position.add(ulX, lrY))

  lazy val upperLeftCorner: Pos  = boundingBoxCorners.head
  lazy val lowerRightCorner: Pos = boundingBoxCorners.tail.tail.head

  override lazy val boundary: Bounds =
    Bounds(upperLeftCorner, lowerRightCorner)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Polygon =
    internalCopy(newPoints = points.map( _.scaleByRelativeToOrigin(horizontalFactor, verticalFactor) ))

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double, relativityPoint: Pos): Polygon =
    internalCopy(newCenter = scale(position, horizontalFactor, verticalFactor, relativityPoint),
                 newPoints = points.map( _.scaleByRelativeToOrigin(horizontalFactor, verticalFactor) ))

  override def rotateAround(pivot: Pos, degrees: Double): Polygon =
    internalCopy(newCenter = rotate(position, degrees, pivot),
                 newPoints = points.map( _.rotateAround(pivot, degrees)))

  override def rotateAroundOrigin(degrees: Double): Polygon =
    internalCopy(newCenter = rotate(position, degrees),
                 newPoints = points.map( _.rotateAroundOrigin(degrees) ))

end Polygon