package o1.smile.pictures

import o1.world.Pos
import o1.gui.*

/** A trait for vector graphics, which are picture elements defined in terms of points, lines, and
  * curves. Vector graphics can be filled and/or lined with specific styles. */
trait VectorGraphic extends DrawableElement:
  override def copy(newPosition: Pos): VectorGraphic
  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): VectorGraphic
  override def scaleBy(horizontalFactor: Double, verticalFactor: Double, relativityPoint: Pos): VectorGraphic
  override def rotateAround(pivot: Pos, degrees: Double): VectorGraphic
  override def rotateAroundOrigin(degrees: Double): VectorGraphic

  /** The style used to fill the vector graphic. If `None`, the graphic is not filled. */
  val fill: Fill = Fill.None

  /** The style used to line the outline of the vector graphic. If `None`, the graphic is not lined. */
  val line: Stroke = Line.None

end VectorGraphic
