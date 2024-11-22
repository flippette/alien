package o1.smile.pictures

import o1.gui.{Fill, Stroke}
import o1.smile.modeling.{BoundaryCalculator, Bounds, scale}
import o1.world.Pos

/** Represents a single line of text as a graphical element.
  * @param pos           The position of the text.
  * @param customBounds  Optional custom bounds for the text.
  * @param content       The content of the text.
  * @param font          The font or typeface used for rendering the text.
  * @param fill          Optional fill style for the text.
  * @param line          Optional line style for the text. */
class Text(center: Pos, val customBounds: Option[Bounds], val content: String, val font: String, val size: Double,
           val weight: Int, override val fill: Fill, override val line: Stroke) extends VectorGraphic:

  override def copy(newPosition: Pos): Text = new Text(newPosition, customBounds, content, font, size, weight, fill, line)

  private def internalCopy(newCenter: Pos = position, newCustomBounds: Option[Bounds] = customBounds, newContent: String = content, newFont: String = font,
                            newSize: Double = size, newWeight: Int = weight, newFill: Fill = fill, newStroke: Stroke = line): Text =
    new Text(newCenter, newCustomBounds, newContent, newFont, newSize, newWeight, newFill, newStroke)

  override lazy val boundary: Bounds = customBounds.getOrElse(BoundaryCalculator.fromText(this))

  override lazy val position: Pos = center

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Text =
    val newBounds = scale(Seq(boundary.upperLeftCorner, boundary.lowerRightCorner), horizontalFactor, verticalFactor)
    internalCopy(newCustomBounds = Some(Bounds(newBounds.head, newBounds.last)))

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double, relativityPoint: Pos): Text =
    val newBounds = scale(Seq(boundary.upperLeftCorner, boundary.lowerRightCorner), horizontalFactor, verticalFactor, relativityPoint)
    internalCopy(newCustomBounds = Some(Bounds(newBounds.head, newBounds.last)))

  override def rotateAround(pivot: Pos, degrees: Double): Text = ??? // LATER: implement properly, currently fixed with a hack elsewhere

  override def rotateAroundOrigin(degrees: Double): Text = ??? // LATER: implement properly, currently fixed with a hack elsewhere

end Text

