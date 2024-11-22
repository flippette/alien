package o1.smile.pictures

import o1.world.Pos
import o1.gui.Stroke
import o1.gui.Fill

/** A factory object for creating lines. */
object Line:

  /** Creates a line between two specified points with a given line style.
    * @param startX  the x coordinate of the line’s start point
    * @param startY  the y coordinate of the line’s start point
    * @param endX    the x coordinate of the line’s end point
    * @param endY    the y coordinate of the line’s end point
    * @param stroke  the style with which to draw the line */
  def apply(startX: Double, startY: Double, endX: Double, endY: Double, stroke: Stroke): VectorGraphic =
    apply(Pos(startX, startY), Pos(endX, endY), stroke)

  /** Creates a line between two specified positions.
    * @param start   the line’s start position of the line.
    * @param end     the line’s end position of the line.
    * @param stroke  the style with which to draw the line */
  def apply(start: Pos, end: Pos, stroke: Stroke): VectorGraphic =
    val lineCenter = Pos((start.x + end.x) / 2.0, (start.y + end.y) / 2.0)
    val startRelativeToCenter = start - lineCenter
    val endRelativeToCenter   = end - lineCenter
    val twoPointsForPolygon = Seq(startRelativeToCenter, endRelativeToCenter)
    new Polygon(lineCenter, twoPointsForPolygon, Fill.None, stroke)

end Line