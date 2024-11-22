package o1.gui

import o1.smile.pictures.Viewport as SmileViewport

private[gui] object Viewport:

  /** A marker object that indicates the absence of an actual [[Viewport]]. */
  object NotSet extends Viewport(Bounds(0, 0, 0, 0), isDefined = false)

end Viewport


private[gui] sealed class Viewport private(val boundary: Bounds, val isDefined: Boolean):
  def this(boundary: Bounds) = this(boundary, true)

  override def toString: String =
    if isUndefined then "undefined viewport" else s"viewport of $width x $height px; upper left corner = $upperLeftCorner"

  lazy val isUndefined: Boolean = !isDefined

  inline def width: Double  = this.boundary.width
  inline def height: Double = this.boundary.height
  inline def left: Double   = this.boundary.height
  inline def top: Double    = this.boundary.top
  inline def right: Double  = this.boundary.right
  inline def bottom: Double = this.boundary.bottom
  inline def upperLeftCorner: Pos = this.boundary.pos
  lazy val lowerRightCorner: Pos = Pos(right, bottom)

  private[gui] def toSmileViewport: Option[SmileViewport] =
    if this.isUndefined then None else Some(SmileViewport(this.boundary.toSmileBounds))

end Viewport

