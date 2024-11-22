package o1.gui

import o1.smile.pictures.{Bitmap, Arc as SmileArc, Circle as SmileCircle, Ellipse as SmileEllipse,
  Line as SmileLine, Polygon as SmilePolygon, Rectangle as SmileRectangle, Star5 as SmileStar5,
  Text as SmileText, Triangle as SmileTriangle}

import o1.util.nice.number.*
import o1.util.proper.*

import Anchor.*
import Pos.Origin

// N.B. This trait has defaults so doesn’t currently work in exports, but it would be nice to convert it into an exported API.

/** This trait provides a variety of shape-manipulating methods. It is used internally by O1Library.
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand the purpose of this trait.**
  * Most of these methods are available via `import o1.*`, and all of them via `import o1.gui.Pic.*`. */
private[gui] trait ShapeAPI:

  private val StarCuspRadiusFactor: Double = 0.201

  private def op(methodName: ProperString, shapeName: String) =
    PicHistory(PicHistory.op.Create(method = methodName, simpleDescription = shapeName + "-shape"))
  private def op(methodName: ProperString): PicHistory =
    op(methodName, methodName)


  /** Creates a new [[Pic]] that portrays a rectangle.
    * @param width    the width of the rectangle and the [[Pic]]
    * @param height   the height of the rectangle and the [[Pic]]
    * @param fill     the rectangle’s color or some other fill style (such as a gradient)
    * @param outline  the style of the rectangle’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the rectangle (a vector graphic) */
  def rectangle(width: Double, height: Double, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val smileContent = SmileRectangle(width atLeast 0, height atLeast 0, fill, outline)
    Pic(smileContent, anchor, op("rectangle".p))


  /** Creates a new [[Pic]] that portrays a filled, single-color rectangle and sets its
    * [[o1.world.objects.Anchor Anchor]] at [[o1.world.objects.Anchor.TopLeft TopLeft]].
    * @param width   the width of the rectangle and the [[Pic]]
    * @param height  the height of the rectangle and the [[Pic]]
    * @param color   the color of the rectangle and thus the only color visible in the [[Pic]];
    *                if unspecified, defaults to `White`
    * @return a [[Pic]] of the rectangle (a vector graphic) */
  def emptyCanvas(width: Double, height: Double, color: Color = colors.White): Pic =
    val rectangle = SmileRectangle(width atLeast 0, height atLeast 0, color, Line.None)
    val smileContent = rectangle.moveUpperLeftCornerTo(Origin).toPicture.withContentBoundaryAsViewport
    Pic(smileContent, TopLeft, op("emptyCanvas".p, "rectangle".p))


  /** Creates a new [[Pic]] that portrays a square.
    * @param side     the width and height of the square and the [[Pic]]
    * @param fill     the square’s color or some other fill style (such as a gradient)
    * @param outline  the style of the square’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the square (a vector graphic) */
  def square(side: Double, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val smileContent = SmileRectangle(side atLeast 0, fill, outline)
    Pic(smileContent, anchor, op("square".p))


  /** Creates a new [[Pic]] that portrays a filled polygon. The picture is just large
    * enough to contain the polygon; its background is fully transparent.
    * @param points   the polygon’s corners (only their relative positioning matters)
    * @param fill     the polygon’s color or some other fill style (such as a gradient)
    * @param outline  the style of the polygon’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the polygon (a vector graphic) */
  def polygon(points: Seq[Pos], color: Color, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val smileContent = SmilePolygon(center = Origin, points, fill, outline)
    Pic(smileContent, anchor, op("polygon".p))


  /** Creates a new [[Pic]] that portrays a circle. The background is fully transparent.
    * @param diameter  the diameter of the circle, which also sets the width and height of the [[Pic]]
    * @param fill      the circle’s color or some other fill style (such as a gradient)
    * @param outline   the style of the circle’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor    an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the circle (a vector graphic) */
  def circle(diameter: Double, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val radius = (diameter / 2.0) atLeast 0
    val smileContent = SmileCircle(center = Origin, radius, fill, outline)
    Pic(smileContent, anchor, op("circle".p))

  /** Creates a new [[Pic]] that portrays an ellipse. The background is fully transparent.
    * @param width    the width of the ellipse and the [[Pic]]
    * @param height   the height of the ellipse and the [[Pic]]
    * @param fill     the ellipse’s color or some other fill style (such as a gradient)
    * @param outline  the style of the ellipse’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the ellipse (a vector graphic) */
  def ellipse(width: Double, height: Double, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val xAxis = (width / 2.0) atLeast 0
    val yAxis = (height / 2.0) atLeast 0
    val smileContent = SmileEllipse(center = Origin, xAxis, yAxis, fill, outline)
    Pic(smileContent, anchor, op("ellipse".p))


  /** Creates a new [[Pic]] that portrays an isosceles triangle. The triangle’s base is at the
    * bottom of the image, and its apex is at the top center. The background is fully transparent.
    * @param width    the width of the triangle’s base, which determines the width of the [[Pic]], too
    * @param height   the height of the triangle, which determines the height of the [[Pic]], too
    * @param fill     the triangle’s color or some other fill style (such as a gradient)
    * @param outline  the style of the triangle’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the triangle (a vector graphic) */
  def isoscelesTriangle(width: Double, height: Double, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val smileContent = SmileTriangle.basedOnHeightAndBase(height atLeast 0, width atLeast 0, fill, outline)
    Pic(smileContent, anchor, op("isoscelesTriangle".p, "triangle".p))

  /** Creates a new [[Pic]] that portrays an equilateral triangle. The triangle’s base is at the
    * bottom of the image, and its apex is at the top center. The background is fully transparent.
    * @param side     the length of the triangle’s each side, which determines the width of the [[Pic]], too
    * @param fill     the triangle’s color or some other fill style (such as a gradient)
    * @param outline  the style of the triangle’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the triangle (a vector graphic) */
  def equilateralTriangle(side: Double, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val smileContent = SmileTriangle(side atLeast 0, fill, outline)
    Pic(smileContent, anchor, op("equilateralTriangle".p, "triangle".p))


  /** Creates a new [[Pic]] that portrays a five-pointed star. The background is fully transparent.
    * @param width    the width of the star, which determines the [[Pic]]’s dimensions
    * @param fill     the star’s color or some other fill style (such as a gradient)
    * @param outline  the style of the star’s outline; if unspecified, defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the star (a vector graphic) */
  def star(width: Double, fill: Fill, outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val cuspRadius = (StarCuspRadiusFactor * width) atLeast 0
    val xSize = width atLeast 0
    val ySize = xSize
    val smileContent = SmileStar5(xSize, ySize, cuspRadius, Origin, fill, outline)
    Pic(smileContent, anchor, op("star".p))


  /** Creates a new [[Pic]] that portrays a line. The line is specified in terms
    * of two [[o1.world.Pos Pos]] objects: imagine drawing a line between the two points
    * on a plane and then cropping the plane to just the part that contains the line.
    *
    * The line always runs from one corner of the resulting [[Pic]] to another. The
    * [[o1.world.objects.Anchor Anchor]] of the [[Pic]] is at one of the four corners:
    * the one that’s closest to `from`.
    *
    * The background is fully transparent.
    *
    * @param from   the line’s “starting point”; the [[Pic]] will anchor at the corresponding corner
    * @param to     the line’s “end point”
    * @param style  the stroke style of the line (color, width, etc.)
    * @return a [[Pic]] of the line (a vector graphic) */
  def line(from: Pos, to: Pos, style: Stroke): Pic =
    val anchor = (from.x < to.x, from.y < to.y) match
      case (true, true)   => TopLeft
      case (true, false)  => BottomLeft
      case (false, true)  => TopRight
      case (false, false) => BottomRight
    val smileContent = SmileLine(from, to, style)
    Pic(smileContent, anchor, op("line".p))


  /** Creates a new [[Pic]] that portrays a line. The line is specified in terms
    * of two [[o1.world.Pos Pos]] objects: imagine drawing a line between the two points
    * on a plane and then cropping the plane to just the part that contains the line.
    *
    * This version of `line` specifies the line style as a color and a width.
    * The line ends are round. Calling `line(pos1, pos2, Blue, 3)` is equivalent
    * to calling `line(pos1, pos2, Stroke(Blue, 3, Line.Cap.Round))`.
    *
    * @param from       the line’s “starting point”; the [[Pic]] will anchor at the corresponding corner
    * @param to         the line’s “end point”
    * @param color      the color of the line
    * @param thickness  the line’s width in pixels; defaults to `1.0``
    * @return a [[Pic]] of the line (a vector graphic) */
  def line(from: Pos, to: Pos, color: Color, thickness: Double = 1.0): Pic =
    line(from, to, Stroke(color, thickness, Line.Cap.Round))


  /** Creates a new [[Pic]] that portrays an arcing line. The line is specified in
    * terms of an ellipse: it has a starting point and a length along an (otherwise
    * invisible) elliptical curve. The background is fully transparent. This
    * version of `arc` specifies the line style in terms of color and thickness.
    * The anchor of the image is at the center of the defining ellipse.
    * @param width          the width of the ellipse that defines the arc;
    *                       this also becomes the width of the resulting image
    * @param height         the height of the ellipse that defines the arc;
    *                       this also becomes the height of the resulting image
    * @param startDegrees   the position along the ellipse where the arc starts
    *                       (e.g., 0 degrees means at the rightmost point,
    *                       90 degrees at the top, and 270 degrees at the bottom)
    * @param lengthDegrees  the length of the arc (e.g., 10 degrees means a small
    *                       part of the ellipse and 180 degrees means half an ellipse)
    * @param color          the color of the arc
    * @param thickness      the width of the arc
    * @return a [[Pic]] of the arc (a vector graphic) */
  def arc(width: Double, height: Double, startDegrees: Double, lengthDegrees: Double,
          color: Color, thickness: Double): Pic =
    val stroke = Stroke(color, thickness)
    arc(width, height, startDegrees, lengthDegrees, stroke)

  /** Creates a new [[Pic]] that portrays an arcing line. The line is specified in
    * terms of an ellipse: it has a starting point and a length along an (otherwise
    * invisible) elliptical curve. The background is fully transparent.
    * @param width          the width of the ellipse that defines the arc;
    *                       this also becomes the width of the resulting image
    * @param height         the height of the ellipse that defines the arc;
    *                       this also becomes the height of the resulting image
    * @param startDegrees   the position along the ellipse where the arc starts
    *                       (e.g., 0 degrees means at the rightmost point,
    *                       90 degrees at the top, and 270 degrees at the bottom)
    * @param lengthDegrees  the length of the arc (e.g., 10 degrees means a small
    *                       part of the ellipse and 180 degrees means half an ellipse)
    * @param style          the stroke style of the line (color, width, etc.)
    * @param fill           an optional fill style for the ellipse segment;
    *                       if unspecified, there is no fill, just the arc
    * @param anchor         an anchor for the new [[Pic]]; if unspecified, defaults
    *                       to [[Center]], i.e., the center of the ellipse
    * @return a [[Pic]] of the arc (a vector graphic) */
  def arc(width: Double, height: Double, startDegrees: Double, lengthDegrees: Double,
          style: Stroke = Line.None, fill: Fill = Fill.None, anchor: Anchor = Center): Pic =
    val smileContent = SmileArc(center = Origin, width, height, startDegrees,
                                lengthDegrees, rotationAngle = 0, fill, style)
    Pic(smileContent, anchor, op("arc".p))


  /** Creates a new [[Pic]] that contains the given text. The picture is just large
    * enough to contain the text; its background is fully transparent.
    * @param content  the text to display in the picture
    * @param font     the name of a font family or specific font (e.g., "Serif", "SansSerif",
    *                 "Times New Roman" or "Times New Roman Italic"); the font must be
    *                 installed on the computer (the tools in [[o1.gui.font]] can check
    *                 what is installed) or a default font will be used instead
    * @param size     the font size
    * @param fill     the text’s color or some other fill style (such as a gradient);
    *                 if unspecified, defaults to [[colors.Black]]
    * @param outline  a styling for character outlines; if unspecified,
    *                 defaults to no outline ([[Line.None]])
    * @param anchor   an anchor for the new [[Pic]]; if unspecified, defaults to [[Center]]
    * @return a [[Pic]] of the text (a vector graphic) */
  def text(content: String, font: String, size: Double, fill: Fill = colors.Black,
           outline: Stroke = Line.None, anchor: Anchor = Center): Pic =
    val smileContent = SmileText(center = Origin, customBounds = None, content, font, size, weight = 0, fill, outline)
    import PicHistory.op.Create
    val history = PicHistory(Create(method = "text".p, simpleDescription = s"""the text "$content" as an image"""))
    Pic(smileContent, anchor, history)

end ShapeAPI

