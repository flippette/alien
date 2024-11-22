package o1.gui

import o1.gui.colors.Transparent
import java.awt
import awt.BasicStroke as SwingStroke
import Line.*


/** A `Stroke` is a style for drawing shape outlines and other lines. A `Stroke` object defines,
  * among other things, the color and width of lines drawn using the stroke.
  * @param fill       a fill style for lines drawn using this stroke; this can be a [[Color]]
  *                   or a gradient, for example
  * @param thickness  the width of lines drawn using this stroke; defaults to 1
  * @param cap        the style of the cap (end point) for lines drawn using this stroke;
  *                   the default is a square cap.
  * @param join       the style in which lines drawn using this stroke join up with each other;
  *                   the default is a miter join
  * @param onTop      a value that affects filled shapes that have an outline drawn using this
  *                   stroke: if `true`, the outline will be drawn on top of the filled shape
  *                   and fully visible; if `false`, the outline will be below the fill and
  *                   partially invisible. Defaults to `true`. */
sealed class Stroke(val fill: Fill, val thickness: Double = 1, val cap: Cap = Cap.Square,
                    val join: Join = Join.Miter, val onTop: Boolean = true):

  private[o1] lazy val radius = this.thickness / 2

  private[o1] lazy val needsDrawing = !this.fill.isTransparent

  private[o1] def toSwing: awt.Stroke = Line.toSwing(this.thickness, this.cap, this.join)

end Stroke


/** The `Line` singleton provides tools for specifying what a line-drawing
  * style — a [[Stroke]] — is like. */
object Line:

  /** A [[Stroke]] that is not drawn at all. Used as the outline for shapes
    * that do not have a visible outline. */
  object None extends Stroke(Transparent, thickness = 0)

  private[gui] opaque type SwingCap = Int
  private[gui] opaque type SwingJoin = Int

  private[gui] def toSwing(width: Double, cap: Cap, join: Join): awt.Stroke =
    SwingStroke(width.toFloat, cap.toSwing, join.toSwing)

  /** An enumeration of different styles of line caps. Each such style specifies what
    * the end points of lines look like.
    *
    * These styles correspond directly to the line-cap styles illustrated in
    * [[https://docs.oracle.com/javase/tutorial/2d/geometry/strokeandfill.html Oracle’s Java graphics tutorial]].
    * See that page for pictures. */
  enum Cap(private val swingCap: SwingCap) derives CanEqual:

    private[o1] def toSwing = this.swingCap

    /** a line-cap style with flat line endings that are cut off right at the end points */
    case Butt extends Cap(SwingStroke.CAP_BUTT)

    /** a line-cap style with rounded line endings around the end points */
    case Round extends Cap(SwingStroke.CAP_ROUND)

    /** a line-cap style with square line endings that extend slightly beyond the end points */
    case Square extends Cap(SwingStroke.CAP_SQUARE)

  end Cap

  /** An enumeration of different styles of line caps. Each such style specifies how
    * the connections between lines look like.
    *
    * These styles correspond directly to the join styles illustrated in
    * [[https://docs.oracle.com/javase/tutorial/2d/geometry/strokeandfill.html Oracle’s Java graphics tutorial]].
    * See that page for pictures. */
  enum Join(private val swingJoin: SwingJoin) derives CanEqual:

    def toSwing = this.swingJoin

    /** a line-join style where the outer edges of connecting line segments meet at a sharp point */
    case Miter extends Join(SwingStroke.JOIN_MITER)

    /** a line-join style connection between the line segments is rounded */
    case Round extends Join(SwingStroke.JOIN_ROUND)

    /** a line-join style where the join is cut flat */
    case Bevel extends Join(SwingStroke.JOIN_BEVEL)

  end Join

end Line

