package o1.smile.pictures

import o1.smile.modeling.Angle
import o1.gui.{Fill, Stroke}
import o1.world.Pos

/** Factory object for creating circles.
  */
object Circle:

  /** Creates a circle represented as an `Arc` object. This is because a circle is a special case of
    * an arc that spans 360 degrees.
    *
    * @param center
    *   The center position of the circle.
    * @param radius
    *   The radius of the circle in pixels.
    * @param fill
    *   Optional fill style to apply to the circle. Determines the inside color.
    * @param line
    *   Optional line style to apply to the circle's perimeter. Defines the outline appearance.
    * @return
    *   A `VectorGraphic` instance representing the circle, specifically an `Arc` with full angular
    *   extent.
    */
  def apply(
             center: Pos,
             radius: Double,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    val diameter = radius * 2
    new Arc(
      center,
      diameter,
      diameter,
      startAngle = Angle.Zero.inDegrees,
      arcAngle = Angle.FullAngleInDegrees,
      fill = fill,
      line = line
    )
