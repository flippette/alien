package o1.smile.pictures

import o1.smile.modeling.Angle
import o1.gui.{Fill, Stroke}
import o1.world.Pos

/** Factory object for creating ellipses. */
object Ellipse:

  /** Creates an ellipse represented as an [[Arc]] object. An ellipse is specified by its center, the
    * lengths of its semi-major and semi-minor axes, and optional styles for filling and stroking.
    * @param center         The center position of the ellipse.
    * @param semiMajorAxis  The length of the semi-major axis in pixels.
    * @param semiMinorAxis  The length of the semi-minor axis in pixels.
    * @param fill           Optional fill style to apply to the ellipse. Determines the inside color.
    * @param line           Optional line style to apply to the ellipse's perimeter. Defines the outline appearance.
    * @return A `VectorGraphic` instance representing the ellipse, specifically an [[Arc]] with a full angular extent. */
  def apply(center: Pos, semiMajorAxis: Double, semiMinorAxis: Double, fill: Fill, line: Stroke): VectorGraphic =
    Arc(center, semiMajorAxis * 2, semiMinorAxis * 2, startAngle = Angle.Zero.inDegrees,
      arcAngle = Angle.FullAngleInDegrees, fill = fill, line = line)

end Ellipse
