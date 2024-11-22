package o1.smile.modeling

import o1.smile.infrastructure.DrawingSurface
import o1.smile.pictures.Text
import o1.world.Pos

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

/** Provides utility methods for calculating boundaries around picture elements and positions. */
object JVMBoundaryCalculator:

  /** Calculates the bounding box for a given text object.
    * @param text  The `Text` object for which to calculate the bounding box.
    * @return The `Bounds` representing the bounding box of the text. */
  def fromText(text: Text): Bounds =
    val frc = new FontRenderContext(new AffineTransform(), false, false)
    val font = DrawingSurface.fontToAWT(text.font, text.size)
    val lineMetrics = font.getLineMetrics(text.content, frc)
    val height      = lineMetrics.getHeight
    val width       = font.getStringBounds(text.content, frc).getWidth
    val upperLeft: Pos  = text.position.add(-width / 2.0, -height / 2.0)
    val lowerRight: Pos = text.position.add(width / 2.0, height / 2.0)
    Bounds(upperLeft, lowerRight)

end JVMBoundaryCalculator
