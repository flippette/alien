package o1.smile

import o1.gui.Color
import o1.gui.Align
import o1.world.Pos
import o1.smile.modeling.PositionType
import o1.smile.modeling.Angle

import java.awt.Image.*
import java.awt.image.AffineTransformOp.*

/** Settings Configuration for the SMILe Graphics Library.
  *
  * This object contains global settings affecting the behavior and appearance of graphical elements
  * created using the SMILe library. These settings can be modified to change default behaviors
  * across various operations and elements. */
object Settings:
  /** Enumeration of the different methods used for scaling of bitmaps. See [[java.awt.Image]] for details. */
  enum ScalingMethod(val value: Int) derives CanEqual:
    case Smooth          extends ScalingMethod(SCALE_SMOOTH)
    case Fast            extends ScalingMethod(SCALE_FAST)
    case AreaAveraging   extends ScalingMethod(SCALE_AREA_AVERAGING)
    case NearestNeighbor extends ScalingMethod(-1000)

  /** Enumeration of the different methods used for transforming bitmaps. See [[java.awt.image.AffineTransformOp]] for details. */
  enum TransformMethod(val value: Int) derives CanEqual:
    case Bilinear extends TransformMethod(TYPE_BILINEAR)
    case Nearest  extends TransformMethod(TYPE_NEAREST_NEIGHBOR)
    case Bicubic  extends TransformMethod(TYPE_BICUBIC)

  var DrawingIsAntiAliased: Boolean            = false
  var BufferScalingMethod: ScalingMethod       = ScalingMethod.Smooth
  var BufferTransformMethod: TransformMethod   = TransformMethod.Bilinear
  var DefaultGap: Double                       = 0
  var DefaultBackgroundColor: Color            = o1.gui.colors.Transparent
  var DefaultPositionType: PositionType        = PositionType.Center
  var DefaultHorizontalAlign: Align.Horizontal = Align.Left
  var DefaultVerticalAlign: Align.Vertical     = Align.VMiddle
  var DefaultPosition: Pos                     = Pos.Origin
  var DefaultRotationAngle: Angle              = Angle.Zero
  var DefaultRotationAngleInDegrees: Double    = DefaultRotationAngle.inDegrees

end Settings