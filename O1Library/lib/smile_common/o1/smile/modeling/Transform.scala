package o1.smile.modeling


/** Represents an affine transformation defined by matrix coefficients for scaling, rotating, and
  * translating points in 2D space.
  * @param alpha  the X scaling component
  * @param gamma  the X to Y shearing component
  * @param tauX   the X translation component
  * @param delta  the Y to X shearing component
  * @param beta   the Y scaling component
  * @param tauY   the Y translation component */
class Transform(val alpha: Double, val gamma: Double, val tauX: Double,
                val delta: Double, val beta: Double, val tauY: Double)



/** Provides factory methods for creating common affine transformations, including flips and
  * rotations, relative to various axes and points. */
object Transform:
  private val Zero = 0.0
  private val One = 1.0

  /** The identity transformation, which makes no changes to the coordinates. */
  lazy val Identity: Transform =
    Transform(One, Zero, Zero, Zero, One, Zero)

  /** Creates an affine transformation for a horizontal flip about the Y-axis.
    * @param horizontalSize  The width of the image in pixels.
    * @return An affine transformation that flips the image horizontally. */
  def horizontalFlip(horizontalSize: Double): Transform =
    Transform(-One, Zero, -horizontalSize, Zero, One, Zero)

  /** Creates an affine transformation for a vertical flip about the X-axis.
    * @param verticalSize  The height of the image in pixels.
    * @return An affine transformation that flips the image vertically. */
  def verticalFlip(verticalSize: Double): Transform =
    Transform(One, Zero, Zero, Zero, -One, -verticalSize)

  /** Creates an affine transformation for a diagonal flip about the origin, flipping both
    * horizontally and vertically.
    * @param horizontalSize  The width of the image in pixels.
    * @param verticalSize  The height of the image in pixels.
    * @return An affine transformation that flips the image diagonally. */
  def diagonalFlip(horizontalSize: Double, verticalSize: Double): Transform =
    Transform(-One, Zero, -horizontalSize, Zero, -One, -verticalSize)

  /** Creates an affine transformation for rotation about a specified point.
    * @param angle  The angle of rotation in degrees.
    * @param x  The X-coordinate of the point around which to rotate.
    * @param y  The Y-coordinate of the point around which to rotate.
    * @return An affine transformation representing the rotation. */
  def rotationAround(x: Double, y: Double, angle: Double): Transform =
    import scala.math.*
    val cosine = cos(toRadians(angle))
    val sine   = sin(toRadians(angle))
    this.rotationAround(x, y, cosine, sine)

  /** Creates an affine transformation for rotation about a specified point using precomputed cosine
    * and sine values.
    * @param cos  The cosine of the rotation angle.
    * @param sin  The sine of the rotation angle.
    * @param x  The X-coordinate of the point around which to rotate.
    * @param y  The Y-coordinate of the point around which to rotate.
    * @return An affine transformation representing the rotation. */
  def rotationAround(x: Double, y: Double, cos: Double, sin: Double): Transform =
    Transform(cos, sin,
              x + sin * y - cos * x,
              -sin, cos,
              y - sin * x - cos * y)

end Transform

