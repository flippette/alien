package o1.smile.pictures

import o1.smile.modeling.Angle
import o1.gui.{Fill, Stroke}
import o1.world.Pos

/** Factory object for creating triangles.
  *
  * Provides methods to create equilateral, isosceles, and scalene triangles based on side lengths,
  * height, base length, or specific corner positions.
  */
object Triangle:

  /** Height of an equilateral triangle as a factor of the length of its side. */
  val HeightOfEquilateralTriangleAsFactorOfSide: Double = math.sqrt(3) / 2.0

  /** Side length of an equilateral triangle as a factor of its halfHeight. */
  val SideOfEquilateralTriangleAsFactorOfHeight: Double =
    1.0 / HeightOfEquilateralTriangleAsFactorOfSide

  /** Creates an equilateral triangle with a specified side length.
    *
    * @param sideLength
    *   Length of each side of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing an equilateral triangle.
    * @throws IllegalArgumentException
    *   if the side length is negative.
    */
  def apply(
             sideLength: Double,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    apply(sideLength, Pos.Origin, fill, line)

  /** Creates an equilateral triangle with a specified side length.
    *
    * @param sideLength
    *   Length of each side of the triangle.
    * @param center
    *   Center of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing an equilateral triangle.
    * @throws IllegalArgumentException
    *   if the side length is negative.
    */
  def apply(
             sideLength: Double,
             center: Pos,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    validateSide(sideLength, "side")

    val halfHeight: Double =
      HeightOfEquilateralTriangleAsFactorOfSide * sideLength / 2.0

    val halfBase: Double = sideLength / 2.0

    createIsosceles(halfHeight, halfBase, center, fill, line)

  /** Creates an equilateral triangle based on the specified height.
    *
    * @param height
    *   Height of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing an equilateral triangle.
    * @throws IllegalArgumentException
    *   if the height is negative.
    */
  def basedOnHeight(
                     height: Double,
                     fill: Fill,
                     line: Stroke
  ): VectorGraphic =
    basedOnHeight(height, Pos.Origin, fill, line)

  /** Creates an equilateral triangle based on the specified height.
    *
    * @param height
    *   Height of the triangle.
    * @param center
    *   Center of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing an equilateral triangle.
    * @throws IllegalArgumentException
    *   if the height is negative.
    */
  def basedOnHeight(
                     height: Double,
                     center: Pos,
                     fill: Fill,
                     line: Stroke
  ): VectorGraphic =

    if height < 0 then
      throw new IllegalArgumentException(s"Height of triangle cannot be negative (was: $height).")

    val halfSide: Double =
      SideOfEquilateralTriangleAsFactorOfHeight * height / 2.0

    val halfHeight: Double = height / 2.0

    createIsosceles(halfHeight, halfSide, center, fill, line)

  /** Creates an isosceles triangle with specified side lengths and base length.
    *
    * @param sideLength
    *   Length of the two equal sides of the triangle.
    * @param baseLength
    *   Length of the base of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing an isosceles triangle.
    * @throws IllegalArgumentException
    *   if any of the lengths are negative or if the triangle inequality does not hold.
    */
  def apply(
             sideLength: Double,
             baseLength: Double,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    apply(sideLength, baseLength, Pos.Origin, fill, line)

  /** Creates an isosceles triangle with specified side lengths and base length.
    *
    * @param sideLength
    *   Length of the two equal sides of the triangle.
    * @param baseLength
    *   Length of the base of the triangle.
    * @param center
    *   Center of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing an isosceles triangle.
    * @throws IllegalArgumentException
    *   if any of the lengths are negative or if the triangle inequality does not hold.
    */
  def apply(
             sideLength: Double,
             baseLength: Double,
             center: Pos,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =

    validateSide(baseLength, "base")
    validateSide(sideLength, "side")

    val halfHeight: Double =
      math.sqrt(sideLength * sideLength - baseLength * baseLength / 4.0) / 2.0

    val halfBase: Double = baseLength / 2.0

    createIsosceles(halfHeight, halfBase, center, fill, line)

  /** Creates a new isosceles triangle based on specified height and base length.
    *
    * @param height
    *   The height of the triangle. Must be non-negative.
    * @param baseLength
    *   The length of the base of the triangle. Must be non-negative.
    * @param fill
    *   Optional fill style for the triangle. None means no fill.
    * @param line
    *   Optional line style for the triangle. None means no line.
    * @return
    *   A `VectorGraphic` representing the isosceles triangle.
    * @throws IllegalArgumentException
    *   If either `height` or `baseLength` is negative.
    */
  def basedOnHeightAndBase(
                            height: Double,
                            baseLength: Double,
                            fill: Fill,
                            line: Stroke
  ): VectorGraphic =
    basedOnHeightAndBase(
      height,
      baseLength,
      Pos.Origin,
      fill,
      line
    )

  /** Creates a new isosceles triangle that has a specific center point, based on specified height
    * and base length.
    *
    * @param height
    *   The height of the triangle. Must be non-negative.
    * @param baseLength
    *   The length of the base of the triangle. Must be non-negative.
    * @param center
    *   The center position of the triangle.
    * @param fill
    *   Optional fill style for the triangle. None means no fill.
    * @param line
    *   Optional line style for the triangle. None means no line.
    * @return
    *   A `VectorGraphic` representing the isosceles triangle.
    * @throws IllegalArgumentException
    *   If either `height` or `baseLength` is negative.
    */
  def basedOnHeightAndBase(
                            height: Double,
                            baseLength: Double,
                            center: Pos,
                            fill: Fill,
                            line: Stroke
  ): VectorGraphic =

    validateSide(baseLength, "base")

    if height < 0 then
      throw new IllegalArgumentException(s"Height of triangle cannot be negative (was: $height).")

    createIsosceles(
      height / 2.0,
      baseLength / 2.0,
      center,
      fill,
      line
    )

  /** Helper method to create an isosceles triangle with a specific center point. This method
    * calculates corner positions based on half the height and half the base length, then constructs
    * the triangle.
    *
    * @param halfHeight
    *   Half the height of the triangle.
    * @param halfBase
    *   Half the base length of the triangle.
    * @param center
    *   The center position of the triangle.
    * @param fill
    *   Optional fill style for the triangle. None means no fill.
    * @param line
    *   Optional line style for the triangle. None means no line.
    * @return
    *   A `VectorGraphic` representing the isosceles triangle.
    */
  private def createIsosceles(
                               halfHeight: Double,
                               halfBase: Double,
                               center: Pos,
                               fill: Fill,
                               line: Stroke
  ): VectorGraphic =

    val firstCorner: Pos = Pos(0, -halfHeight)

    val secondCorner: Pos = Pos(center.x + halfBase, halfHeight)

    val thirdCorner: Pos = Pos(center.x - halfBase, halfHeight)

    apply(
      center,
      firstCorner,
      secondCorner,
      thirdCorner,
      fill,
      line
    )

  /** Creates a scalene triangle with specified side lengths.
    *
    * @param baseLength
    *   Length of the base of the triangle.
    * @param leftSideLength
    *   Length of the left side of the triangle.
    * @param rightSideLength
    *   Length of the right side of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing a scalene triangle.
    * @throws IllegalArgumentException
    *   if any of the lengths are negative or if the triangle inequality does not hold.
    */
  def apply(
             baseLength: Double,
             leftSideLength: Double,
             rightSideLength: Double,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    apply(
      baseLength,
      leftSideLength,
      rightSideLength,
      Pos.Origin,
      fill,
      line
    )

  /** Creates a scalene triangle with specified side lengths.
    *
    * @param baseLength
    *   Length of the base of the triangle.
    * @param leftSideLength
    *   Length of the left side of the triangle.
    * @param rightSideLength
    *   Length of the right side of the triangle.
    * @param center
    *   Center of the triangle.
    * @param fill
    *   Optional fill style for the triangle.
    * @param line
    *   Optional line style for the triangle.
    * @return
    *   A `VectorGraphic` representing a scalene triangle.
    * @throws IllegalArgumentException
    *   if any of the lengths are negative or if the triangle inequality does not hold.
    */
  def apply(
             baseLength: Double,
             leftSideLength: Double,
             rightSideLength: Double,
             center: Pos,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    validateSides(baseLength, leftSideLength, rightSideLength)

    val leftAngle: Double =
      val cos = (leftSideLength * leftSideLength + baseLength * baseLength - rightSideLength * rightSideLength) / (2 * leftSideLength * baseLength)
      import scala.math.*
      toDegrees(acos(cos))

    val prelimTop =
      Pos(leftSideLength, 0).rotateAroundOrigin(leftAngle)

    val xOffset =
      if leftAngle <= Angle.RightAngleInDegrees then
        val halfWidth = prelimTop.x.max(baseLength) / 2.0
        -halfWidth
      else
        val halfWidth = (prelimTop.x.abs + baseLength) / 2.0
        halfWidth - baseLength

    val halfHeight = prelimTop.y / 2.0

    val firstCorner: Pos = Pos(prelimTop.x + xOffset, -halfHeight)

    val secondCorner: Pos = Pos(xOffset, halfHeight)

    val thirdCorner: Pos = Pos(secondCorner.x + baseLength, halfHeight)

    apply(
      center,
      firstCorner,
      secondCorner,
      thirdCorner,
      fill,
      line
    )

  /** Creates a new scalene triangle based on corner positions relative to a center.
    *
    * @param center
    *   The center position of the triangle.
    * @param firstCornerRelativeToCenter
    *   Position of the first corner relative to the center.
    * @param secondCornerRelativeToCenter
    *   Position of the second corner relative to the center.
    * @param thirdCornerRelativeToCenter
    *   Position of the third corner relative to the center.
    * @param fill
    *   Optional fill style for the triangle. None means no fill.
    * @param line
    *   Optional line style for the triangle. None means no line.
    * @return
    *   A `VectorGraphic` representing a scalene triangle.
    */
  private def apply(
                     center: Pos,
                     firstCornerRelativeToCenter: Pos,
                     secondCornerRelativeToCenter: Pos,
                     thirdCornerRelativeToCenter: Pos,
                     fill: Fill,
                     line: Stroke
  ): VectorGraphic =

    val points: Seq[Pos] =
      Seq(firstCornerRelativeToCenter, secondCornerRelativeToCenter, thirdCornerRelativeToCenter)

    Polygon(center, points, fill, line)

  /** Validates the lengths of the sides of a triangle to ensure they adhere to the triangle
    * inequality theorem.
    *
    * @param baseLength
    *   Length of the base side of the triangle.
    * @param leftSideLength
    *   Length of the left side of the triangle.
    * @param rightSideLength
    *   Length of the right side of the triangle.
    * @throws IllegalArgumentException
    *   If the side lengths do not satisfy the triangle inequality theorem.
    */
  private def validateSides(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double
  ): Unit =

    validateSide(baseLength, "base")
    validateSide(leftSideLength, "left side")
    validateSide(rightSideLength, "right side")

    checkTriangleInequality(baseLength, leftSideLength, rightSideLength)

  /** Validates that a given side length is not negative.
    *
    * @param length
    *   The length of the side.
    * @param name
    *   The name of the side (for error messages).
    * @throws IllegalArgumentException
    *   If the length is negative.
    */
  private def validateSide(length: Double, name: String): Unit =
    if length < 0 then
      throw new IllegalArgumentException(
        s"Length of triangle's $name cannot be negative (was: $length)."
      )

  /** Checks the triangle inequality theorem for the given side lengths.
    *
    * @param a
    *   Length of the first side.
    * @param b
    *   Length of the second side.
    * @param c
    *   Length of the third side.
    * @throws IllegalArgumentException
    *   If the side lengths do not satisfy the triangle inequality theorem.
    */
  private def checkTriangleInequality(a: Double, b: Double, c: Double): Unit =
    if !((a + b >= c) && (a + c >= b) && (b + c >= a)) then
      throw new IllegalArgumentException(
        s"Illegal side lengths ($a, $b, $c): The triangle inequality does not hold."
      )
