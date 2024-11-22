package o1.smile.pictures

import o1.smile.Settings.DefaultPosition
import o1.world.Pos
import o1.gui.{Fill, Stroke}

/** Factory object for creating rectangles.
  */
object Rectangle:

  /** Creates a square with a given side length, fill style, and line style.
    *
    * @param sideLength
    *   The length of each side of the square.
    * @param fill
    *   Optional fill style for the square.
    * @param line
    *   Optional line style for the square.
    * @return
    *   A `VectorGraphic` representing the square.
    */
  def apply(
             sideLength: Double,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    apply(sideLength, DefaultPosition, fill, line)

  /** Creates a square with a given side length, center position, fill style, and line style.
    *
    * @param sideLength
    *   The length of each side of the square.
    * @param center
    *   The center position of the square.
    * @param fill
    *   Optional fill style for the square.
    * @param line
    *   Optional line style for the square.
    * @return
    *   A `VectorGraphic` representing the square.
    * @throws IllegalArgumentException
    *   If the side length is negative.
    */
  def apply(
             sideLength: Double,
             center: Pos,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    if sideLength < 0.0 then
      throw new IllegalArgumentException(
        s"Square's side length cannot be negative (was: $sideLength)."
      )

    apply(sideLength, sideLength, center, fill, line)

  /** Creates a rectangle with a given base length, height, fill style, and line style.
    *
    * @param baseLength
    *   The length of the base of the rectangle.
    * @param height
    *   The height of the rectangle.
    * @param fill
    *   Optional fill style for the rectangle.
    * @param line
    *   Optional line style for the rectangle.
    * @return
    *   A `VectorGraphic` representing the rectangle.
    */
  def apply(
             baseLength: Double,
             height: Double,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    apply(baseLength, height, DefaultPosition, fill, line)

  /** Creates a rectangle with a given base length, height, center position, fill style, and line
    * style.
    *
    * @param baseLength
    *   The length of the base of the rectangle.
    * @param height
    *   The height of the rectangle.
    * @param center
    *   The center position of the rectangle.
    * @param fill
    *   Optional fill style for the rectangle.
    * @param line
    *   Optional line style for the rectangle.
    * @return
    *   A `VectorGraphic` representing the rectangle.
    * @throws IllegalArgumentException
    *   If the base length or height is negative.
    */
  def apply(
             baseLength: Double,
             height: Double,
             center: Pos,
             fill: Fill,
             line: Stroke
  ): VectorGraphic =
    if baseLength < 0.0 then
      throw new IllegalArgumentException(
        s"Rectangle's base length cannot be negative (was: $baseLength)."
      )
    if height < 0.0 then
      throw new IllegalArgumentException(s"Rectangle's height cannot be negative (was: $height).")

    val cornerPoints =
      val halfWidth  = baseLength / 2.0
      val halfHeight = height / 2.0

      Seq(
        Pos(-halfWidth, -halfHeight),
        Pos(halfWidth, -halfHeight),
        Pos(halfWidth, halfHeight),
        Pos(-halfWidth, halfHeight)
      )

    Polygon(
      center,
      cornerPoints,
      fill,
      line
    )
