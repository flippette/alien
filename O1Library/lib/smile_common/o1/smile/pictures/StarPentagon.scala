package o1.smile.pictures

import o1.smile.Settings.DefaultPosition
import o1.smile.modeling.Angle
import o1.gui.{Fill, Stroke}
import o1.world.Pos

/** Factory object for creating star-shaped pentagons. */
object Star5:

  /** Intersperses elements of two lists, starting with the first element of the first list.
    * @param firstList   The first list of elements.
    * @param secondList  The second list of elements.
    * @tparam ListItem   The type of elements in the lists.
    * @return A list containing elements from both lists, starting with the first list. */
  private def intersperse[ListItem](firstList: List[ListItem], secondList: List[ListItem]): List[ListItem] =
    firstList match
      case first :: tail => first :: intersperse(secondList, tail)
      case _             => secondList

  /** Creates a star pentagon with specified dimensions, fill, and line styles.
    * @param width       Width of the bounding box of the star pentagon.
    * @param height      Height of the bounding box of the star pentagon.
    * @param cuspRadius  Radius from the center to the inner corners of the star.
    * @param fill        Optional fill style for the star pentagon.
    * @param line        Optional line style for the star pentagon.
    * @return A `VectorGraphic` representing the star pentagon.
    * @throws IllegalArgumentException If any of the dimensions are negative. */
  def apply(width: Double, height: Double, cuspRadius: Double, fill: Fill, line: Stroke): VectorGraphic =
    apply(width, height, cuspRadius, DefaultPosition, fill, line)

  /** Creates a star pentagon with specified dimensions, center position, fill, and line styles.
    * @param width       Width of the bounding box of the star pentagon.
    * @param height      Height of the bounding box of the star pentagon.
    * @param cuspRadius  Radius from the center to the inner corners of the star.
    * @param center      Center position of the star pentagon.
    * @param fill        Optional fill style for the star pentagon.
    * @param line        Optional line style for the star pentagon.
    * @return A `VectorGraphic` representing the star pentagon centered at the given position.
    * @throws IllegalArgumentException If any of the dimensions are negative. */
  def apply(width: Double, height: Double, cuspRadius: Double, center: Pos, fill: Fill, line: Stroke): VectorGraphic =
    if width < 0 then
      throw new IllegalArgumentException(s"Star pentagon's width cannot be negative (was: $width).")
    if height < 0 then
      throw new IllegalArgumentException(s"Star pentagon's width cannot be negative (was: $height).")
    if cuspRadius < 0 then
      throw new IllegalArgumentException(s"Length of star pentagon's cusp radius cannot be negative (was: $cuspRadius).")
    val circumradius = Pentagon.limitCircumradiusTo(width, height)
    apply(circumradius, cuspRadius, center, fill, line)

  /** Creates a star pentagon with specified circumradius, cusp radius, center position, fill, and line styles.
    * @param circumradius  Radius from the center to the outer corners of the star.
    * @param cuspRadius    Radius from the center to the inner corners of the star.
    * @param center        Center position of the star pentagon.
    * @param fill          Optional fill style for the star pentagon.
    * @param line          Optional line style for the star pentagon.
    * @return A `VectorGraphic` representing the star pentagon centered at the given position.
    * @throws IllegalArgumentException If either circumradius or cuspRadius is negative. */
  def apply(circumradius: Double, cuspRadius: Double, center: Pos, fill: Fill, line: Stroke): VectorGraphic =
    if circumradius < 0 then
      throw new IllegalArgumentException(s"Length of star pentagon's circumradius cannot be negative (was: $circumradius).")
    if cuspRadius < 0 then
      throw new IllegalArgumentException(s"Length of star pentagon's cusp radius cannot be negative (was: $cuspRadius).")
    val outerPoints = Pentagon.pointsFor(circumradius, Angle.Zero).toList
    val innerPoints = cuspRadiusPointsFor(cuspRadius).toList
    val points      = intersperse(outerPoints, innerPoints)
    Polygon(center, points, fill, line)

  /** Generates the inner cusp points of a star pentagon.
    * @param cuspRadius Radius from the center to the inner corners of the star.
    * @return A sequence of positions for the inner points. */
  private def cuspRadiusPointsFor(cuspRadius: Double): Seq[Pos] =
    Pentagon.pointsFor(cuspRadius, Pentagon.RotationalSymmetryAngle / 2)

end Star5