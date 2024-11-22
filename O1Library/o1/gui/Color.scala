package o1.gui

import o1.util.nice.number.*

/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
//////
//////        COLOR COMPANION OBJECT
//////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////

/** This companion object of [[Color class `Color`]] provides methods for creating new `Color`
  * objects. There is also a small selection of related utility methods and constants.
  *
  * For many constants of type [[Color]] that represent different preset colors, see [[o1.gui.colors]].
  *
  * This object has an alias in the top-level package [[o1]], so it’s accessible to students simply
  * via `import o1.*`. */
object Color:

  /** The minimum value of a [[Color]]’s RGB and opacity components.
    * (They range between 0 and 255, so this equals zero.) */
  val Min: Int = 0
  /** The maximum value of a [[Color]]’s RGB and opacity components.
    * (They range between 0 and 255, so this equals 255.) */
  val Max: Int = 255

  /** Creates a new fully opaque [[Color]] with the given RGB components.
    * @param red   the amount of red   in the color; values outside the 0–255 range are clamped to fit it
    * @param green the amount of green in the color; values outside the 0–255 range are clamped to fit it
    * @param blue  the amount of blue  in the color; values outside the 0–255 range are clamped to fit it
    * @return the color */
  def apply(red: Double, green: Double, blue: Double): Color = apply(red, green, blue, opacity = Max)

  /** Creates a new [[Color]] with the given RGB components and opacity.
    * @param red       the amount of red   in the color; values outside the 0–255 range are clamped to fit it
    * @param green     the amount of green in the color; values outside the 0–255 range are clamped to fit it
    * @param blue      the amount of blue  in the color; values outside the 0–255 range are clamped to fit it
    * @param opacity   the color’s opacity             ; values outside the 0–255 range are clamped to fit it
    * @return the color */
  def apply(red: Double, green: Double, blue: Double, opacity: Double): Color =
    apply(red, green, blue, opacity, name = "")

  private[o1] def apply(argb: Int, name: String = ""): Color =
    apply((argb >> 16) & 0xff,
          (argb >> 8) & 0xff,
          (argb & 0xff),
          (argb >> 24) & 0xff,
          name)

  private[gui] def apply(red: Double, green: Double, blue: Double, opacity: Double, name: String): Color =
    val redInt     = clampComponent(red).toInt
    val greenInt   = clampComponent(green).toInt
    val blueInt    = clampComponent(blue).toInt
    val opacityInt = clampComponent(opacity).toInt
    val nameOption = Option(name.trim).filterNot( _.isEmpty )
    new Color(redInt, greenInt, blueInt, opacityInt, nameOption)


  /** Deconstructs a [[Color]] to its components and returns them as a tuple.
    * Its presence here in the companion object enables us to write things like:
    * ```scala
    * myColor match
    *   case Color(r, g, b, 255)          => "opaque color: " + r + "," + g + "," + b
    *   case Color(r, _, _, _) if r > 200 => "high in red"
    *   case _                            => "other color"
    * ```
    * @param color  any color
    * @return a `Some` that contains a tuple with all the RGB components of the given color and its opacity */
  def unapply(color: Color): Option[(Int, Int, Int, Int)] =
    Some((color.red, color.green, color.blue, color.opacity))


  private def clampComponent(n: Double) = n.clamp(Min, Max)


  private def rgbOf(color: Color) =
    (color.red, color.green, color.blue)


  private object preset:
    import scala.collection.mutable.Map
    val names: Map[(Int, Int, Int), List[String]] =
      Map() withDefaultValue List()
    def namesFor(color: Color): List[String] =
      names(rgbOf(color))
    def addName(color: Color, pascalCaseName: String) =
      val rgb = rgbOf(color)
      names(rgb) = pascalCaseName :: names(rgb)
  end preset


  private val DefaultFactor = 10.0


  /** Creates a [[Color]] specified in terms of the HSI (hue—saturation—intensity) color scheme.
    * @param hue         the color’s hue component (“main observable color”), in degrees around the color wheel (0–360)
    * @param saturation  the color’s saturation component (“richness”); values outside the 0.0–1.0 range are clamped to fit it
    * @param intensity   the color’s intensity component (“brightness”); values outside the 0–255 range are clamped to fit it
    * @param opacity     the color’s opacity; if unspecified, defaults to fully opaque ([[Color.Max]])
    * @return the color (represented internally as RGB nonetheless) */
  def fromHSI(hue: Double, saturation: Double, intensity: Double, opacity: Double = Max): Color =
    import HSI.*
    val normHue = hue.clamp(Hue.Min, Hue.Max)
    val normSat = saturation.clamp(Saturation.Min, Saturation.Max)
    val normIntensity = clampComponent(intensity)
    val normOpacity = clampComponent(opacity).toInt
    val (r, g, b) = hsiToRGB(normHue, normSat, normIntensity)
    Color(r, g, b, normOpacity)

  private def hsiToRGB(hue: Double, saturation: Double, intensity: Double): (Int, Int, Int) =
    val normalizedIntensity = intensity / 255.0

    // Calculate RGB between [0, 1]
    val C: Double = (1 - Math.abs(2 * normalizedIntensity - 1)) * saturation
    val X: Double = C * (1 - Math.abs((hue / 60.0) % 2 - 1))
    val m: Double = normalizedIntensity - C / 2
    val (r1, g1, b1): (Double, Double, Double) =
      if      hue < 60  then (C, X, 0.0)
      else if hue < 120 then (X, C, 0.0)
      else if hue < 180 then (0.0, C, X)
      else if hue < 240 then (0.0, X, C)
      else if hue < 300 then (X, 0.0, C)
      else                   (C, 0.0, X)

    // Scale to [0, 255]
    val red   = Math.round((r1 + m) * 255).toInt
    val green = Math.round((g1 + m) * 255).toInt
    val blue  = Math.round((b1 + m) * 255).toInt
    (red, green, blue)
  end hsiToRGB

  private object HSI:
    val FullCircle = 360
    object Hue:
      val Min = 0.0
      val Max = FullCircle.toDouble
      val Undefined = Double.NaN  // the center of the hue wheel
    object Saturation:
      val Min = 0.0
      val Max = 1.0
  end HSI

end Color



/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
//////
//////        CLASS COLOR
//////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////


/** Each instance of this class represents a color. The class uses the RGB color scheme: each color
  * is a combination of a red, green, and blue components; there’s also a fourth component of opacity.
  * `Color` objects are immutable.
  *
  * You don’t instantiate `Color` directly; instead, you create `Color`s with the methods on the
  * [[Color$ `Color` companion object]] (e.g., `Color(200, 150, 255)`) or use one of the named color
  * constants in [[o1.gui.colors]]. There are also a few methods in this class that returns new
  * [[Color]] objects defined in terms of existing ones (e.g., [[lighter]], [[edit]]).
  *
  * This class has an alias in the top-level package [[o1]], so it’s accessible to students simply
  * via `import o1.*`.
  *
  * @param red         the amount of red   in the color, between [[Color.Min]] and [[Color.Max]]; that is, 0–255
  * @param green       the amount of green in the color, between [[Color.Min]] and [[Color.Max]]; that is, 0–255
  * @param blue        the amount of blue  in the color, between [[Color.Min]] and [[Color.Max]]; that is, 0–255
  * @param opacity     the color’s opacity,              between [[Color.Min]] and [[Color.Max]]; that is, 0–255
  * @param presetName  the name of a color contant in “regular form”, i.e., with spaces and largely in lower case
  *                      (e.g., "saddle blue", "Irish green") */
final class Color private(val red: Int, val green: Int, val blue: Int, val opacity: Int, presetName: Option[String])
                  extends Fill derives CanEqual:
  this.validate()

  private[gui] def withName(name: String): Color =
    new Color(this.red, this.green, this.blue, this.opacity, Option(name))

  private val presetNameInPascalCase = presetName.map( _.split(" ").map( _.capitalize ).mkString )

  this.presetNameInPascalCase.foreach( Color.preset.addName(this, _) )

  /** the HSI (hue—saturation—intensity) equivalents of this RGB color
    * @return the three HSI components as a tuple; cf. [[hue]], [[saturation]], and [[intensity]] */
  lazy val hsi: (Double, Double, Double) =
    import Color.HSI.*
    val rgbSum = this.red + this.green + this.blue
    val hue =
      if this.isGray then
        Hue.Undefined
      else
        val redMinusGreen = this.red - this.green
        val redMinusBlue  = this.red - this.blue
        val root = Math.sqrt(redMinusGreen * redMinusGreen + redMinusBlue * (this.green - this.blue))
        val angleCandidate = Math.toDegrees(Math.acos((redMinusGreen + redMinusBlue) / (2.0 * root)))
        if this.green >= this.blue then angleCandidate else FullCircle - angleCandidate
    val saturation = if this.isBlack then Saturation.Min else 1.0 - 3.0 * (min(red, green, blue) / rgbSum)
    val intensity = rgbSum / 3.0
    (hue, saturation, intensity)
  end hsi

  private[o1] def averageColor: Color = this

  /** Returns a `Paint` object that corresponds to this color, for the purpose of
    * using this color as a [[Fill]] in Swing GUIs.  On `Color` objects, this method
    * never returns `None`. */
  def swingPaint: Option[java.awt.Paint] = Some(this.toSwingColor)

  /** Returns true if and only if this color’s [[opacity]] is zero. */
  def isTransparent: Boolean =
    this.opacity == 0

  /** Returns true if and only if this color’s [[opacity]] equals [[Color.Max]] (which equals 255). */
  def isOpaque: Boolean =
    this.opacity == Color.Max

  private def isGray =
    this.red == this.blue && this.blue == this.green

  private def isBlack =
    this.red == Color.Min && this.green == Color.Min && this.blue == Color.Min

  /** the hue (“main observable color”) component of this color when viewed in the HSI color scheme,
    * in degrees around the color wheel (0–360) */
  def hue: Double = this.hsi(0)
  /** the saturation (“richness”) component of the color when viewed in the HSI color scheme,
    * between [[Color.Min]] and [[Color.Max]]; that is, 0–255 */
  def saturation: Double = this.hsi(1)
  /** the intensity (“brightness”) component of this color when viewed in the HSI color scheme,
    * between [[Color.Min]] and [[Color.Max]]; that is, 0–255 */
  def intensity: Double = this.hsi(2)

  private[o1] def toARGBInt: Int =
    (this.opacity << 24) | (this.red << 16) | (this.green << 8) | this.blue


  /** Determines if this `Color` equals another object. It does if and only if the other object is
    * also a `Color` and its [[red]], [[green]], [[blue]], and [[opacity]] components are equal
    * to this `Color`’s. (Any names the colors may have are irrelevant.) */
  override def equals(other: Any): Boolean =
    other.asInstanceOf[Matchable] match  // “The cast is guaranteed to succeed at run-time since Any and Matchable both erase to Object.” https://docs.scala-lang.org/scala3/reference/other-new-features/matchable.html#matchable-and-universal-equality
      case Color(r, g, b, o) => r == this.red && g == this.green && b == this.blue && o == this.opacity
      case _                 => false


  /** A hash code generated for this `Color` from its four components. */
  override lazy val hashCode: Int =
    val prime = 31
    var result = 1
    result = prime * result + red
    result = prime * result + green
    result = prime * result + blue
    result = prime * result + opacity
    result


  /** Returns a color with the specified color components. The parameters default to this `Color`
    * object’s component values, so you can specify just the ones you want to change, as in
    * `myColor.edit(red = 255, opacity = 100)`. */
  def edit(red: Int = this.red, green: Int = this.green, blue: Int = this.blue, opacity: Int = this.opacity): Color =
    Color(red, green, blue, opacity)


  /** Returns the color whose R, G, and B components are the complements of this `Color` object’s
    * respective components (255 minus the value). Opacity is retained. */
  def negative = Color(Color.Max - this.red, Color.Max - this.green, Color.Max - this.blue, this.opacity)


  /** Returns a color that is somewhat lighter than this one. */
  def lighter: Color = this.lighter(Color.DefaultFactor)
  /** Returns a color that is somewhat darker than this one. */
  def darker: Color = this.darker(Color.DefaultFactor)

  /** Returns a lighter version of this color.
    * @param percentage  the percentage to lighten the color by, in the range [0, 100] */
  def lighter(percentage: Double): Color =
    val factor = (percentage / 100.0) atLeast 0.0 atMost 1.0
    val newRed   = (this.red   + factor * (Color.Max - this.red)).ceil.toInt
    val newGreen = (this.green + factor * (Color.Max - this.green)).ceil.toInt
    val newBlue  = (this.blue  + factor * (Color.Max - this.blue)).ceil.toInt
    Color(newRed, newGreen, newBlue, opacity)


  /** Returns a darker version of this color.
    * @param percentage  the percentage to darken the color by, in the range [0, 100] */
  def darker(percentage: Double): Color =
    val factor = (percentage / 100.0) atLeast 0.0 atMost 1.0
    val inverted = 1.0 - factor
    val newRed   = (inverted * this.red).toInt
    val newGreen = (inverted * this.green).toInt
    val newBlue  = (inverted * this.blue).toInt
    Color(newRed, newGreen, newBlue, opacity)



  /** Returns a string description of this color. This description reflects
    * how the color was created. Examples:
    *  - `"Red"`, `"CornflowerBlue"` (named preset colors)
    *  - `"Color(200, 100, 200)"` (opaque custom color)
    *  - `"Color(200, 100, 200, 50)"` (non-opaque custom color)
    *  @see [[name]], [[description]] */
  override def toString: String =
    this.presetNameInPascalCase getOrElse this.colorDetails

  /** If this color matches one of the named preset colors, returns that name; `None` otherwise.
    * In case this color matches multiple preset colors, uses one of those names.
    * @see [[description]], [[this.toString]] */
  def name: Option[String] =
    val names = Color.preset.namesFor(this)
    if this.presetNameInPascalCase.exists(names.contains) then this.presetNameInPascalCase else names.lastOption

  /** Returns a string description of this color, using preset color names where possible.
    * This description reflects the color’s RGB values and opacity. Examples:
    *  - `"Color(200, 100, 200)"` (opaque custom color)
    *  - `"transparent Color(200, 100, 200, 0)"` (fully transparent custom color)
    *  - `"translucent Color(200, 100, 200, 50)"` (non-opaque, non-transparent custom color)
    *  - `"opaque Brown"` (opaque color with RGB values 165, 42, and 42 — whether the constant LightGreen or an identical custom color)
    *  - `"opaque Brown"` (non-opaque color with RGB values 165, 42, and 42)
    *  - `"transparent Brown"` (fully transparent custom color with RGB values 165, 42, and 42)
    *  - `"translucent Brown"` (non-opaque, non-transparent custom color with RGB values 165, 42, and 42)
    * @see [[name]], [[this.toString]] */
  def description: String =
    val opacity = if this.isOpaque then "" else if this.isTransparent then "transparent" else "translucent"
    val space = if opacity.isEmpty then "" else " "
    val name = this.name getOrElse this.colorDetails
    s"$opacity$space$name"

  private def colorDetails =
    if this.opacity < Color.Max then s"Color($red, $green, $blue, $opacity)" else s"Color($red, $green, $blue)"


  /** Returns the [[java.awt.Color]] equivalent of this color (which is compatible with Swing GUIs). */
  def toSwingColor: java.awt.Color =
    new java.awt.Color(this.red, this.green, this.blue, this.opacity)


  inline private def validate() =
    import Color.Min, Color.Max
    def check(value: Double, name: String) =
      require(value >= Min && value <= Max, s"$name must be between $Min and $Max")
    check(red,     "Red")
    check(green,   "Green")
    check(blue,    "Blue")
    check(opacity, "Opacity")

end Color


