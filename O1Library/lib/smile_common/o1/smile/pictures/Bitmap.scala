package o1.smile.pictures

import o1.smile.Settings.DefaultPosition
import o1.gui.Color
import o1.smile.infrastructure.{BufferAdapter, JVMBufferAdapter, Renderer, ResourceFactory}
import o1.smile.modeling.*
import o1.world.Pos
import Transform.*

import java.awt.image.BufferedImage

/** Companion object for the `Bitmap` class, providing factory methods for creating `Bitmap` instances. */
object Bitmap:
  /** A type alias for functions generating colors based on pixel locations. */
  type LocationToColorGenerator = (Int, Int) => Color

  /** Creates a `Bitmap` of specified dimensions using a function to generate pixel colors.
    * @param width             The width of the bitmap.
    * @param height            The height of the bitmap.
    * @param contentGenerator  A function to generate colors for each pixel based on its location.
    * @return A new `Bitmap` instance. */
  def apply(width: Int, height: Int, contentGenerator: LocationToColorGenerator): Bitmap =
    new Bitmap(width, height).setColorsByLocation(contentGenerator)

  /** Creates a `Bitmap` from a sequence of colors, assuming row-wise population.
    * @param width   The width of the bitmap.
    * @param height  The height of the bitmap.
    * @param colors  A sequence of colors to populate the bitmap.
    * @return A new `Bitmap` instance. */
  def apply(width: Int, height: Int, colors: Seq[Color]): Bitmap =
    require(colors.length == (width * height), "The colors sequence must have exactly width * height items")
    val bitmap = new Bitmap(width, height)
    bitmap.buffer.setColorsFromSeq(colors)
    bitmap

  /** Loads a `Bitmap` from a resource path.
    * @param sourceResourcePath  The path to the source image resource.
    * @return A new `Bitmap` instance loaded from the specified path. */
  def apply(sourceResourcePath: String): Bitmap =
    val image = ResourceFactory().bufferAdapterFromPath(sourceResourcePath)
    new Bitmap(image, Bounds(DefaultPosition, image.width, image.height))

  /** Creates a `Bitmap` from a collection of picture elements.
    * @param elements  A collection of `PictureElement` instances to render into a bitmap.
    * @return A `Bitmap` representing the rendered picture elements. */
  def apply(elements: PictureElement*): Bitmap = Renderer().createBitmapFrom(elements*)

  /** Creates a `Bitmap` from a `Picture`.
    * @param picture  The `Picture` to render into a bitmap.
    * @return  A `Bitmap` representing the rendered picture. */
  def apply(picture: Picture): Bitmap = Renderer().createBitmapFrom(picture)

  val Empty = new Bitmap(BufferAdapter.Empty, NullBounds)

/** Represents a bitmap image, capable of holding and manipulating pixel data.
  * @param buffer  The underlying buffer storing pixel data.
  * @param bounds  The boundaries of the bitmap. */
class Bitmap(val buffer: BufferAdapter[?], bounds: Bounds) extends DrawableElement:

  /** Primary constructor for creating a `Bitmap` with specified dimensions.
    * @param width   The width of the bitmap.
    * @param height  The height of the bitmap.*/
  def this(width: Int, height: Int) =
    this(BufferAdapter(width, height), Bounds(DefaultPosition, width, height))

  import javax.swing.Icon
  def toAWTImage: BufferedImage = this.buffer.get.asInstanceOf[BufferedImage]    // N.B. platform-specific
  def toSwingIcon: Icon = this.buffer.asInstanceOf[JVMBufferAdapter].toSwingIcon // N.B. platform-specific

  override lazy val boundary: Bounds = bounds

  /** Creates a copy of this `Bitmap` at a new position.
    * @param newPosition  The new position for the bitmap.
    * @return A new `Bitmap` instance at the specified position. */
  override def copy(newPosition: Pos): Bitmap =
    new Bitmap(buffer, boundary.moveBy(newPosition.x, newPosition.y))

  private def internalCopy(newBuffer: BufferAdapter[?] = buffer, newBounds: Bounds = bounds): Bitmap =
    new Bitmap(newBuffer, newBounds)

  /** Moves this `Bitmap` by specified offsets.
    * @param xOffset  the horizontal offset
    * @param yOffset  the vertical offset
    * @return A new `Bitmap` instance moved by the specified offsets. */
  override def moveBy(xOffset: Double, yOffset: Double): Bitmap =
    internalCopy(newBounds = boundary.moveBy(xOffset, yOffset))

  /** Creates a deep copy of this `Bitmap`. */
  def deepCopy(): Bitmap = internalCopy(buffer.deepCopy)

  /** Gets the color of a pixel at specified coordinates. */
  def getColor(x: Int, y: Int): Color = buffer.pixelColor(x, y)

  /** Optionally gets the color at specified coordinates, returning `None` if out of bounds.
    * @param x  the x coordinate
    * @param y  the y coordinate
    * @return An `Option[Color]` containing the color if within bounds, or `None`. */
  def colorAt(x: Int, y: Int): Option[Color] =
    if x < 0 || x >= buffer.width || y < 0 || y >= buffer.height then None
    else Some(getColor(x, y))

  /** Merges this `Bitmap` with another `Bitmap` using a specified pixel merging function.
    * @param another      The other bitmap to merge with.
    * @param pixelMerger  A function that takes two colors and returns a merged color.
    * @return A new `Bitmap` instance representing the merged image. */
  def mergeWith(another: Bitmap, pixelMerger: (Color, Color) => Color): Bitmap =
    val resultWidth  = buffer.width.min(another.buffer.width)
    val resultHeight = buffer.height.min(another.buffer.height)
    new Bitmap(buffer.mergeWith(another.buffer, pixelMerger), Bounds(DefaultPosition, resultWidth, resultHeight))

  /** Sets the colors of the bitmap based on a function that generates colors for each pixel location.
    * @param generator  A function that generates a color for a given pixel location.
    * @return The `Bitmap` instance with updated colors. */
  def setColorsByLocation(generator: (Int, Int) => Color): Bitmap =
    val result = new Bitmap(buffer.width, buffer.height)
    result.buffer.setColorsByLocation(generator)
    result

  /** Transforms each pixel color of this bitmap using a specified color transformer function.
    * @param transformer  a function that takes a color and returns a transformed color
    * @return A new `Bitmap` instance with transformed colors. */
  def transformColorToColor(transformer: Color => Color): Bitmap =
    this.internalCopy(newBuffer = buffer.transformColorToColor(transformer))

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Bitmap =
    this.scaleBy(horizontalFactor, verticalFactor, position)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double, relativityPoint: Pos): Bitmap =
    this.scaleTo(horizontalFactor * buffer.width, verticalFactor * buffer.height, relativityPoint)

  override def scaleTo(targetWidth: Double, targetHeight: Double, relativityPoint: Pos): Bitmap =
    if targetWidth.toInt == 0 || targetHeight.toInt == 0 then
      Bitmap.Empty
    else
      val newCenter = boundary.center.scaleBy(targetWidth / buffer.width, targetHeight / buffer.height, relativityPoint)
      val newBuffer = buffer.scaleTo(targetWidth, targetHeight)
      val newUpperLeftCorner = newCenter.subtract(targetWidth / 2.0, targetHeight / 2.0)
      val newLowerRightCorner = newCenter.add(targetWidth / 2.0, targetHeight / 2.0)
      val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)
      this.internalCopy(newBuffer, newBounds)

  override def rotateAround(pivot: Pos, degrees: Double): Bitmap =
    val newBuffer = this.transformBuffer(rotationAround(x = this.buffer.width / 2.0, y = this.buffer.height / 2.0, angle = degrees))
    val oldPositions = Seq(boundary.upperLeftCorner, boundary.upperRightCorner, boundary.lowerLeftCorner, boundary.lowerRightCorner)
    val newBounds = BoundaryCalculator.fromPositions(oldPositions.map( _.rotateAround(pivot, degrees) ))
    this.internalCopy(newBuffer, newBounds)

  override def rotateAroundOrigin(degrees: Double): Bitmap =
    val newBuffer = this.transformBuffer(rotationAround(x = this.buffer.width / 2.0, y = this.buffer.height / 2.0, angle = degrees))
    val newCenter = boundary.center.rotateAroundOrigin(degrees)
    val newUpperLeftCorner = newCenter.subtract(newBuffer.width / 2.0, newBuffer.height / 2.0)
    val newLowerRightCorner = newUpperLeftCorner.add(newBuffer.width - 1.0, newBuffer.height - 1.0)
    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner)
    internalCopy(newBuffer, newBounds)

  /** Transforms the content of this bitmap using a specified [[Transform]]. This method
    * is internal and facilitates various geometric transformations.
    * @param  transformation A [[Transform]] instance to apply to the bitmap.
    * @return A new [[BufferAdapter]] containing the transformed pixel data. */
  private def transformBuffer(transformation: Transform): BufferAdapter[?] =
    this.buffer.transform(transformation)

  /** Flips the bitmap content horizontally. This is equivalent to reflecting the image across a
    * vertical axis running through the image's center.
    * @return A new [[Bitmap]] instance with horizontally flipped content. */
  def flipHorizontally: Bitmap =
    val newBuffer = this.transformBuffer(horizontalFlip(buffer.width))
    internalCopy(newBuffer)

  /** Flips the bitmap content vertically. This is equivalent to reflecting the image across a
    * horizontal axis running through the image's center.
    * @return A new [[Bitmap]] instance with vertically flipped content. */
  def flipVertically: Bitmap =
    val newBuffer = this.transformBuffer(verticalFlip(buffer.height))
    internalCopy(newBuffer)

  /** Flips the bitmap content diagonally. This is equivalent to rotating the image by 180 degrees
    * around the point at the center of its bounds.
    * @return A new [[Bitmap]] instance with diagonally flipped content. */
  def flipDiagonally: Bitmap =
    val newBuffer = this.transformBuffer(diagonalFlip(buffer.width, buffer.height))
    internalCopy(newBuffer)

  override def crop(upperLeftXInPixels: Double, upperLeftYInPixels: Double, lowerRightXInPixels: Double, lowerRightYInPixels: Double): Bitmap =
    val upperLeftX  = upperLeftXInPixels.floor
    val upperLeftY  = upperLeftYInPixels.floor
    val lowerRightX = lowerRightXInPixels.floor
    val lowerRightY = lowerRightYInPixels.floor
    val xMin = upperLeftX.min(lowerRightX)
    val xMax = upperLeftX.max(lowerRightX)
    val yMin = upperLeftY.min(lowerRightY)
    val yMax = upperLeftY.max(lowerRightY)
    require(xMin >= 0, s"X coordinate $xMin is negative")
    require(xMax <= buffer.width, s"X coordinate $xMax is out of bounds")
    require(yMin >= 0, s"Y coordinate $yMin is negative")
    require(yMax <= buffer.height, s"Y coordinate $yMax is out of bounds")
    val resultingWidth  = xMax - xMin
    val resultingHeight = yMax - yMin
    if resultingWidth < 1 || resultingHeight < 1 then return Bitmap.Empty
    val newBuffer = buffer.copyPortionXYXY(xMin, yMin, xMax, yMax)
    val newUpperLeftCorner  = Pos(xMin, yMin)
    val newLowerRightCorner = Pos(xMax, yMax)
    val newBounds = Bounds(newUpperLeftCorner, newLowerRightCorner) // LATER: check properly
    internalCopy(newBuffer = newBuffer, newBounds = newBounds)

  // def saveAsPngTo(path: String): Boolean = false // LATER: implement using buffer.saveToPath?
end Bitmap
