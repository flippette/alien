package o1.gui

import o1.smile.modeling.{PositionType, Bounds as SmileBounds}
import o1.smile.pictures.{Bitmap as SmileBitmap, Picture as SmilePicture, PictureElement as SmilePictureElement, 
                          ReferencePoint as SmileKeypoint}
import o1.smile.pictures.Transformable.SimpleIdentity
import o1.gui.Align.*
import o1.gui.compat.*
import o1.gui.Anchor.*
import o1.gui.PicHistory.op.{AdjustViewport, Create, Miscellaneous, Transform, Process}

import scala.util.Try
import o1.util.nice.number.*
import o1.gui.colors.Transparent

import java.awt.image.BufferedImage
import javax.swing.Icon


/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
//////
//////        PIC COMPANION OBJECT
//////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////

/** The primary purpose of this companion object of [[Pic class `Pic`]] is to provide methods for
  * creating new [[Pic]] instances: ([[apply]], [[generate]], [[circle]], etc. There is also a small
  * selection of related utility methods.
  *
  * This object has an alias in the top-level package [[o1]], so it’s accessible to students simply
  * via `import o1.*`. The shape-creating methods of this object (such as `circle`) are also available
  * as functions in package [[o1]]. */
object Pic extends ShapeAPI:

  /** Takes an image file path or URL as a string, loads image data from that location, and constructs
    * an anchored [[Pic]] from that data. Throws an error if the file doesn’t exist or could not
    * be accessed (cf. [[asTry]], [[asOption]]).
    * @param pathOrURL  a classpath-relative path or a URL string that starts with "http://" or "https://".
    *                   Note that the O1Library project exports its `pics` folder, so the example images
    *                   in that folder can be loaded using just the file name.
    * @param anchor     an anchor for the new `Pic`
    * @return the loaded image (a bitmap) */
    // * @throws AccessDeniedByServerError                   for HTTP status codes 401, 402, 403, 407, and 451
    // * @throws EmptyFileError                              if the given path points to an empty file
    // * @throws FileAttributeRetrievalFailedError           if the attributes of the file that the given path points to could not be retrieved
    // * @throws FileNotFoundError                           if the given path points to a file that does not seem to exist
    // * @throws ImageInputStreamNotCreatedError             if a cache file is needed but could not be created
    // * @throws ImageNotFoundError                          for HTTP status codes 204, 205, 404, and 410, and if the requested resource could not be found
    // * @throws ImageReaderNotRetrievedError                if the first suitable [[ImageReader]] cannot be retrieved
    // * @throws MaximumBitmapSizeExceededError              if a bitmap is larger than the maximum allowed bitmap size
    // * @throws MinimumBitmapSizeNotMetError                if a bitmap is smaller than the minimum allowed bitmap size
    // * @throws OperationPreventedBySecurityManagerError    if retrieval of file attributes was prevented by a security manager
    // * @throws PathDoesNotPointToRegularFileError          if the given path does not point to a regular file
    // * @throws PathIsEmptyOrOnlyWhitespaceError            if the given path is empty or contains only whitespace
    // * @throws PathIsNullError                             if the given path was actually null
    // * @throws PathPointsToFolderError                     if the given path points to a folder
    // * @throws PathPointsToSymbolicLinkError               if the given path poins to a symbolic link
    // * @throws RedirectionRequestedError                   for HTTP status codes 301, 302, 307, and 308
    // * @throws RequestedURITooLongError                    for HTTP status code 414
    // * @throws ServerError                                 for all HTTP status codes beginning with 5
    // * @throws SuitableImageReaderNotFoundError            if no suitable [[ImageReader]] is found
    // * @throws SuitableImageStreamProviderNotFoundError    if [[ImageIO]] did not find a suitable image stream service provider instance
    // * @throws TooManyRequestsToServerError                for HTTP status code 429
    // * @throws UnknownFileExtensionError                   if the file extension is unknown
    // * @throws UnknownHTTPResponseError                    for all HTTP status codes other than 200 that are not reported with other exceptions
    // * @throws UnknownMIMETypeError                        if the MIME type sent by the server is not supported
    // * @throws UnableToRetrieveDataOverHTTPConnectionError if an I/O error occurs while creating an [[InputStream]] or if the protocol to be used does not support input
    // * @throws UnableToOpenHTTPConnectionError             if an [[HttpURLConnection]] instance could not be created; if the HTTP request method cannot be reset; if the request method is not valid; if the connection timeout expires before a connection has been established; or if an I/O error occurs during establishing the connection/
  def apply(pathOrURL: String, anchor: Anchor): Pic =
    val loadedImage = SmileBitmap(pathOrURL)
    val newHistory = PicHistory(Create(method = "Pic", simpleDescription = pathOrURL))
    apply(smileContent = loadedImage, anchor, newHistory)

  /** Takes an image file path or URL as a string, loads image data from that location, and constructs
    * a [[Pic]] from that data. Anchors the `Pic` at its center. Throws an error if the file
    * doesn’t exist or could not be accessed (cf. [[asTry]], [[asOption]]).
    * @param pathOrURL  a classpath-relative path or a URL string that starts with "http://" or "https://".
    *                   Note that the O1Library project exports its `pics` folder, so the example images
    *                   in that folder can be loaded using just the file name.
    * @return the loaded image (a bitmap) */
  def apply(pathOrURL: String): Pic = this(pathOrURL, Center)

  private[gui] final def apply(smileContent: SmilePicture, anchor: Anchor, history: PicHistory): Pic =
    new Pic(smileContent, anchor, history)

  private[gui] final def apply(smileContent: SmilePictureElement, anchor: Anchor, history: PicHistory): Pic =
    new Pic(smileContent.toPicture, anchor, history)


  /** An image with no content. */
  val empty: Pic =
    val newHistory = PicHistory(Create(method = "Pic.empty", simpleDescription = "empty image"))
    new Pic(SmilePicture(), Center, newHistory)


  /** Takes an image file path or URL as a string and attempts to load image data from that location and
    * construct a [[Pic]] from that data.
    * @param pathOrURL  a classpath-relative path or a URL string that starts with "http://" or "https://".
    * @param anchor     an anchor for the new `Pic`; if unspecified, defaults to [[Center]]
    * @return the loaded [[Pic]] (a bitmap) or the error that caused the attempt to fail
    * @see [[asOption]] */
  def asTry(pathOrURL: String, anchor: Anchor = Center): Try[Pic] =
    Try(Pic(pathOrURL, anchor))

  /** Takes an image file path or URL as a string and attempts to load image data from that location and
    * construct a [[Pic]] from that data.
    * @param pathOrURL  a classpath-relative path or a URL string that starts with "http://" or "https://".
    * @param anchor     an anchor for the new `Pic`; if unspecified, defaults to [[Center]]
    * @return the loaded [[Pic]] (a bitmap); `None` in case the attempt failed for any reason
    * @see [[asTry]] */
  def asOption(pathOrURL: String, anchor: Anchor = Center): Option[Pic] =
    Pic.asTry(pathOrURL, anchor).toOption

  /** Takes an image file path or URL as a string and attempts to load image data from that location and
    * construct a [[java.awt.image.BufferedImage BufferedImage]] from that data. Note that this method
    * does not construct a [[Pic]] at all.
    * @param pathOrURL  a classpath-relative path or a URL string that starts with "http://" or "https://".
    * @return the loaded image; `None` in case the attempt failed for any reason */
  def asImage(pathOrURL: String): Option[BufferedImage] =
    Pic.asOption(pathOrURL).map( _.toImage )


  /** Displays the given [[Pic]] in a minimalistic GUI window. Calling this method repeatedly on the
    * same [[Pic]] doesn’t display multiple windows but reuses the existing one. This method is meant
    * only for experimentation and debugging; not for GUI construction (cf. [[o1.gui.mutable.ViewFrame views]]).
    * @param pic              the image to display; should have a height and width of at least one pixel
    * @param background        the color to appear behind the `Pic` in the frame (where the `Pic` is transparent)
    * @param border            the width of the simple black window frame, in pixels
    * @see [[hide]]
    * @see [[Pic.show]] */
  def show(pic: Pic, background: Color, border: Int): Unit =
    val reasonNotToShow = pic.dimensions match
      case (w, h) if w < 1 && h < 1 => Some(s"The picture is too small to show (width=$w < 1, height=$h < 1).")
      case (w, h) if w < 1          => Some(s"The picture is too narrow to show (width=$w < 1).")
      case (w, h) if h < 1          => Some(s"The picture is too flat to show (height=$h < 1).")
      case pictureLargeEnoughToShow => if o1.gui.isInTestMode then Some("Not actually showing the picture because in text-based test mode.") else None
    reasonNotToShow match
      case Some(reason) => println(reason)
      case None         => PicFrame.show(pic, background, border)


  /** Hides the window that has been created (with [[show]]) to display the given [[Pic]].
    * If there is no such window, does nothing.
    * @see [[show]], [[hideAll]] */
  def hide(pic: Pic): Unit =
    PicFrame.hide(pic)

  /** Hides any and all windows that have been created (with [[show]]) to display [[Pic]]s.
    * @see [[show]], [[hide]] */
  def hideAll(): Unit =
    PicFrame.hideAll()


  /** Creates a new [[Pic]] by applying the given pixel-generating function to each pair coordinates
    * within the new image.
    * @param width      the width, in pixels, of the new `Pic`
    * @param height     the height, in pixels, of the new `Pic`
    * @param makeColor  a function that `generate` calls on every pixel location of
    *                   the new `Pic` to produce the color at that location
    * @return the generated [[Pic]] (a bitmap) */
  def generate(width: Int, height: Int, makeColor: (Int, Int) => Color): Pic =
    val smileContent = o1.smile.pictures.Bitmap(width, height, makeColor)
    val newHistory = PicHistory(Create(method = "Pic.generate", simpleDescription = "generated pic"))
    Pic(smileContent, anchor = Center, newHistory)


  /** Creates a new [[Pic]] by setting colors of each individual pixel according to the `colors` list.
    * @param width  the width, in pixels, of the new `Pic`
    * @param height the height, in pixels, of the new `Pic`
    * @param colors a sequence of colors, one per pixel, starting from the upper-left corner and
    *               continuing to the right and line-by-line towards the bottom
    * @return the new [[Pic]] (a bitmap) */
  def fromColors(width: Int, height: Int, colors: Seq[Color]): Pic =
    val smileContent = o1.smile.pictures.Bitmap(width, height, colors)
    val newHistory = PicHistory(Create(method = "Pic.fromColors", simpleDescription = "composed pic"))
    Pic(smileContent, anchor = Center, newHistory)

  private[gui] def leftToRight(pics: Seq[Pic]) = pics.reduceLeft( _ leftOf _ )
  private[gui] def topToBottom(pics: Seq[Pic]) = pics.reduceLeft( _ above _ )


  // Utilities for the crop methods
  private inline def normalizeBound(base: Double, span: Double): (Double, Double) =
    if base < 0 then (0, span - base.abs) else (base, span)
  private inline def normalizeBoundsTuple(left: Double, top: Double, width: Double, height: Double): (Double, Double, Double, Double) =
    val (nLeft, nWidth) = normalizeBound(left, width)
    val (nTop, nHeight) = normalizeBound(top, height)
    (nLeft, nTop, nWidth, nHeight)
  private inline def normalizeBounds(bounds: Bounds): (Double, Double, Double, Double) =
    normalizeBoundsTuple(bounds.left, bounds.top, bounds.width, bounds.height)


end Pic



/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
//////
//////        CLASS PIC
//////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////

/** Each instance of this class represents a picture: an immutable two-dimensional image.
  *
  * You don’t instantiate `Pic` directly; instead, you create `Pic`s with the methods on the
  * [[Pic$ `Pic` companion object]]. For instance, you can:
  *
  *  - load existing images: `Pic("mypic.png")` or
  *    `Pic("http://address.of/mypic.png")`),
  *  - create `Pic`s of shapes (e.g., `circle(150, Red)` or
  *    `star(100, Black)`); or
  *  - generate the pixels of a `Pic` with a function.
  *
  * Moreover, many of the methods of a `Pic` object create and return new `Pic`s that
  * represent combinations or transformations of existing `Pic`s.
  *
  * This class has an alias in the top-level package [[o1]], so it’s accessible to students
  * simply via `import o1.*`.
  *
  * The examples below illustrate a few of the methods (assuming the above import):
  *
  * ```scala
  * val background = rectangle(400, 300, Red)
  * val rotatedSquare = square(50, Blue).clockwise(30)
  * val squareAgainstBg = background.place(rotatedSquare, Pos(100, 100))
  * squareAgainstBg.show()
  * ```
  *
  * ```scala
  * val ladybug = Pic("ladybug.png").scaleTo(100).flipHorizontal
  * val combination = circle(100, Red).leftOf(ladybug)
  * val part = combination.crop(Pos(10, 10), 180, 80)
  * val negative = part.transformColors( _.negative )
  * negative.show()
  * ```
  *
  * Some of the methods of a `Pic` that use [[o1.world.Pos Pos]] objects or plain x and y coordinates to
  * indicate positions within the `Pic` (e.g., `place` and `crop` above). All these methods
  * consider the origo to be at the top left-hand corner or the `Pic`, with x values increasing
  * downwards and y values rightwards. The coordinates are in pixels.
  *
  * Each image has an [[anchor]] that defines where it connects to other `Pic`s. By default,
  * the anchor is [[o1.world.objects.Anchor.Center Center]]; for example, the `place` method call above puts the center of
  * the square at `Pos(100, 100)` within the background.
  *
  * Here is a list of the main types of operations as methods on a `Pic`, and examples of each type:
  *
  *  - Combining `Pic`s by positioning them relative to each
  *    other: `above`, `below`, `leftOf`, `rightOf`, `onto`,
  *    `against`, `place`.
  *
  *  - Rotations (*): `clockwise`, `counterclockwise`
  *
  *  - Changing size (*): `scaleBy`, `scaleTo`
  *
  *  - Selecting and moving pixels (**): `crop`, `shift`,
  *    `flipHorizontal`, `flipVertical`
  *
  *  - Examining and manipulating individual pixels (**):
  *    `pixelColor`, `transformXY`, `transformColors`, `combine`
  *
  *  - Convenience methods for experimentation and
  *    debugging: `show`, `hide`.
  *
  * ### Notes on implementation and efficiency:
  *
  * Internally, a `Pic` stores its contents either as vector-based graphics, as a bitmap (raster),
  * or as a combination of the two. By design, that internal representation is meant to be largely
  * opaque to the user of the `Pic` class: students in O1 working on O1’s standard assignments
  * generally shouldn’t need to know or care about it. Nevertheless, whether a particular `Pic`
  * is stored in vector or bitmap form does have very substantial effect on efficiency in some
  * contexts; `Pic`, like the rest of O1Library, is not designed for high-performance graphics.
  *
  * Some users of this class may wish to know the following:
  *
  *  - `Pic`s start out in either vector form or bitmap form,
  *    depending on which method created them. Specifically, the
  *    shape-creating methods (like `rectangle` and `circle`).
  *    produce `Pic`s in vector form.
  *
  *  - No operation on a `Pic` ever changes an already rasterized
  *    bitmap into vector graphics. (An operation such as `leftOf`
  *    can produce an `Pic` that is stored as a combination of
  *    a vector graphic and a bitmap.)
  *
  *  - Some operations always produce a rasterized `Pic`. These
  *    are marked with a double asterisk (**) in the list above.
  *    Some operations sometimes produce rasterized `Pic`s but
  *    may retain the vector-based representation in simple cases.
  *    These are marked with a single asterisk (*).
  *
  *  - You can call [[freeze]] to force rasterization. */
final class Pic private[gui](private[gui] val smilePic: SmilePicture, itsAnchor: Anchor,
                             private[o1] val historyDetails: PicHistory)
                extends HasAnchor derives CanEqual:

  given deriveOtherPicsFrom: Pic = this

  /** The `Pic`’s anchor. */
  val anchor: Anchor = itsAnchor

  /** A list of method names used for creating this `Pic`. Only the most recently applied methods
    * are listed (up to [[historyLength]]. This method may be useful for demonstrative purposes,
    * debugging, and/or automatic assessment. */
  lazy val history: List[String] = this.historyDetails.methodList

  /** A limit on the number of historical method calls that are tracked by the `Pic` and available
    * through [[history]]. Defaults to 10. Can be changed via [[withHistoryLength]]. */
  val historyLength: Int = this.historyDetails.maximumLength

  /** Sets a new limit to the history length on the `Pic`.
    * @param maxLength  a new [[historyLength]] (>= 0) */
  def withHistoryLength(maxLength: Int): Pic =
    Pic(this.smilePic, this.anchor, this.historyDetails.withMaxLength(maxLength))


  /** Returns a very simple string description of this `Pic`. Examples:
    *  — `"ladybug.png"`
    *  — `circle-shape`
    *  — `"ladybug.png" (transformed)`
    *  — `circle-shape (transformed)`
    *  — `combined pic` */
  override def toString: String =
    val builder = StringBuilder()
    builder ++= this.historyDetails.creationOp.simpleDescription
    if this.historyDetails.containsTransformations then
      builder ++= " (transformed)"
    builder.toString


  /** Forces rasterization of any vector graphics in this `Pic`’s internal representation
    * into bitmap form. See the [[Pic introduction to this class]] for more information.
    * @return the same picture, stored entirely in bitmap form
    * @see [[isBitmap]] */
  def freeze: Pic =
    this.toSmileBitmap.toO1Pic(Miscellaneous("freeze"))


  inline private def toSmileBitmap: SmileBitmap =
    if this.isBitmap then
      this.smilePic.elements.head.asInstanceOf[SmileBitmap]
    else
      this.smilePic.toBitmap


  /** Indicates whether this `Pic` is internally stored as a bitmap raster.
    * See the [[Pic introduction to this class]] for more information.
    * @return `true` if the entire `Pic` is rasterized as a bitmap;
    *         `false` if the `Pic`’s internal representation uses any vector graphics
    * @see [[freeze]] */
  def isBitmap: Boolean =
    this.smilePic.elements.sizeIs == 1 && this.smilePic.elements.head.isInstanceOf[SmileBitmap]


  /** Displays this `Pic` in a minimalistic GUI window. Calling this method repeatedly on the same
    * `Pic` doesn’t display multiple windows but reuses the existing one. This method is meant only for
    * experimentation and debugging; not for GUI construction (cf. [[o1.gui.mutable.ViewFrame views]]).
    *
    * Students may alternatively use the `show(pic)` variant in the top-level package [[o1]].
    *
    * @param background        the color to appear behind the `Pic` in the frame (where the `Pic` is transparent);
    *                          defaults to a light gray (`White70`)
    * @param border            the width of the simple black window frame, in pixels; if unspecified, defaults to 1
    * @see [[hide]]
    * @see [[Pic$.show Pic.show]] */
  def show(background: Color = colors.White70, border: Int = 1): Unit =
    Pic.show(this, background, border)

  /** Hides the window that has been created (with [[show]]) to display the `Pic`.
    * If there is no such window, does nothing.
    *
    * Students may alternatively use the `hide(pic)` variant in the top-level package [[o1]].
    *
    * @see [[show]], [[Pic$.hide Pic.hide]] */
  def hide(): Unit = Pic.hide(this)


  //-----------------------------------------------------------------------------------------------
  //
  //
  // Dimensions and locations
  //
  //
  //-----------------------------------------------------------------------------------------------

  private[gui] lazy val boundary: Bounds =
    val smileBounds: SmileBounds = this.smilePic.viewport.map( _.boundary ) getOrElse this.smilePic.boundary
    smileBounds.toO1Bounds

  /** The width of the `Pic` in pixels. */
  lazy val width: Double = this.smilePic.viewport.map( _.width.inPixels ) getOrElse this.smilePic.width.inPixels
  /** The height of the `Pic` in pixels. */
  lazy val height: Double = this.smilePic.viewport.map( _.height.inPixels ) getOrElse this.smilePic.height.inPixels
  /** The [[width]] and [[height]] of the `Pic` as a tuple. */
  lazy val dimensions: (Double, Double) = (this.width, this.height)


  /** Determines if the given [[o1.world.Pos Pos]] is within this `Pic`’s borders. For that to be the case,
    * the `Pos` object’s `x` needs to be between zero (inclusive) and this `Pic`’s `width` (exclusive)
    * and its `y` needs to be between zero (inclusive) and this `Pic`’s `height` (exclusive). */
  def contains(pixel: Pos): Boolean = pixel.x.isBetween(0, this.width) && pixel.y.isBetween(0, this.height)


  /** Returns the colors of the pixels in this `Pic` that are “near” a given [[o1.world.Pos Pos]].
    * A pixel counts as being near if it’s located at a `Pos` whose distance from the given `Pos` is less
    * than or equal to the given `range`. This includes the pixel at the given `Pos` (if there is one).
    *
    * Distances are measured between the “centers” of the two pixels. For example, if pixel A is three
    * pixels right and four pixels down pixel B, then their distance is five.
    *
    * @param pos    a position whose nearby pixels in this `Pic` will be returned
    * @param range  the maximum distance of a returned pixel from `pos` (must be non-negative)
    * @return a collection of the pixel colors, in arbitrary order */
  def pixelsNear(pos: Pos, range: Double): Seq[Color] =
    val maxDelta = range.abs.round
    def candidatesAround(center: Double) =
      BigDecimal(center - maxDelta) to BigDecimal(center + maxDelta) by 1.0
    val possNear = for
      y <- candidatesAround(pos.y)
      x <- candidatesAround(pos.x)
      candidate = Pos(x.toDouble, y.toDouble)
      if pos.distance(candidate) <= range && this.contains(candidate)
    yield candidate
    possNear.map(this.pixelColor).toSeq


  //-----------------------------------------------------------------------------------------------
  //
  //
  // Anchors
  //
  //
  //-----------------------------------------------------------------------------------------------

  /** An [[o1.world.objects.Anchor.Absolute Anchor.Absolute]] that points to the same spot
    * in the `Pic` that the `Pic`’s current anchor does. */
  def absoluteAnchor: Anchor = this.anchor.toAbsoluteWithin(this)

  /** Anchors the `Pic` at the given position.
    * @param anchor  a new anchoring position
    * @return a new `Pic` that’s identical to the original but anchored at the given position */
  infix def anchorAt(anchor: Anchor): Pic =
    this.smilePic.toO1Pic(anchor, Miscellaneous("anchorAt"))

  // LATER: This currently supports named anchors. Remove this when Smile properly integrated.
  private[o1] lazy val smileTopLeft: Pos = this.smilePic.boundary.upperLeftCorner

  //-----------------------------------------------------------------------------------------------
  //
  //
  // Viewports
  //
  //
  //-----------------------------------------------------------------------------------------------

  // Viewport-related operations are not public in O1Library at the present time.

  private[gui] lazy val viewport: Viewport =
    this.smilePic.viewport.map( _.toO1Viewport ) getOrElse Viewport.NotSet

  inline private[gui] def setViewportFrom(picture: Pic): Pic = setViewport(picture.viewport)

  private[gui] def setViewportToContentBoundary: Pic = setViewportToContentBoundaryOf(this)

  inline private[gui] def setViewportToContentBoundaryOf(picture: Pic): Pic =
    this.setViewport(picture.smilePic.boundary.toO1Bounds)

  inline private[gui] def setViewport(topLeft: Pos, bottomRight: Pos): Pic =
    this.setViewport(topLeft.x, topLeft.y, bottomRight.x, bottomRight.y)
  inline private[gui] def setViewport(left: Double, top: Double, right: Double, bottom: Double): Pic =
    val bounds = Bounds(left, top, right - left + 1, bottom - top + 1)
    this.setViewport(bounds)
  inline private[gui] def setViewport(boundary: Bounds): Pic =
    setViewport(Viewport(boundary))
  inline private[gui] def setViewport(viewport: Viewport): Pic =
    val possibleViewport = Option(viewport).flatMap( _.toSmileViewport )
    val smileContent = possibleViewport.map(this.smilePic.setViewport) getOrElse this.smilePic.withoutViewport
    smileContent.toO1Pic(AdjustViewport("setViewport"))

  inline private[gui] def hasViewport: Boolean = this.smilePic.hasViewport

  private[gui] def removeViewport: Pic =
    val smileContent = this.smilePic.withoutViewport
    smileContent.toO1Pic(AdjustViewport("removeViewport"))


  //-----------------------------------------------------------------------------------------------
  //
  //
  // Exporting images
  //
  //
  //-----------------------------------------------------------------------------------------------

  /** This `Pic` converted to a [[java.awt.image.BufferedImage Java AWT `BufferedImage`]]
    * (for use in Swing GUIs). */
  lazy val toImage: BufferedImage =
    this.toSmileBitmap.toAWTImage  // Unless this pic is already in bitmap form, this will be less efficient as the pic will be rasterized first.

  /** This `Pic` converted to a [[javax.swing.Icon Swing `Icon`]]. */
  lazy val toIcon: Icon =
    this.toSmileBitmap.toSwingIcon


  //  Saves the `Pic` in the given file in PNG format.
  //  @param path  an absolute local path or a path relative to the working directory
  //  @return `true` if the saving was successful and `false` if no file was created because there was no bitmap to save */
  //  * @throws FileOverwritingIsDeniedBySmileError       if given path points to an existing file (not folder)
  //  * @throws ImageWriterNotRetrievedError             if the first suitable [[ImageWriter]] cannot be retrieved
  //  * @throws ImageWritingFailedError                  if an [[IOException]] occurred while writing to the file represented by the given path
  //  * @throws OperationPreventedBySecurityManagerError if an existing security manager prevents access to the file represented by the given path
  //  * @throws PathIsNullError                          if given path is null
  //  * @throws PathIsEmptyOrOnlyWhitespaceError         if given path is an empty string or contains only whitespace
  //  * @throws PathPointsToFolderError                  if given path points to an existing folder
  //  * @throws SuitableImageWriterNotFoundError         if no suitable [[ImageWriter]] is found
  //  * @throws UnableToOpenFileForWritingError          if the file represented by the given path cannot be opened
  // def save(path: String): Boolean = this.toSmileBitmap.saveAsPngTo(path)



  //-----------------------------------------------------------------------------------------------
  //
  //
  // Color Transformations
  //
  //
  //-----------------------------------------------------------------------------------------------

  /** Returns the color of the pixel at the given position within the image.
    *
    * The given position must be within the image’s bounds; an out-of-bounds
    * error will occur otherwise.
    *
    * *N.B.* This is inefficient on a `Pic` that’s not fully rasterized as a bitmap.
    * If you have a vector graphic and need to call this method many times, consider
    * [[freeze freezing]] the `Pic` first. */
  def pixelColor(pos: Pos): Color = pos.turnInto(this.pixelColor)

  /** Returns the color of the pixel at the given coordinates within the image.
    *
    * The given position must be within the image’s bounds; an out-of-bounds
    * error will occur otherwise.
    *
    * *N.B.* This is inefficient on a `Pic` that’s not fully rasterized as a bitmap.
    * If you have a vector graphic and need to call this method many times, consider
    * [[freeze freezing]] the `Pic` first. */
  def pixelColor(x: Int, y: Int): Color =
    this.toSmileBitmap.colorAt(x, y) getOrElse Transparent

  /** Returns the color of the pixel at the given coordinates within the image.
    *
    * The given position must be within the image’s bounds; an out-of-bounds
    * error will occur otherwise.
    *
    * *N.B.* This is inefficient on a `Pic` that’s not fully rasterized as a bitmap.
    * If you have a vector graphic and need to call this method many times, consider
    * [[freeze freezing]] the `Pic` first. */
  def pixelColor(x: Double, y: Double): Color =
    this.toSmileBitmap.colorAt(x.floor.toInt, y.floor.toInt) getOrElse Transparent

  /** Returns the color of the pixel at the given coordinates within the image.
    * (This is equivalent to calling `pixelColor`.)
    *
    * The given position must be within the image’s bounds; an out-of-bounds
    * error will occur otherwise.
    *
    * *N.B.* This is inefficient on a `Pic` that’s not fully rasterized as a bitmap.
    * If you have a vector graphic and need to call this method many times, consider
    * [[freeze freezing]] the `Pic` first. */
  def apply(pixelX: Int, pixelY: Int): Color =
    this.pixelColor(pixelX, pixelY)

  /** Returns the color of the pixel at the given coordinates within the image.
    * (This is equivalent to calling `pixelColor`.)
    *
    * The given position must be within the image’s bounds; an out-of-bounds
    * error will occur otherwise.
    *
    * *N.B.* This is inefficient on a `Pic` that’s not fully rasterized as a bitmap.
    * If you have a vector graphic and need to call this method many times, consider
    * [[freeze freezing]] the `Pic` first. */
  def apply(pixelPos: Pos): Color =
    this.pixelColor(pixelPos)


  /** Applies the given operation to each pixel in the `Pic` to generate a different `Pic`.
    * That is, every color in the image will be replaced by another color in the generated image,
    * as specified by the given function.
    * @param transformer  a function that `transformColors` calls on every pixel in the image to map its color to a new one
    * @return the new (bitmap) image obtained by calling `transformer` and putting together the outputs */
  def transformColors(transformer: Color => Color): Pic =
    val smileContent = this.toSmileBitmap.transformColorToColor(transformer)
    smileContent.toO1Pic(Transform("transformColors"))


  /** Applies the given operation to each position in the `Pic` to generate a different `Pic`.
    * @param newColorAt  a function that `transformXY` calls repeatedly to determine the pixels of the transformed `Pic`
    * @return the new (bitmap) image obtained by calling `newColorAt` and putting together the outputs */
  def transformXY(newColorAt: (Int, Int) => Color): Pic =
    val smileContent = this.toSmileBitmap.setColorsByLocation(newColorAt)
    smileContent.toO1Pic(Transform("transformXY"))


  //-----------------------------------------------------------------------------------------------
  //
  //
  // Geometrical Transformations
  //
  //
  //-----------------------------------------------------------------------------------------------

  /** Returns a partial `Pic` of a rectangular area within this `Pic`.
    * @param boundary  the cropping frame within this `Pic`
    * @return a new `Pic` (a bitmap) that contains a part of the original */
  def crop(boundary: Bounds): Pic =
    val (left, top, width, height) = Pic.normalizeBounds(boundary)
    val ulCorner = Pos(left, top)
    val smileContent = this.smilePic.crop(ulCorner, width atLeast 0, height atLeast 0).toPicture
    val smileContentWithViewport =
      if this.smilePic.hasViewport then smileContent.withContentBoundaryAsViewport else smileContent
    smileContentWithViewport.toO1Pic(Transform("crop"))

  /** Returns a partial `Pic` of a rectangular area within this `Pic`.
    * @param topLeft      the cropping frame’s top-left corner within this `Pic`; this point must not be outside the `Pic`
    * @param bottomRight  the cropping frame’s bottom-right corner within this `Pic`; this point must not be outside the `Pic`
    * @return a new `Pic` (a bitmap) that contains a part of the original */
  def crop(topLeft: Pos, bottomRight: Pos): Pic =
    val smileContent = this.smilePic.crop(
        upperLeftX = topLeft.x,
        upperLeftY = topLeft.y,
        lowerRightX = bottomRight.x,
        lowerRightY = bottomRight.y).toPicture
    val smileContentWithViewport =
      if this.smilePic.hasViewport then smileContent.withContentBoundaryAsViewport else smileContent
    smileContentWithViewport.toO1Pic(Transform("crop"))

  /** Returns a partial `Pic` of a rectangular area within this `Pic`.
    * @param topLeft  the cropping frame’s top-left corner within this `Pic`
    * @param width    the width of the cropping frame
    * @param height   the height of the cropping frame
    * @return a new `Pic` (a bitmap) that contains a part of the original */
  def crop(topLeft: Pos, width: Double, height: Double): Pic =
    val (x, y, nWidth, nHeight) = Pic.normalizeBoundsTuple(topLeft.x, topLeft.y, width, height)
    val ulCorner = Pos(x, y)
    val smileContent = this.smilePic.crop(ulCorner, nWidth atLeast 0, nHeight atLeast 0).toPicture
    val smileContentWithViewport =
      if this.smilePic.hasViewport then smileContent.withContentBoundaryAsViewport else smileContent
    smileContentWithViewport.toO1Pic(Transform("crop"))

  /** Returns a partial `Pic` of a rectangular area within this `Pic`.
    * @param x        the x coordinate of the cropping frame’s left edge within this `Pic`
    * @param y        the y coordinate of the cropping frame’s top edge within this `Pic`
    * @param width    the width of the cropping frame
    * @param height   the height of the cropping frame
    * @return a new `Pic` (a bitmap) that contains a part of the original */
  def crop(x: Double, y: Double, width: Double, height: Double): Pic =
    val (left, top, nWidth, nHeight) = Pic.normalizeBoundsTuple(x, y, width, height)
    val ulCorner = Pos(left, top)
    val smileContent = this.smilePic.crop(ulCorner, nWidth atLeast 0, nHeight atLeast 0).toPicture
    val smileContentWithViewport =
      if this.smilePic.hasViewport then smileContent.withContentBoundaryAsViewport else smileContent
    smileContentWithViewport.toO1Pic(Transform("crop"))



  /** Creates a version of this `Pic` that is larger or smaller as per the given multiplier.
    * A value between 0 and 1 produces a smaller `Pic`, a value above 1 a larger one.
    * Retains the aspect ratio of the original image.
    * @param factor  the ratio between the sizes of the result and the original
    * @return a new `Pic` that is a scaled version of this one; it will usually be
    *         a bitmap, but some simple vector-based `Pic`s may retain their vector form */
  def scaleBy(factor: Double): Pic =
    val smileContent = this.smilePic.scaleBy(factor)
    smileContent.toO1Pic(Transform("scaleBy"))

  /** Creates a scaled version of this `Pic` that has the given dimensions. The aspect ratio
    * of the resulting image may be different from the original’s.
    * @param targetWidth   the width of the output image
    * @param targetHeight  the height of the output image
    * @return a new `Pic` that is a scaled version of this one; it will usually be
    *         a bitmap, but some simple vector-based `Pic`s may retain their vector form */
  def scaleTo(targetWidth: Double, targetHeight: Double): Pic =
    val smileContent = this.smilePic.scaleTo(targetWidth, targetHeight)
    smileContent.toO1Pic(Transform("scaleTo"))

  /** Creates a scaled version of this `Pic` that has the given size and is rectangular. If
    * the original `Pic` isn’t rectangular, the aspect ratio of the result will be different.
    * @param targetSize  the width and height of the output image
    * @return a new `Pic` that is a scaled version of this one; it will usually be
    *         a bitmap, but some simple vector-based `Pic`s may retain their vector form */
  def scaleTo(targetSize: Double): Pic =
    val smileContent = this.smilePic.scaleTo(targetSize, targetSize)
    smileContent.toO1Pic(Transform("scaleTo"))

  /** Creates a scaled version of this `Pic` that has the dimensions of the given `Pic`.
    * The aspect ratio of the resulting image may be different from the original’s.
    * @param sizingPic  a `Pic` whose width and height are given to the scaled `Pic`
    * @return a new `Pic` that is a scaled version of this one; it will usually be
    *         a bitmap, but some simple vector-based `Pic`s may retain their vector form */
  def scaleTo(sizingPic: Pic): Pic =
    this.scaleTo(sizingPic.width, sizingPic.height)


  /** Creates a mirrored version of this `Pic`: what’s on the left in the original is on the right
    * in the resulting `Pic` and vice versa.
    * @return a new `Pic` (a bitmap) that is a flipped version of this one */
  def flipHorizontal: Pic =
    val smileContent = this.toSmileBitmap.flipHorizontally.toPicture
    val smileContentWithViewport = this.smilePic.viewport.map(smileContent.setViewport) getOrElse smileContent
    smileContentWithViewport.toO1Pic(Transform("flipHorizontal"))

  /** Creates a mirrored version of this `Pic`: what’s at the top in the original is at the bottom
    * in the resulting `Pic` and vice versa.
    * @return a new `Pic` (a bitmap) that is a flipped version of this one */
  def flipVertical: Pic =
    val smileContent = this.toSmileBitmap.flipVertically.toPicture
    val smileContentWithViewport = this.smilePic.viewport.map(smileContent.setViewport) getOrElse smileContent
    smileContentWithViewport.toO1Pic(Transform("flipVertical"))

  /** Creates a mirrored version of this `Pic`: what’s in each corner of the original is in the
    * opposite corner of the resulting `Pic`.
    * @return a new `Pic` (a bitmap) that is a flipped version of this one */
  def flipDiagonal: Pic =
    val smileContent = this.toSmileBitmap.flipDiagonally.toPicture
    val smileContentWithViewport = this.smilePic.viewport.map(smileContent.setViewport) getOrElse smileContent
    smileContentWithViewport.toO1Pic(Transform("flipDiagonal"))


  /** Creates a version of this `Pic` that’s rotated clockwise around its center. The resulting image
    * is sized so that the entire contents of the original `Pic` are visible; any empty space in the
    * corners is [[Transparent]].
    * @param  degrees the amount of clockwise rotation, in degrees; if unspecified, defaults to 90.0
    * @return a new `Pic` that is a rotated version of this one; it will usually be a
    *         bitmap, but some simple vector-based `Pic`s may retain their vector form */
  def clockwise(degrees: Double = 90.0): Pic =
    val unrotated = if this.smilePic.hasText then this.freeze.smilePic else this.smilePic
    // LATER: remove this hasText-based hack for text rotations
    val smileContent = unrotated.rotateAround(this.centerOfRotation, -degrees)
    smileContent.toO1Pic(Transform("clockwise"))

  /** Creates a version of this `Pic` that’s rotated counterclockwise around its center. The resulting
    * image is sized so that the entire contents of the original `Pic` are visible; any empty space
    * in the corners is [[Transparent]].
    * @param  degrees the amount of counterclockwise rotation, in degrees; if unspecified, defaults to 90.0
    * @return a new `Pic` that is a rotated version of this one; it will usually be a
    *         bitmap, but some simple vector-based `Pic`s may retain their vector form */
  def counterclockwise(degrees: Double = 90.0): Pic =
    val unrotated = if this.smilePic.hasText then this.freeze.smilePic else this.smilePic
    // LATER: remove this hasText-based hack for text rotations
    val smileContent = unrotated.rotateAround(this.centerOfRotation, degrees)
    smileContent.toO1Pic(Transform("counterclockwise"))

  private def centerOfRotation: Pos =
    val anchorLocation = this.anchor.internalPosWithin(this)
    val ulCorner = this.smilePic.boundary.upperLeftCorner
    val centerX = ulCorner.x + anchorLocation.x
    val centerY = ulCorner.y + anchorLocation.y
    Pos(centerX, centerY)


  /** Shifts each pixel within the `Pic` to the right by the given amount. Retains the
    * size of the image: any pixels that would go beyond the right edge of the `Pic`
    * instead wrap around to the left-hand side.
    * @param offset  the number of pixels the image shifts to the right
    * @return a new `Pic` (a bitmap) that contains a shifted version of the original */
  def shiftRight(offset: Double): Pic =
    val normalizedOffset = offset % this.width
    if normalizedOffset == 0 then
      this
    else
      val actualOffset = if normalizedOffset > 0 then normalizedOffset else (this.width + normalizedOffset) % this.width
      val leftSmileContent  = this.smilePic.crop(Pos(0, 0), this.width - actualOffset, this.height)
      val rightSmileContent = this.smilePic.crop(Pos(this.width - actualOffset, 0), actualOffset, this.height)
      val smileContent = leftSmileContent.addToLeft(content = rightSmileContent, gap = 0, align = VMiddle)
      this.shiftedContentInViewport(smileContent).toO1Pic(Transform("shiftRight"))


  /** Shifts each pixel within the `Pic` to the left by the given amount. Retains the
    * size of the image: any pixels that would go beyond the left edge of the `Pic`
    * instead wrap around to the right-hand side.
    * @param offset  the number of pixels the image shifts to the left
    * @return a new `Pic` (a bitmap) that contains a shifted version of the original */
  def shiftLeft(offset: Double): Pic =
    val normalizedOffset = offset % this.width
    if normalizedOffset == 0 then
      this
    else
      val actualOffset = if normalizedOffset >= 0 then normalizedOffset else (this.width + normalizedOffset) % this.width
      val leftSmileContent = this.smilePic.crop(Pos(0, 0), actualOffset, this.height)
      val rightSmileContent = this.smilePic.crop(Pos(actualOffset, 0), this.width - actualOffset, this.height)
      val smileContent = rightSmileContent.addToRight(content = leftSmileContent, gap = 0, align = VMiddle)
      this.shiftedContentInViewport(smileContent).toO1Pic(Transform("shiftLeft"))


  /** Shifts each pixel within the `Pic` downwards by the given amount. Retains the
    * size of the image: any pixels that would go beyond the bottom edge of the `Pic`
    * instead wrap around to the top.
    * @param offset  the number of pixels the image shifts down
    * @return a new `Pic` (a bitmap) that contains a shifted version of the original */
  def shiftDown(offset: Double): Pic =
    val normalizedOffset = offset % this.height
    if normalizedOffset == 0 then
      this
    else
      val actualOffset = if normalizedOffset > 0 then normalizedOffset else (this.height + normalizedOffset) % this.height
      val topSmileContent = this.smilePic.crop(Pos(0, 0), this.width, this.height - actualOffset)
      val bottomSmileContent = this.smilePic.crop(Pos(0, this.height - actualOffset), this.width, actualOffset)
      val smileContent = topSmileContent.addToTop(content = bottomSmileContent, gap = 0, align = HMiddle)
      this.shiftedContentInViewport(smileContent).toO1Pic(Transform("shiftDown"))



  /** Shifts each pixel within the `Pic` upwards by the given amount. Retains the
    * size of the image: any pixels that would go beyond the top edge of the `Pic`
    * instead wrap around to the bottom.
    * @param offset  the number of pixels the image shifts up
    * @return a new `Pic` (a bitmap) that contains a shifted version of the original */
  def shiftUp(offset: Double): Pic =
    val normalizedOffset = offset % this.height
    if normalizedOffset == 0 then
      this
    else
      val actualOffset = if normalizedOffset > 0 then normalizedOffset else (this.height + normalizedOffset) % this.height
      val topSmileContent = this.smilePic.crop(Pos(0, 0), this.width, actualOffset)
      val bottomSmileContent = this.smilePic.crop(Pos(0, actualOffset), this.width, this.height - actualOffset)
      val smileContent = bottomSmileContent.addToBottom(content = topSmileContent, gap = 0, align = HMiddle)
      this.shiftedContentInViewport(smileContent).toO1Pic(Transform("shiftUp"))


  inline private def shiftedContentInViewport(smileContent: SmilePicture) =
    if this.smilePic.hasViewport then smileContent.withContentBoundaryAsViewport else smileContent


  //-----------------------------------------------------------------------------------------------
  //
  //
  // Combining Pictures
  //
  //
  //-----------------------------------------------------------------------------------------------

  /** Combines this `Pic` and the given one so that this `Pic` appears in front of the other
    * `Pic`. The combined `Pic` will be larger than either of the two originals unless this
    * `Pic` fits completely in front of the other one. This version of `onto` uses the given
    * [[o1.world.objects.Anchor Anchor]]s instead of the default `anchor` of either of the original `Pic`s.
    * @param backPic  the `Pic` that is behind this one in the combination
    * @param my       the point within this `Pic` that will be placed at `atIts`
    * @param atIts    the point within `backPic` that this `Pic` is positioned at
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but leaves out any “hanging” parts of the foreground image */
  def onto(backPic: Pic, my: Anchor, atIts: Anchor): Pic =
    this.onto(backPic, my, atIts.internalPosWithin(backPic))

  /** Combines this `Pic` and the given one so that this `Pic` appears in front of the other
    * `Pic`. The combined `Pic` will be larger than either of the two originals unless this
    * `Pic` fits completely in front of the other one. This version of `onto` anchors this
    * `Pic` from its default [[anchor]] to an anchor in the other image.
    * @param backPic  the `Pic` that is behind this one in the combination
    * @param atIts    the point within `backPic` that this `Pic`’s [[anchor]]
    *                 is positioned at; if unspecified, defaults to [[Center]]
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but leaves out any “hanging” parts of the foreground image */
  infix def onto(backPic: Pic, atIts: Anchor = Center): Pic = this.onto(backPic, anchor, atIts)

  /** Combines this `Pic` and the given one so that this `Pic` appears in front of the other
    * `Pic` at specific coordinates. The combined `Pic` will be larger than either of the two
    * originals unless this `Pic` fits completely in front of the other one. This version of
    * `onto` anchors this `Pic` at its default [[anchor]].
    * @param backPic  the `Pic` that is behind this one in the combination
    * @param at       the point within `backPic` that this `Pic`’s [[anchor]] is positioned at
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but leaves out any “hanging” parts of the foreground image */
  def onto(backPic: Pic, at: Pos): Pic = this.onto(backPic, anchor, at)

  /** Combines this `Pic` and the given one so that this `Pic` appears in front of the other `Pic`
    * at specific coordinates. The combined `Pic` will be larger than either of the two originals
    * unless this `Pic` fits completely in front of the other one. This version of `onto` uses the
    * given [[o1.world.objects.Anchor Anchor]] instead of this `Pic`s default `anchor`.
    * @param backPic  the `Pic` that is behind this one in the combination
    * @param my       the point within this `Pic` that will be placed at the given coordinates
    * @param at       the point within `backPic` that this `Pic` is positioned at
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but leaves out any “hanging” parts of the foreground image */
  def onto(backPic: Pic, my: Anchor, at: Pos): Pic =
    this.onto(backPic, my, at, newViewport = Viewport.NotSet)

  inline private def onto(backPic: Pic, my: Anchor, at: Pos, newViewport: Viewport): Pic =
    val bkgUpperLeftCorner = backPic.boundary.pos
    val absoluteAtPosOnBkg = bkgUpperLeftCorner.add(at.x, at.y)

    val myInternalPos = my.internalPosWithin(this)
    val xAbsolute = absoluteAtPosOnBkg.x - myInternalPos.x
    val yAbsolute = absoluteAtPosOnBkg.y - myInternalPos.y

    val newSmileContent = backPic.smilePic.addAt(this.smilePic, xAbsolute, yAbsolute, PositionType.UpperLeftCorner)

    val newViewportIfAny = Option(newViewport).flatMap( _.toSmileViewport )
    val newSmileContentVPSet = newViewportIfAny.map(newSmileContent.setViewport) getOrElse newSmileContent.withoutViewport
    Pic(newSmileContentVPSet, backPic.anchor, historyForCombinedPic)
  end onto


  /** Combines this `Pic` and the given one so that the other `Pic` serves as a background and this
    * `Pic` appears in front of it but not beyond its borders. The visible part of the combined `Pic`
    * is the size of the background `Pic`. In the combination, some or even none of this `Pic` is visible
    * in front of the background, depending on the relative positioning and dimensions of the two images;
    * any parts of this `Pic` that don’t fit against the background are left out. (The left-out parts are
    * still part of the `Pic`’s data but not visible; only the part within the framing background image
    * will be shown when the `Pic` is rendered onscreen.) This version of `against` anchors this `Pic`
    * at its default [[anchor]] to a specific [[o1.world.Pos Pos]] in the background.
    * @param background  a `Pic` that serves as a background for this one and determines the size of
    *                    the visible part of the resulting image
    * @param at          the point within `background` that this `Pic`’s [[anchor]] is positioned at
    * @return the combined `Pic`
    * @see `onto`,
    *      which does the same but doesn’t leave out the “hanging” parts of the foreground image
    * @see `place`,
    *      which does the same but switches the order of the two `Pic`s */
  def against(background: Pic, at: Pos): Pic =
    this.against(background, anchor, at)

  /** Combines this `Pic` and the given one so that the other `Pic` serves as a background and this
    * `Pic` appears in front of it but not beyond its borders. The visible part of the combined `Pic`
    * is the size of the background `Pic`. In the combination, some or even none of this `Pic` is visible
    * in front of the background, depending on the relative positioning and dimensions of the two images;
    * any parts of this `Pic` that don’t fit against the background are left out. (The left-out parts are
    * still part of the `Pic`’s data but not visible; only the part within the framing background image
    * will be shown when the `Pic` is rendered onscreen.) This version of `against` uses the given [[o1.world.objects.Anchor Anchor]]
    * instead of this `Pic`s default `anchor` and places it at a specific [[o1.world.Pos Pos]] in the background.
    * @param background  a `Pic` that serves as a background for this one and determines the size of
    *                    the visible part of the resulting image
    * @param my          the point within this `Pic` that will be placed at the given coordinates
    * @param at          the point within `background` that this `Pic` is positioned at
    * @return the combined `Pic`
    * @see `onto`,
    *      which does the same but doesn’t leave out the “hanging” parts of the foreground image
    * @see `place`,
    *      which does the same but switches the order of the two `Pic`s */
  def against(background: Pic, my: Anchor, at: Pos): Pic =
    val newViewport = background.smilePic.viewport.map( _.toO1Viewport ) getOrElse Viewport(background.boundary)
    this.onto(background, my, at, newViewport)

  /** Combines this `Pic` and the given one so that the other `Pic` serves as a background and this
    * `Pic` appears in front of it but not beyond its borders. The visible part of the combined `Pic`
    * is the size of the background `Pic`. In the combination, some or even none of this `Pic` is visible
    * in front of the background, depending on the relative positioning and dimensions of the two images;
    * any parts of this `Pic` that don’t fit against the background are left out. (The left-out parts are
    * still part of the `Pic`’s data but not visible; only the part within the framing background image will
    * be shown when the `Pic` is rendered onscreen.) This version of `against` uses the given [[o1.world.objects.Anchor Anchor]]s
    * instead of the default `anchor` of either of the original `Pic`s.
    * @param background  a `Pic` that serves as a background for this one and determines the size of
    *                    the visible part of the resulting image
    * @param my          the point within this `Pic` that will be placed at the given coordinates
    * @param atIts       the point within `background` that this `Pic` is positioned at
    * @return the combined `Pic`
    * @see `onto`,
    *      which does the same but doesn’t leave out the “hanging” parts of the foreground image
    * @see `place`,
    *      which does the same but switches the order of the two `Pic`s */
  def against(background: Pic, my: Anchor, atIts: Anchor): Pic =
    this.against(background, my, atIts.internalPosWithin(background))

  /** Combines this `Pic` and the given one so that the other `Pic` serves as a background and this
    * `Pic` appears in front of it but not beyond its borders. The visible part of the combined `Pic`
    * is the size of the background `Pic`. In the combination, some or even none of this `Pic` is visible
    * in front of the background, depending on the relative positioning and dimensions of the two images;
    * any parts of this `Pic` that don’t fit against the background are left out. (The left-out parts are
    * still part of the `Pic`’s data but not visible; only the part within the framing background image will
    * be shown when the `Pic` is rendered onscreen.) This version of `against` anchors this `Pic` at its
    * default [[anchor]] to an anchor in the background.
    * @param background  a `Pic` that serves as a background for this one and determines the size of
    *                    the visible part of the resulting image
    * @param atIts       the point within `background` that this `Pic`’s [[anchor]]
    *                    is positioned at; if unspecified, defaults to [[Center]]
    * @return the combined `Pic`
    * @see `onto`,
    *      which does the same but doesn’t leave out the “hanging” parts of the foreground image
    * @see `place`,
    *      which does the same but switches the order of the two `Pic`s */
  infix def against(background: Pic, atIts: Anchor = Center): Pic =
    this.against(background, anchor, atIts)


  /** Combines this `Pic` and the given one so that this `Pic` serves as a background and the other
    * `Pic` appears in front but not beyond this `Pic`’s borders. The visible part of the combined `Pic`
    * is the size of this background `Pic`. Some or even none of the other `Pic` is visible in front of
    * this background, depending on the relative positioning and dimensions of the two images; any parts
    * of the other `Pic` that don’t fit against this background are left out. (The left-out parts are
    * still part of this `Pic`’s data but not visible; only the part within this framing background
    * image will be shown when the `Pic` is rendered onscreen.) This version of `place` uses the given
    * [[o1.world.objects.Anchor Anchor]] instead of the other `Pic`s default `anchor` and places it at
    * a specific [[o1.world.Pos Pos]] in this background.
    * @param foreground  the `Pic` that will be placed against this background
    * @param its         the point within `foreground` that will be placed at the given coordinates
    * @param at          the point within this `Pic` that `foreground` will be placed at
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but switches the order of the two `Pic`s */
  def place(foreground: Pic, its: Anchor, at: Pos): Pic =
    val foregroundAnchor = its
    foreground.against(this, foregroundAnchor, at)

  /** Combines this `Pic` and the given one so that this `Pic` serves as a background and the other
    * `Pic` appears in front but not beyond this `Pic`’s borders. The visible part of the combined `Pic`
    * is the size of this background `Pic`. Some or even none of the other `Pic` is visible in front of
    * this background, depending on the relative positioning and dimensions of the two images; any parts
    * of the other `Pic` that don’t fit against this background are left out. (The left-out parts are
    * still part of this `Pic`’s data but not visible; only the part within this framing background
    * image will be shown when the `Pic` is rendered onscreen.) This version of `place` uses the given
    * [[o1.world.objects.Anchor Anchor]]s instead of the default `anchor` of either of the original `Pic`s.
    * @param foreground  the `Pic` that will be placed against this background
    * @param its         the point within `foreground` that will be placed at the given coordinates
    * @param atMy        the point within this `Pic` that `foreground` will be placed at
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but switches the order of the two `Pic`s */
  def place(foreground: Pic, its: Anchor, atMy: Anchor): Pic =
    foreground.against(this, its, atMy.internalPosWithin(this))

  /** Combines this `Pic` and the given one so that this `Pic` serves as a background and the other
    * `Pic` appears in front but not beyond this `Pic`’s borders. The visible part of the combined `Pic`
    * is the size of this background `Pic`. Some or even none of the other `Pic` is visible in front of
    * this background, depending on the relative positioning and dimensions of the two images; any parts
    * of the other `Pic` that don’t fit against this background are left out. (The left-out parts are still
    * part of this `Pic`’s data but not visible; only the part within this framing background image will
    * be shown when the `Pic` is rendered onscreen.) This version of `place` anchors the other `Pic` at
    * its default [[anchor]] to a specific [[o1.world.Pos Pos]] in this background.
    * @param foreground  the `Pic` that will be placed against this background
    * @param at          the point within this `Pic` that `foreground`’s anchor will be placed at
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but switches the order of the two `Pic`s */
  def place(foreground: Pic, at: Pos): Pic =
    this.place(foreground, foreground.anchor, at)

  /** Combines this `Pic` and the given one so that this `Pic` serves as a background and the other
    * `Pic` appears in front but not beyond this `Pic`’s borders. The visible part of the combined `Pic`
    * is the size of this background `Pic`. Some or even none of the other `Pic` is visible in front of
    * this background, depending on the relative positioning and dimensions of the two images; any parts
    * of the other `Pic` that don’t fit against this background are left out. (The left-out parts are still
    * part of this `Pic`’s data but not visible; only the part within this framing background image will
    * be shown when the `Pic` is rendered onscreen.) This version of `place` anchors the other `Pic` at
    * its default [[anchor]] to a specified anchor in this background.
    * @param foreground  the Pic` that will be placed against this background
    * @param atMy        the point within this `Pic` that `foreground`’s anchor will be placed at
    * @return the combined `Pic`
    * @see `against`,
    *      which does the same but switches the order of the two `Pic`s */
  def place(foreground: Pic, atMy: Anchor): Pic =
    this.place(foreground, foreground.anchor, atMy)

  /** Combines this `Pic` and the given ones so that this `Pic` serves as a background and the other
    * `Pic`s appear in front but not beyond this `Pic`’s borders. The visible part of the combined `Pic`
    * is the size of this background `Pic`. This is equivalent to calling `place(Pic,Pos)` repeatedly.
    * @param foregroundPics  tuples containing: (1) the `Pic`s that will be placed against this background;
    *                        (2) the points within this `Pic` where their default [[anchor]]s will be placed
    * @return the combined `Pic` */
  def place(foregroundPics: Iterable[(Pic, Pos)]): Pic =
    foregroundPics.foldLeft(this)( _.placePicPos(_) )

  private def placePicPos(picPos: (Pic, Pos)): Pic =
    this.place(picPos(0), picPos(1))

  /** Combines this `Pic` and the given ones so that this `Pic` serves as a background and the other
    * `Pic`s appear in front but not beyond this `Pic`’s borders. The visible part of the combined `Pic`
    * is the size of this background `Pic`. This is equivalent to calling `place(Pic,Pos)` repeatedly.
    * @param foregroundPics  tuples containing: (1) the `Pic`s that will be placed against this background;
    *                        (2) the points within this `Pic` where their default [[anchor]]s will be placed
    * @return the combined `Pic` */
  def place(foregroundPics: (Pic, Pos)*): Pic =
    this.place(foregroundPics)

  /** Combines this `Pic` and copies of the given one so that this `Pic` serves as a background and the copies
    * of the other `Pic`s appear in front but not beyond this `Pic`’s borders. The visible part of the combined
    * `Pic` is the size of this background `Pic`. This is equivalent to calling `place(Pic,Pos)`
    * repeatedly, passing in the same foreground image each time.
    * @param foregroundPic  the `Pic`s whose copies will be placed against this background
    * @param at             the points within this `Pic` where the default [[anchor]]s of the copies should be placed
    * @return the combined `Pic` */
  inline def placeCopies(foregroundPic: Pic, at: Iterable[Pos]): Pic =
    this.place(at.map( at => foregroundPic -> at ))


  /** Combines this `Pic` and the given one so that this `Pic` appears immediately to the left
    * the other `Pic`. The combined `Pic` will be large enough to fit both originals.
    * @param rightPic      the `Pic` that is to the right of this `Pic` in the combination
    * @param align         how to align the original `Pic`s vertically in case they have different heights;
    *                      this optional parameter defaults to [[Align.VMiddle]] (that is, centered alignment)
    * @param retainAnchor  `true` means that the [[anchor]] of the combination should be equally far from the
    *                      top left-hand corner as this `Pic`’s [[anchor]] is; `false` (the default) means that
    *                      the combination should follow the same general anchoring policy has this `Pic` (e.g.,
    *                      to anchor from its center). If unspecified, defaults to `false`.
    * @param gap           how many empty pixels to leave between the two `Pic`s; defaults to zero
    * @return the combined `Pic`
    * @see [[above]], [[below]], [[rightOf]], [[rowOf]] */
  infix def leftOf(rightPic: Pic, align: Align.Vertical = VMiddle,
                   retainAnchor: Boolean = false, gap: Int = 0): Pic =
    val newSmileContent = rightPic.smilePic.addToLeft(this.smilePic, gap, align)
    val newAnchor = if retainAnchor then this.absoluteAnchor else this.anchor
    Pic(newSmileContent, newAnchor, historyForCombinedPic)

  /** Combines this `Pic` and the given one so that this `Pic` appears immediately to the right
    * the other `Pic`. The combined `Pic` will be large enough to fit both originals.
    * @param leftPic       the `Pic` that is to the left of this `Pic` in the combination
    * @param align         how to align the original `Pic`s vertically in case they have different heights;
    *                      this optional parameter defaults to [[Align.VMiddle]] (that is, centered alignment)
    * @param retainAnchor  `true` means that the [[anchor]] of the combination should be equally far from the
    *                      top left-hand corner as this `Pic`’s [[anchor]] is; `false` (the default) means that
    *                      the combination should follow the same general anchoring policy has this `Pic` (e.g.,
    *                      to anchor from its center). If unspecified, defaults to `false`.
    * @param gap           how many empty pixels to leave between the two `Pic`s; defaults to zero
    * @return the combined `Pic`
    * @see [[above]], [[below]], [[leftOf]], [[rowOf]] */
  infix def rightOf(leftPic: Pic, align: Align.Vertical = VMiddle,
                    retainAnchor: Boolean = false, gap: Int = 0): Pic =
    val newSmileContent = leftPic.smilePic.addToRight(this.smilePic, gap, align)
    val newAnchor = if retainAnchor then this.absoluteAnchor else this.anchor
    Pic(newSmileContent, newAnchor, historyForCombinedPic)

  /** Combines this `Pic` and the given one so that this `Pic` appears immediately below the other `Pic`.
    * The combined `Pic` will be large enough to fit both originals.
    * @param abovePic      the `Pic` that is above this `Pic` in the combination
    * @param align         how to align the original `Pic`s horizontally in case they have different widths;
    *                      this optional parameter defaults to [[Align.HMiddle]] (that is, centered alignment)
    * @param retainAnchor  `true` means that the [[anchor]] of the combination should be equally far from the
    *                      top left-hand corner as this `Pic`’s [[anchor]] is; `false` (the default) means that
    *                      the combination should follow the same general anchoring policy has this `Pic` (e.g.,
    *                      to anchor from its center). If unspecified, defaults to `false`.
    * @param gap           how many empty pixels to leave between the two `Pic`s; defaults to zero
    * @return the combined `Pic`
    * @see [[above]], [[leftOf]], [[rightOf]], [[columnOf]] */
  infix def below(abovePic: Pic, align: Align.Horizontal = HMiddle,
                  retainAnchor: Boolean = false, gap: Int = 0): Pic =
    val newSmileContent = abovePic.smilePic.addToBottom(this.smilePic, gap, align)
    val newAnchor = if retainAnchor then this.absoluteAnchor else this.anchor
    Pic(newSmileContent, newAnchor, historyForCombinedPic)

  /** Combines this `Pic` and the given one so that this `Pic` appears immediately above the other `Pic`.
    * The combined `Pic` will be large enough to fit both originals.
    * @param lowerPic      the `Pic` that is below this `Pic` in the combination
    * @param align         how to align the original `Pic`s horizontally in case they have different widths;
    *                      this optional parameter defaults to [[Align.HMiddle]] (that is, centered alignment)
    * @param retainAnchor  `true` means that the [[anchor]] of the combination should be equally far from the
    *                      top left-hand corner as this `Pic`’s [[anchor]] is; `false` (the default) means that
    *                      the combination should follow the same general anchoring policy has this `Pic` (e.g.,
    *                      to anchor from its center). If unspecified, defaults to `false`.
    * @param gap           how many empty pixels to leave between the two `Pic`s; defaults to zero
    * @return the combined `Pic`
    * @see [[below]], [[leftOf]], [[rightOf]], [[columnOf]] */
  infix def above(lowerPic: Pic, align: Align.Horizontal = HMiddle,
                  retainAnchor: Boolean = false, gap: Int = 0): Pic =
    val newSmileContent = lowerPic.smilePic.addToTop(this.smilePic, gap, align)
    val newAnchor = if retainAnchor then this.absoluteAnchor else this.anchor
    Pic(newSmileContent, newAnchor, historyForCombinedPic)


  /** Combines copies of this `Pic` so that they appear in a horizontal row.
    * The combined `Pic` will be large enough to fit all the copies.
    * @param number  the number of copies in the row (e.g., `3` means the result has three identical images)
    * @see [[leftOf]], [[rightOf]], [[columnOf]], [[alternatingRow]] */
  def rowOf(number: Int): Pic =
    if number < 0 then
      throw IllegalArgumentException("Cannot form a row of negative size")
    else if number == 0 then
      Pic.empty
    else
      val smileContent = this.smilePic.replicateRightwards(
          numberOfReplicas = number - 1, gap = 0, VMiddle, SimpleIdentity)
      Pic(smileContent, this.anchor, historyForCombinedPic)

  /** Combines copies of this `Pic` so that they appear in a vertical column.
    * The combined `Pic` will be large enough to fit all the copies.
    * @param number  the number of copies in the column (e.g., `3` means the result has three identical images)
    * @see [[above]], [[below]], [[rowOf]], [[alternatingColumn]] */
  def columnOf(number: Int): Pic =
    if number < 0 then
      throw IllegalArgumentException("Cannot form a column of negative size")
    else if number == 0 then
      Pic.empty
    else
      val smileContent =
        this.smilePic.replicateDownwards(numberOfReplicas = number - 1, gap = 0, HMiddle, SimpleIdentity)
      Pic(smileContent, this.anchor, historyForCombinedPic)



  /** Combines copies of this `Pic` and another `Pic` so that they alternate in a horizontal row
    * (starting with this `Pic`). The combined `Pic` will be large enough to fit all the copies.
    * @param number  the total number of `Pic`s in the row (e.g., `6` means the result has three copies of each `Pic`)
    * @see [[leftOf]], [[rightOf]], [[rowOf]], [[alternatingColumn]] */
  def alternatingRow(another: Pic, number: Int): Pic =
    if number < 0 then
      throw IllegalArgumentException("Cannot form a row of negative size")
    else if number == 0 then
      Pic.empty
    else
      val smileContent =
        this.smilePic.alternateRightwardsWith(alternatives = Seq(another.smilePic), numberOfAlternations = number - 1, gap = 0, VMiddle)
      Pic(smileContent, this.anchor, historyForCombinedPic)


  /** Combines copies of this `Pic` and another `Pic` so that they alternate in a vertical column
    * (starting with this `Pic`). The combined `Pic` will be large enough to fit all the copies.
    * @param number  the total number of `Pic`s in the column (e.g., `6` means the result has three copies of each `Pic`)
    * @see [[above]], [[below]], [[columnOf]], [[alternatingRow]] */
  def alternatingColumn(another: Pic, number: Int): Pic =
    if number < 0 then
      throw IllegalArgumentException("Cannot form a column of negative size")
    else if number == 0 then
      Pic.empty
    else
      val smileContent =
        this.smilePic.alternateDownwardsWith(alternatives = Seq(another.smilePic), numberOfAlternations = number - 1, gap = 0, HMiddle)
      Pic(smileContent, this.anchor, historyForCombinedPic)


  /** Creates a new `Pic` by combining this `Pic` with the given one on a pixel-by-pixel basis.
    * For each pair of pixel coordinates in the two original `Pic`s, `combine` calls the given
    * function to compute the color of the corresponding pixel in the new `Pic`. The new `Pic`’s
    * width equals the lesser of the two originals’ widths; the same goes for height.
    * @param determinePixel  a function generates a pixel for the combination, given two pixel
    *                        colors from the same location in the originals
    * @return the new (bitmap) image obtained by calling `determinePixel` and putting together the outputs */
  def combine(another: Pic, determinePixel: (Color, Color) => Color): Pic =
    val smileContent = smilePic.mergePixelsWith(another.smilePic, determinePixel)
    Pic(smileContent, this.anchor, historyForCombinedPic)


  inline private def historyForCombinedPic: PicHistory =
    val creation = Create(method = "<combining method>", simpleDescription = "combined pic")
    this.historyDetails.copy(newCreationOp = creation, newProcessingOpList = List())

end Pic


// LATER:
// NOTE 2/2022: When Scala3doc updated to support links to overloaded methods,
//              fix docstrings here (e.g., various @sees).
