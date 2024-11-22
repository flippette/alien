package o1.smile.infrastructure

import o1.smile.Settings
import o1.smile.Settings.DefaultBackgroundColor
import o1.gui.Color
import o1.smile.modeling.Transform

import java.awt.{Graphics2D, RenderingHints, Component, Graphics}
import java.awt.geom.{AffineTransform, Rectangle2D}
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.Icon
import o1.gui.swingops
import o1.gui.swingops.draw

object JVMBufferAdapter:
  val Empty: JVMBufferAdapter = JVMBufferAdapter(1, 1)

/** Adapter for managing and manipulating a `BufferedImage`. This class provides methods for common
  * image processing tasks such as copying, scaling, and transforming.
  * @param buffer  the underlying `BufferedImage` instance */
class JVMBufferAdapter(private val buffer: BufferedImage) extends BufferAdapter[BufferedImage]:

  /** Creates a `JVMBufferAdapter` instance with specified width and height, initializing a new `BufferedImage`.
    * @param width   the width of the image
    * @param height  the height of the image */
  def this(width: Int, height: Int) =
    this(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))

  val width: Int  = buffer.getWidth
  val height: Int = buffer.getHeight

  // used for image scaling operations
  private def ScalingMethod: Int = Settings.BufferScalingMethod.value

  // used for image transformation operations
  private def TransformMethod: Int = Settings.BufferTransformMethod.value

  private def transformationToAWT(transformation: Transform): AffineTransform =
    val awtTransform = AffineTransform()
    awtTransform.setTransform(transformation.alpha, transformation.delta, transformation.gamma,
                              transformation.beta,  transformation.tauX,  transformation.tauY)
    awtTransform

  def deepCopy: JVMBufferAdapter =
    val newBuffer = JVMBufferAdapter(width, height)
    newBuffer.withGraphics2D( g => buffer.draw(g) )
    newBuffer

  /** Provides access to the underlying `BufferedImage`.
    * @return the underlying `BufferedImage` */
  def get: BufferedImage = buffer

  def imageData: Seq[Color] =
    val raster  = buffer.getRaster
    val (red, green, blue, alpha) = (0, 1, 2, 3)
    val colors = for
      y <- 0 until height
      x <- 0 until width
    yield
      val pixel = raster.getPixel(x, y, Array.ofDim[Int](4))
      Color(pixel(red), pixel(green), pixel(blue), pixel(alpha))
    colors

  /** Converts the buffer into a Swing `Icon`.
    * @return an `Icon` representing the buffer */
  def toSwingIcon: Icon = new Icon:
    def paintIcon(target: Component, graphics: Graphics, x: Int, y: Int): Unit =
      graphics.drawImage(buffer, x, y, target)
    def getIconWidth: Int  = width
    def getIconHeight: Int = height
  end toSwingIcon

  def pixelColor(x: Int, y: Int): Color =
    val argb = this.buffer.getRGB(x, y)
    Color(argb)

  def setRGBA(x: Int, y: Int, color: Color): Unit =
    buffer.setRGB(x, y, color.toARGBInt)

  def scaleTo(targetWidth: Double, targetHeight: Double): JVMBufferAdapter =
    val isNearestNeighbor = Settings.BufferScalingMethod == Settings.ScalingMethod.NearestNeighbor
    val newWidth  = targetWidth.toInt.abs
    val newHeight = targetHeight.toInt.abs
    val newBuffer = JVMBufferAdapter(newWidth, newHeight)
    val g = newBuffer.buffer.createGraphics()

    // Flip the image if necessary.
    if targetWidth < 0 || targetHeight < 0 then
      val tx = AffineTransform.getScaleInstance(if targetWidth < 0 then -1 else 1,
                                                if targetHeight < 0 then -1 else 1)
      if targetWidth < 0 then tx.translate(-newWidth, 0)
      if targetHeight < 0 then tx.translate(0, -newHeight)
      g.transform(tx)

    if isNearestNeighbor then
      g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      this.buffer.draw(g, x = 0, y = 0, newWidth, newHeight)
    else
      val image = this.buffer.getScaledInstance(newWidth, newHeight, ScalingMethod)
      image.draw(g)
    g.dispose()

    newBuffer
  end scaleTo

  def setColorsFromSeq(colors: Seq[Color]): Unit =
    def colorArray = new Array[Int](colors.length)
    val reds       = colorArray
    val greens     = colorArray
    val blues      = colorArray
    val opacities  = colorArray

    for (color, index) <- colors.zipWithIndex do
      if Option(color).isDefined then
        reds(index)      = color.red
        greens(index)    = color.green
        blues(index)     = color.blue
        opacities(index) = color.opacity

    val raster     = buffer.getRaster
    val setSamples = raster.setSamples(0, 0, width, height, _: Int, _: Array[Int])
    setSamples(0, reds)
    setSamples(1, greens)
    setSamples(2, blues)
    setSamples(3, opacities)

    buffer.setData(raster)
  end setColorsFromSeq

  def copyPortionXYWH(topLeftX: Double, topLeftY: Double, width: Double, height: Double): JVMBufferAdapter =
    if width <= 0 || height <= 0 then
      JVMBufferAdapter.Empty
    else
      val flooredWidth: Int  = width.floor.toInt
      val flooredHeight: Int = height.floor.toInt
      val sourceBufferArea =
        buffer.getSubimage(topLeftX.floor.toInt, topLeftY.floor.toInt, flooredWidth, flooredHeight)
      new JVMBufferAdapter(sourceBufferArea)
  end copyPortionXYWH

  private[infrastructure] def withGraphics2D[ResultType](workUnit: Graphics2D => ResultType): ResultType =
    var gr2D: Graphics2D | Null = swingops.Null
    var memorizedThrowable: Throwable | Null = swingops.Null
    try
      gr2D = buffer.createGraphics()
      setDefaultGraphics2DProperties(gr2D)
      workUnit(gr2D)
    catch
      case caughtThrowable: Throwable =>
        memorizedThrowable = caughtThrowable
        throw caughtThrowable
    finally
      if Option(gr2D).isDefined then
        try
          gr2D.dispose()
        catch
          case caughtThrowable: Throwable if Option(memorizedThrowable).isDefined =>
            memorizedThrowable.addSuppressed(caughtThrowable)

  private inline final def setDefaultGraphics2DProperties(g: Graphics2D): Unit =
    val antialiasingState = if Settings.DrawingIsAntiAliased then RenderingHints.VALUE_ANTIALIAS_ON
                                                             else RenderingHints.VALUE_ANTIALIAS_OFF
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, antialiasingState)
    val textAntialiasingState = if Settings.DrawingIsAntiAliased then RenderingHints.VALUE_TEXT_ANTIALIAS_ON
                                                                 else RenderingHints.VALUE_TEXT_ANTIALIAS_OFF
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, textAntialiasingState)
  end setDefaultGraphics2DProperties

  /** Creates a new `JVMBufferAdapter` instance that is a transformed version of the current buffer.
    * The transformation is applied using an `AffineTransformation`. The canvas can optionally be
    * resized based on the transformation.
    * @param transformation   the `AffineTransformation` to apply to the image
    * @param backgroundColor  the background color to use when clearing the canvas if resizing is necessary;
    *                         defaults to `DefaultBackgroundColor`.
    * @return a new `JVMBufferAdapter` instance containing the transformed image */
  def transform(transformation: Transform, backgroundColor: Color = DefaultBackgroundColor): JVMBufferAdapter =

    val globalInterpolationMethod = TransformMethod

    val lowLevelTransformation = transformationToAWT(transformation)
    val transformedContentBoundaries: Rectangle2D =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod).getBounds2D(buffer)

    val (offsetLeft, offsetTop, offsetRight, offsetBottom) =
      (-transformedContentBoundaries.getMinX,
       -transformedContentBoundaries.getMinY,
       transformedContentBoundaries.getMaxX - width,
       transformedContentBoundaries.getMaxY - height)

    val (resultingImageWidth, resultingImageHeight) =
      (Math.floor(width.toDouble + offsetLeft + offsetRight).toInt,
       Math.floor(height.toDouble + offsetTop + offsetBottom).toInt)

    if offsetTop > 0 || offsetLeft > 0 then
      val translationToBringTheRotatedBitmapFullyVisible =
        AffineTransform.getTranslateInstance(offsetLeft, offsetTop)
      lowLevelTransformation.preConcatenate(translationToBringTheRotatedBitmapFullyVisible)

    val finalTransformOperation =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod)

    if resultingImageWidth == 0 || resultingImageHeight == 0 then JVMBufferAdapter.Empty
    else
      val resultingBuffer = JVMBufferAdapter(resultingImageWidth, resultingImageHeight)
      new DrawingSurface(resultingBuffer).clearUsing(backgroundColor, true)
      finalTransformOperation.filter(buffer, resultingBuffer.get)
      resultingBuffer
  end transform

  /** Saves a `BufferedImage` to a specified path. The image is saved in PNG format.
    * @param path  the filesystem path where the image should be saved
    * @return `true` if the image was saved successfully, `false` otherwise */
  def saveToPath(path: String): Boolean =
    ImageIO.write(buffer, "png", new File(path))

end JVMBufferAdapter

