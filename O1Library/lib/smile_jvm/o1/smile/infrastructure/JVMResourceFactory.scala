package o1.smile.infrastructure

import java.awt.image.BufferedImage
import java.io.{File, IOException}
import java.net.URI
import javax.imageio.ImageIO
import javax.imageio.stream.ImageInputStream
import o1.gui.swingops.draw

/** Provides functionalities for creating and saving images from various sources.
  */
object JVMResourceFactory extends ResourceFactory[BufferedImage]:
  /** Creates a `BufferAdapter` from an image located at a given path. This method can load images
    * from local file paths, resources within the application's classpath, and URIs, including those
    * on the internet. If the path is a URL to an image on the internet, the method will attempt to
    * download and load the image.
    *
    * @param path
    *   The path to the image. It can be a local filesystem path, a resource path, or a URL to an
    *   image on the internet.
    * @return
    *   A `BufferAdapter` containing the image loaded from the specified path.
    */
  def bufferAdapterFromPath(path: String): JVMBufferAdapter =
    val image = bufferedImageFromPath(path)
    bufferFromImage(image)

  private def imageInputStreamFromPath(path: String): ImageInputStream =
    val file = new File(path)
    val resource = getClass.getClassLoader.getResource(path)
    try
      if file.exists then
        ImageIO.createImageInputStream(file)
      else if Option(resource).isDefined then
        ImageIO.createImageInputStream(resource.openStream)
      else
        ImageIO.createImageInputStream(URI.create(path).toURL.openStream)
    catch
      case _ =>
        throw new IOException(s"Image at path '$path' not found")

  private def bufferedImageFromPath(path: String): BufferedImage =
    val file = new File(path)
    val resource = getClass.getClassLoader.getResource(path)
    try
      if file.exists then
        ImageIO.read(file)
      else if Option(resource).isDefined then
        ImageIO.read(resource)
      else
        ImageIO.read(URI.create(path).toURL)
    catch
      case _ =>
        throw new IOException(s"Image at path '$path' not found")

  /** Converts a `BufferedImage` to a `BufferAdapter`. This is a utility method used internally to
    * wrap a `BufferedImage` into the custom `BufferAdapter` class.
    *
    * @param image
    *   The `BufferedImage` to convert.
    * @return
    *   A `BufferAdapter` containing the provided image.
    */
  private def bufferFromImage(image: BufferedImage): JVMBufferAdapter =
    val bufferedImage = JVMBufferAdapter(image.getWidth, image.getHeight)
    image.draw(bufferedImage.get.createGraphics())
    bufferedImage
