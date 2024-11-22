package o1.gui

import scala.swing.Component
import java.awt as AWT


/** Life-quality improvements for working with the Swing GUI library in Scala. */
object swingops:
  given CanEqual[Dimension, Dimension] = CanEqual.derived
  given CanEqual[Key.Value, Key.Value] = CanEqual.derived
  given CanEqual[Component, Component] = CanEqual.derived

  type Dimension = AWT.Dimension
  object Dimension:
    def apply(width: Int, height: Int) = new AWT.Dimension(width, height)

  type Rectangle = AWT.Rectangle
  object Rectangle:
    def apply(x: Int, y: Int, width: Int, height: Int) = new AWT.Rectangle(x, y, width, height)

  type Point = AWT.Point
  object Point:
    def apply(x: Int, y: Int) = new AWT.Point(x, y)

  type BufferedImage = AWT.image.BufferedImage
  object BufferedImage:
    export AWT.image.BufferedImage.*
    def apply(width: Int, height: Int, colorScheme: Int) =
      new AWT.image.BufferedImage(width, height, colorScheme)

  extension (self: scala.swing.Label)
    def iconOption = Option(self.icon)
    def iconOption_=(newIcon: Option[javax.swing.Icon]): Unit =
      self.icon = newIcon.orNull

  extension (self: swing.Window)
    def setTitleBarImage(newImage: Option[AWT.Image]): Unit =
      self.peer.setIconImage(newImage getOrElse EmptyImage.orNull)
    def setTitleBarPic(newPic: Option[o1.gui.Pic]): Unit =
      self.setTitleBarImage(newPic.map( _.toImage ))

  extension (self: AWT.image.BufferedImage)
    def dimensions = Dimension(self.getWidth, self.getHeight)
    def width: Int  = self.getWidth
    def height: Int = self.getHeight

  extension (self: AWT.Image)
    inline def draw(graphics: AWT.Graphics, x: Int, y: Int, width: Int, height: Int): Boolean =
      graphics.drawImage(self, x, y, width, height, swingops.Null)
    inline def draw(graphics: AWT.Graphics, x: Int, y: Int): Boolean =
      graphics.drawImage(self, x, y, swingops.Null)
    inline def draw(graphics: AWT.Graphics): Boolean =
      graphics.drawImage(self, 0, 0, swingops.Null)

  extension (self: AWT.Dimension)
    def withWidth(width: Int): Dimension   = Dimension(width, self.height)
    def withHeight(height: Int): Dimension = Dimension(self.width, height)
    def wider(amount: Int): Dimension  = self.withWidth(self.width + amount)
    def higher(amount: Int): Dimension = self.withHeight(self.height + amount)


  val Null: Null = None.orNull

end swingops

