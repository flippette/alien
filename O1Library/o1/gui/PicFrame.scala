package o1.gui

import o1.util.*
import o1.gui.swingops.*
import o1.gui.swingops.given

import java.awt.Color.BLACK
import scala.swing.event.{KeyPressed, MousePressed}
import scala.swing.{Frame, Graphics2D, Panel, Swing}
import scala.language.adhocExtensions // enable extension of Swing classes

import PicFrame.*

private[gui] object PicFrame:

  private val framesForPics = scala.collection.mutable.Map[Pic, PicFrame]()
  private def visibleFrameCount = this.framesForPics.size

  def show(pic: Pic, background: Color, border: Int): Unit =
    def newFrame(pic: Pic) = PicFrame(pic, background, border)
    val frame = this.framesForPics.getOrElseUpdate(pic, newFrame(pic))
    if !frame.visible then
      frame.pack()
      frame.visible = true
    frame.backgroundColor = background
    frame.borderWidth = border
    frame.bringToFocusInFront()

  def hide(pic: Pic) =
    for frame <- framesForPics.remove(pic) do
      frame.dispose()

  def hideAll() =
    framesForPics.keys.foreach(hide)

end PicFrame


private final class PicFrame(val pic: Pic, private var bgColor: Color, private var borders: Int) extends Frame, Tooltips.Fast:
  this.setTitleBarImage(O1LogoImage)
  this.peer.setUndecorated(true)
  this.resizable = false
  this.location = Point(150 + visibleFrameCount * 10, 150 + visibleFrameCount * 10)

  private object panel extends Panel:
    val image: BufferedImage = pic.toImage
    val dims: Dimension = Dimension(pic.width.floor.toInt, pic.height.floor.toInt)
    preferredSize = dims.wider(borderWidth * 2).higher(borderWidth * 2)
    tooltip = "Click or press Esc to close."

    override def paintComponent(myGraphics: Graphics2D): Unit =
      super.paintComponent(myGraphics)
      this.image.draw(myGraphics, x = borderWidth, y = borderWidth)
  end panel

  def bringToFocusInFront() =
    this.peer.setAlwaysOnTop(true)
    this.peer.setState(java.awt.Frame.ICONIFIED) // This setState trickery helps (on Win) for unknown reason. Could be cleaned up later.
    this.peer.setState(java.awt.Frame.NORMAL)
    this.peer.toFront()
    this.peer.requestFocus()
    this.contents.headOption.foreach( _.requestFocus() )
    this.peer.setAlwaysOnTop(false)

  private def hide() =
    Pic.hide(this.pic)

  override def closeOperation() =
    this.hide()

  this.backgroundColor = bgColor
  this.borderWidth = borders
  this.contents = this.panel
  this.listenTo(panel.mouse.clicks, panel.keys)
  this.reactions += {
    case KeyPressed(_, Key.Escape, _, _) => this.hide()
    case press: MousePressed             => this.hide()
  }

  def borderWidth: Int = this.borders
  def borderWidth_=(newWidth: Int): Unit =
    this.borders = newWidth
    this.panel.border = Swing.LineBorder(BLACK, this.borderWidth)
    this.panel.repaint()

  def backgroundColor: Color = this.bgColor
  def backgroundColor_=(color: Color): Unit =
    this.bgColor = color
    this.panel.background = color.toSwingColor
    this.panel.repaint()


end PicFrame

