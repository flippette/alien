/** This package contains tools for building simple GUIs. The toolkit is particularly well suited
  * to constructing simple GUIs that display information as 2D images and/or geometric shapes.
  * It is not designed for demanding graphics that call for high efficiency. Some of the tools in
  * this package are built on the Swing GUI library, and the two libraries can be used in combination.
  *
  * Some of the types in this package have aliases in the top-level package [[o1]], so they are
  * accessible to students simply via `import o1.*`. Some of the package contents are not available
  * in the top-level package or included in this documentation.
  *
  * **Note:** One of this package’s key components (views) comes in multiple varieties, which
  * are defined in the subpackages [[o1.gui.mutable]] and [[o1.gui.immutable]] and not listed below.
  * The `View` that is most commonly used in O1 (and aliased as `o1.View` in the top-level package) is
  * [[o1.gui.mutable.ViewFrame]]. */
package o1.gui

import o1.util.{Program, firstTimeOnly}
import o1.world as W
import java.awt as AWT
import o1.gui.swingops.*
import o1.gui.swingops.given

import scala.swing.event as SwEv
import scala.swing.SimpleSwingApplication
import scala.language.adhocExtensions // enable extension of Swing classes


// Setting this to `true` disables GUI windows created by commands such as `View.start` and `Pic.show`.
// (This is useful for the automatic testing and assessment of certain student programs.
// Generally, there should be no need to touch this setting, which defaults to `false`.)
var isInTestMode = false


/** The `Key` type represents keys on the keyboard; it is an alias for the corresponding type
  * in Scala’s Swing GUI library */
type Key = SwEv.Key.Value
/** The `Key` type represents keys on the keyboard; it is an alias for the corresponding type
  * in Scala’s Swing GUI library */
val Key = SwEv.Key


/** The `Pos` type represents locations on a two-dimensional plane; it is an alias for the
  * [[o1.world.Pos class of the same name in `o1.world`]]. */
type Pos = W.Pos
/** The `Pos` type represents locations on a two-dimensional plane; it is an alias for the
  * [[o1.world.Pos class of the same name in `o1.world`]]. */
val Pos = W.Pos



/** The `Bounds` type represents rectangular boundaries on a two-dimensional plane; it is an
  * alias for the [[o1.world.Bounds class of the same name in `o1.world`]]. */
type Bounds = W.Bounds
/** The `Bounds` type represents rectangular boundaries on a two-dimensional plane; it is an
  * alias for the [[o1.world.Bounds class of the same name in `o1.world`]]. */
val Bounds = W.Bounds



/** The `Anchor` type represents anchoring points of two-dimensional elements (such as [[Pic]]s) within
  * other such elements; it is an alias for the [[o1.world.objects.Anchor type of the same name in `o1.world.objects`]]. */
type Anchor = W.objects.Anchor
/** The `Anchor` type represents anchoring points of two-dimensional elements (such as [[Pic]]s) within
  * other such elements; it is an alias for the [[o1.world.objects.Anchor type of the same name in `o1.world.objects`]]. */
val Anchor = W.objects.Anchor
/** A supertype for two-dimensional elements that have an anchoring point; this is an alias for the
  * [[o1.world.objects.HasAnchor trait of the same name in `o1.world.objects`]]. */
type HasAnchor = W.objects.HasAnchor



/** Give this convenience trait to a Swing GUI frame to make it terminate the application when closed. */
trait TerminatesOnClose extends swing.Window:
  /** Call `closeOperation` on the superclass, then terminates the entire application.
    * (Skips termination if it seems the program is running in the REPL.) */
  override def closeOperation(): Unit =
    super.closeOperation()
    if !Program.isRunningInScalaREPL then
      System.exit(0)


/** Giving this convenience trait to a Swing GUI frame will place the frame at (120, 120)
  * and set it as unresizeable. */
trait DefaultFrameSettings extends swing.Frame:
  this.resizable = false
  this.location = Point(120, 120)


/** Inherit this class to obtain a Swing GUI frame that has the [[DefaultFrameSettings]]
  * and the given title. */
open class SimpleFrame(initialTitle: String) extends swing.Frame, DefaultFrameSettings:
  this.title = initialTitle


/** Give this convenience trait to a Swing GUI frame to make it close when the *Escape* key
  * is pressed. */
trait Escapable extends swing.Frame:
  import javax.swing.KeyStroke
  val escape: Int = AWT.event.KeyEvent.VK_ESCAPE
  val escapeStroke: KeyStroke = javax.swing.KeyStroke.getKeyStroke(escape, 0)
  val wholeWindowScope: Int = javax.swing.JComponent.WHEN_IN_FOCUSED_WINDOW
  this.peer.getRootPane.registerKeyboardAction( _ => this.closeOperation() , escapeStroke, wholeWindowScope)


private[o1] abstract class SimpleButton(title: String) extends swing.Button(title):
  listenTo(this)
  def onClick(): Unit
  this.reactions += { case _: swing.event.ButtonClicked => this.onClick() }


private[gui] val O1LogoPic: Option[o1.gui.Pic]  = o1.gui.Pic.asOption("o1.png")
private[gui] val O1LogoImage: Option[AWT.Image] = O1LogoPic.map( _.toImage )
private val EmptyImage: Option[AWT.Image]       = o1.gui.Pic.asOption("nothing.png").map( _.toImage )


private[o1] object Tooltips:
  import javax.swing.ToolTipManager
  trait Everpresent:
    ToolTipManager.sharedInstance.setInitialDelay(0)
    ToolTipManager.sharedInstance.setDismissDelay(Int.MaxValue)
  trait Fast:
    ToolTipManager.sharedInstance.setInitialDelay(150)


private[gui] def warn(descriptor: Any): Unit =
  System.err.println("o1.gui warning: " + descriptor)

private[gui] def warn(descriptor: Any, cause: Throwable): Unit =
  warn(descriptor)
  cause.printStackTrace(System.err)


private[o1] trait O1WindowDefaults extends swing.Window, O1SwingDefaults:
  this.setTitleBarImage(O1LogoImage)

private[o1] trait O1AppDefaults extends SimpleSwingApplication, O1SwingDefaults:
  override def startup(args: Array[String]) =
    val frame = this.top
    frame.setTitleBarImage(O1LogoImage)
    if frame.size == Dimension(0,0) then
      frame.pack()
    frame.visible = true

private[o1] trait O1SwingDefaults:
  self: (SimpleSwingApplication | swing.Window | App) =>
  O1SwingDefaults()

private[o1] object O1SwingDefaults extends Tooltips.Fast:
  import javax.swing.UIManager

  def apply() = this.makeItSo()

  private val makeItSo =
    firstTimeOnly( UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName) )

end O1SwingDefaults



private[gui] object compat:
  import o1.world.objects.Anchor
  import o1.smile.modeling.Bounds as SmileBounds
  import o1.smile.pictures.Viewport as SmileViewport
  import o1.smile.pictures.PictureElement as SmilePictureElement
  import o1.smile.pictures.Picture as SmilePicture
  import o1.world.Pos
  import o1.gui.PicHistory.op.Process

  extension (self: SmileBounds)
    def toO1Bounds: o1.world.Bounds =
      o1.world.Bounds(self.upperLeftCorner.x, self.upperLeftCorner.y,
                      self.width.inPixels, self.height.inPixels)

  extension (self: SmileViewport)
    def toO1Viewport: Viewport =
      Option(self).map( vp => Viewport(vp.boundary.toO1Bounds) ) getOrElse Viewport.NotSet

  extension (self: SmilePictureElement)
    inline def toO1Pic(producedBy: Process)(using pic: Pic) =
      Pic(self, pic.anchor, producedBy :: pic.historyDetails)
    inline def toO1Pic(newAnchor: Anchor, producedBy: Process)(using pic: Pic) =
      Pic(self, newAnchor, producedBy :: pic.historyDetails)

  extension (self: SmilePicture)
    inline def toO1Pic(producedBy: Process)(using pic: Pic) =
      Pic(self, pic.anchor, producedBy :: pic.historyDetails)
    inline def toO1Pic(newAnchor: Anchor, producedBy: Process)(using pic: Pic) =
      Pic(self, newAnchor, producedBy :: pic.historyDetails)

end compat

