package o1.gui.viewimpl

import o1.gui.*
import o1.gui.View.RefreshPolicy
import o1.gui.event.*
import o1.sound.sampled
import o1.util.{Program, pass}
import o1.util.nice.number.*

import scala.swing.Frame
import o1.gui.swingops.*

abstract class ViewFrameImpl[Model <: Matchable](private[gui] val initialState: Model,
         tickRate: Double, protected val title: String, initialDelay: Int,
         val terminateOnClose: Boolean, val closeWhenDone: Boolean,
         val refreshPolicy: RefreshPolicy) extends View.Controls[Model]:
  viewFrame =>

  private val component = new ViewComponentImpl(initialState, tickRate, initialDelay, refreshPolicy) with DelegateToContainingFrame
  private lazy val modelClassName = initialState.getClass.getSimpleName
  private var frameIcon = O1LogoPic
  private[gui] def ticksPerSecond = this.component.ticksPerSecond

  /** Causes an effect when the view’s GUI window is closed for any reason. By default, this method does nothing. */
  def onClose(): Unit = pass


  private[o1] lazy val swingFrame: Option[Frame] =
    if o1.gui.isInTestMode then None else Some(this.makeSwingFrame)

  private def makeSwingFrame = new SimpleFrame(this.title) with Escapable with O1WindowDefaults:
    contents = component
    component.requestFocusInWindow()
    override def closeOperation(): Unit = viewFrame.stop()


  /** whether this view’s GUI frame is visible onscreen */
  final def visible: Boolean = this.swingFrame.exists( _.visible )
  /** Sets whether this view’s GUI frame is visible onscreen. */
  final def visible_=(desiredVisibility: Boolean): Unit =
    this.swingFrame.foreach( _.visible = desiredVisibility )


  /** The icon to be displayed in the title bar of this view’s GUI frame. */
  def icon: Option[Pic] = this.frameIcon
  /** Sets the icon to be displayed in the title bar of this view’s GUI frame.
    * @param icon  a picture to be used as the icon; if `None`, en empty icon image will be displayed */
  final def icon_=(icon: Option[Pic]): Unit =
    this.frameIcon = icon
    this.swingFrame.foreach( _.setTitleBarPic(icon) )
  /** Sets the icon to be displayed in the title bar of this view’s GUI frame.
    * @param icon  a picture to be used as the icon */
  final def icon_=(icon: Pic): Unit =
    this.icon = Some(icon)


  /** Sets a new tick rate for the view, replacing any previously set by the constructor or this method. */
  final def adjustSpeed(newTickRate: Double): Unit =
    this.component.adjustSpeed(newTickRate)


  /** Starts the view: loads the model in the GUI window, makes the window visible
    * oncreen, and starts the clock. Cf. [[simulate]]. */
  final def start(): Unit =
    if o1.gui.isInTestMode then
      println("Not starting GUI because in text-based test mode.")
    else
      this.component.start()
      this.swingFrame.foreach( _.pack() )
      this.optimizeLocationOnScreen()
      this.visible = true


  /** Moves the frame up from its default position in case its bottom edge would not
    * be visible otherwise. (This helps with “tall” windows on */
  private inline def optimizeLocationOnScreen() =
    for frame <- this.swingFrame do
      val bounds = frame.bounds
      val frameBottom = bounds.y + bounds.height
      val desiredBottomMargin = 35
      val screenBottom = java.awt.Toolkit.getDefaultToolkit.getScreenSize.height
      if frameBottom > screenBottom - desiredBottomMargin then
        val desiredY = screenBottom - bounds.height - desiredBottomMargin
        frame.location = Point(bounds.x, desiredY atLeast 0)




  /** Runs the view as if by calling [[start]] except that it runs “headless”, with no
    * actual GUI window visible and independently of a real-time clock. A number of
    * simulated clock ticks are immediately sent to the view; this continues until
    * either the view determines it is done or a predetermined maximum number of ticks
    * has been reached.
    * @param tickLimit  the maximum number of ticks to simulate; `Int.MaxValue`
    *                   (which is the default) means there is no such limit */
  final def simulate(tickLimit: Int = Int.MaxValue): Unit =
    this.component.simulate(tickLimit)


  /** Stops the view: stops the clock, stops listening to events, and disposes of the GUI window.
    * A stopped view cannot be restarted. */
  final def stop(): Unit =
    this.component.stop()
    this.onClose()
    if this.terminateOnClose && !Program.isRunningInScalaREPL then
      System.exit(0)

    this.swingFrame.foreach( _.visible = false )

  /** Programmatically requests an update to the graphics of the view (even though no
    * clock tick or triggering GUI event occurred). */
  final def refresh(): Unit =
    this.component.refresh()

  /** Closes the view: stops it (as per [[stop]]), does any [[onClose]] effects, hides the GUI
    * window, and possibly terminates the entire application (as per the constructor parameter). */
  final def close(): Unit =
    this.stop()


  /** the tooltip text to be displayed while the mouse hovers on the view */
  final def tooltip: String = this.component.tooltip
  /** Sets the tooltip text to be displayed while the mouse hovers on the view. */
  final def tooltip_=(newText: String): Unit =
    this.component.tooltip = newText


  /** Returns a brief textual description of the view. */
  override def toString: String =
    if this.title.isEmpty then s"view of $modelClassName" else s"""view "$title" of $modelClassName"""


  private trait DelegateToContainingFrame extends ViewComponentImpl[Model]:

    def makePic(state: Model): Pic = viewFrame.makePic(state)
    override def onStop(): Unit = viewFrame.onStop()
    override def isDone(state: Model): Boolean = viewFrame.isDone(state)
    override def isPaused: Boolean = viewFrame.isPaused
    override def sound(state: Model): Option[sampled.Sound] = viewFrame.sound(state)
    def onTick(previousState: Model): Model = viewFrame.onTick(previousState)
    def onTick(previousState: Model, time: Long): Model = viewFrame.onTick(previousState, time)
    def onMouseMove(state: Model, position: Pos): Model = viewFrame.onMouseMove(state, position)
    def onMouseDrag(state: Model, position: Pos): Model = viewFrame.onMouseDrag(state, position)
    def onWheel(state: Model, rotation: Int): Model = viewFrame.onWheel(state, rotation)
    def onClick(state: Model, position: Pos): Model = viewFrame.onClick(state, position)
    def onMouseDown(state: Model, position: Pos): Model = viewFrame.onMouseDown(state, position)
    def onMouseUp(state: Model, position: Pos): Model = viewFrame.onMouseUp(state, position)
    def onKeyDown(state: Model, key: Key): Model = viewFrame.onKeyDown(state, key)
    def onKeyUp(state: Model, key: Key): Model = viewFrame.onKeyUp(state, key)
    def onType(state: Model, character: Char): Model = viewFrame.onType(state, character)
    def onMouseMove(state: Model, event: MouseMoved): Model = viewFrame.onMouseMove(state, event)
    def onMouseDrag(state: Model, event: MouseDragged): Model = viewFrame.onMouseDrag(state, event)
    def onMouseEnter(state: Model, event: MouseEntered): Model = viewFrame.onMouseEnter(state, event)
    def onMouseExit(state: Model, event: MouseExited): Model = viewFrame.onMouseExit(state, event)
    def onMouseUp(state: Model, event: MouseReleased): Model = viewFrame.onMouseUp(state, event)
    def onMouseDown(state: Model, event: MousePressed): Model = viewFrame.onMouseDown(state, event)
    def onWheel(state: Model, event: MouseWheelMoved): Model = viewFrame.onWheel(state, event)
    def onClick(state: Model, event: MouseClicked): Model = viewFrame.onClick(state, event)
    def onKeyDown(state: Model, event: KeyPressed): Model = viewFrame.onKeyDown(state, event)
    def onKeyUp(state: Model, event: KeyReleased): Model = viewFrame.onKeyUp(state, event)
    def onType(state: Model, event: KeyTyped): Model = viewFrame.onType(state, event)

    private[gui] final override def onDone(): Unit =
      viewFrame.onDone()
      super.onDone()
      if viewFrame.closeWhenDone then
        viewFrame.close()
    end onDone

    override private[gui] def maxLifeSpan: Long = viewFrame.maxLifeSpan
    override def toString = viewFrame.toString

  end DelegateToContainingFrame

end ViewFrameImpl

