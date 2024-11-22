package o1.gui.viewimpl

import o1.gui.*
import o1.gui.View.*
import o1.gui.event.*
import o1.gui.swingops.draw
import o1.sound.sampled
import o1.util.{atMost as _, *}
import o1.util.nice.number.*
import o1.gui.colors.Black

import viewimpl.events.*
import Ticker.*

import scala.swing.{Component, Graphics2D}

import akka.actor.{Actor,ActorSystem,Props}
import com.typesafe.config.ConfigFactory

import scala.annotation.tailrec


// LATER: separate concerns (GUI component vs. events)
abstract class ViewComponentImpl[Model <: Matchable](private[gui] val initialState: Model, tickRate: Double,
                                                     private val initialDelay: Int, val refreshPolicy: RefreshPolicy)
  extends Component, View.Controls[Model]:
  view =>

  private[gui] var ticksPerSecond = tickRate


  /** Sets a new tick rate for the view, replacing any previously set by the constructor or this method. */
  final def adjustSpeed(newTickRate: Double): Unit =
    this.ticksPerSecond = newTickRate
    this.ticker.adjust(this.tickDelay(newTickRate))

  this.listenTo(this.mouse.clicks, this.mouse.moves, this.mouse.wheel, this.keys)
  this.reactions += { case event: InputEvent => this.events ! GUIMessage(event) }

  private var ticksSent = 0L
  inline private def sendTick() =
    if !this.isPaused then
      this.ticksSent += 1
      if ticksSent <= this.maxLifeSpan then
        this.events ! Tick(this.ticksSent)
      else
        this.stop()


  private      val initialTickRate = tickRate
  private lazy val eventSystem     = ActorSystem("ViewEventSystem", ConfigFactory.load("conf/akka.conf"))
  private lazy val events          = eventSystem.actorOf(Props(ModelState(initialState)).withDispatcher("view-mailbox"), name = "modelstate")
  private lazy val ticker          = Ticker(this.initialDelay, this.tickDelay(this.initialTickRate))( timedBlock = this.sendTick() )

  private def tickDelay(tickRate: Double) = TickState.fromTickRate(tickRate atMost TicksPerSecondMax)
  private var hasStopped = false
  private var latestComputed = Latest(initialState, None)
  private lazy val modelClassName = initialState.getClass.getSimpleName


  /** Starts the view: loads the model into the component and starts the clock. Cf. [[simulate]]. */
  final def start(): Unit =
    if this.hasStopped then
      warn("Restarting a stopped view is not supported at the present time.")
    else if o1.gui.isInTestMode then
      println("Not starting GUI because in text-based test mode.")
    else
      this.loadModel(this.initialState)
      this.requestFocusInWindow()
      this.ticker.start()


  /** Runs the view as if by calling [[start]] except that it runs “headless”, without
    * expectation of being visible in a GUI and independently of a real-time clock.
    * A number of simulated clock ticks are immediately sent to the view; this continues
    * until either the view determines it is done or a predetermined maximum number of
    * ticks has been reached.
    * @param tickLimit  the maximum number of ticks to simulate; `Int.MaxValue`
    *                   (which is the default) means there is no such limit */
  final def simulate(tickLimit: Int = Int.MaxValue): Unit =
    class TickHandler:
      var isDisabled = false
      def apply(oldState: Model, func: Model=>Model) =
        if this.isDisabled then
          oldState
        else
          Try(func(oldState)) match
            case Success(newState) => newState
            case Failure(NoHandlerDefined)  =>
              this.isDisabled = true
              oldState
            case Failure(crashInClientCode) =>
              this.isDisabled = true
              warn("An error occurred in event-handler code. Disabling the handler. Here is a detailed report:")
              crashInClientCode.printStackTrace(System.err)
              oldState
    end TickHandler

    events // force eval; later: find nicer solution
    val (simpleHandler, fullHandler) = (TickHandler(), TickHandler())
    val noHandlerWarning = firstTimeOnly( warn("Neither tick handler used by the simulation is enabled.") )
    val unlimitedTicks = tickLimit == Int.MaxValue
    var tickCount = 0L
    var mayContinue = true
    @tailrec def runFrom(currentState: Model): Unit =
      if mayContinue && !view.isDone(currentState) then
        tickCount += 1
        val afterFirst = simpleHandler(currentState, view.onTick)
        val afterBoth = fullHandler(afterFirst, oldState => view.onTick(oldState, tickCount) )
        if simpleHandler.isDisabled && fullHandler.isDisabled then
          noHandlerWarning()
        mayContinue = unlimitedTicks || tickCount < tickLimit
        runFrom(afterBoth)
    this.loadModel(this.initialState)
    runFrom(this.initialState)
    this.onDone()

  end simulate


  private[gui] override def onDone(): Unit =
    this.stop()


  /** Programmatically requests an update to the graphics of the view (even though no
    * clock tick or triggering GUI event occurred). */
  final def refresh(): Unit =
    this.events ! Refresh()


  /** Stops the view: stops the clock and stops listening to events. A stopped view cannot be restarted. */
  final def stop(): Unit =
    if !this.hasStopped then
      this.hasStopped = true
      this.ticker.stop()
      this.eventSystem.terminate()
      this.onStop()

  /** Returns a brief textual description of the view. */
  override def toString: String = s"view of $modelClassName"


  /** Renders the view as a [[java.awt.image.BufferedImage Java AWT `BufferedImage`]] onto the component. */
  override def paintComponent(myGraphics: Graphics2D): Unit =
    for image <- latestComputed.pic.map( _.toImage ) do
      image.draw(myGraphics)

  private[viewimpl] def loadModel(initialState: Model): Unit =
    val initialPic =
      try
        view.makePic(initialState).anchorAt(Anchor.Center)
      catch case NothingToDraw => Pic.square(150, Black)
    this.preferredSize = java.awt.Dimension(initialPic.width.floor.toInt, initialPic.height.floor.toInt)
    this.latestComputed = Latest(initialState, Some(initialPic))

  private def renderIfAppropriate(currentState: Model): Unit =
    if this.refreshPolicy.shouldRefresh(this.latestComputed.state, currentState) then
      this.render(currentState)

  private[viewimpl] def render(stateToDraw: Model): Unit =
    Try(view.makePic(stateToDraw).anchorAt(Anchor.Center)) match
      case Success(newPic)             => this.latestComputed = Latest(stateToDraw, Some(newPic)); this.repaint()
      case Failure(NothingToDraw)      => // keep earlier image
      case Failure(crashInClientCode)  => this.error("using makePic to render the view", crashInClientCode)

  private def error(situation: String, cause: Throwable): Unit =
    warn(s"An error occurred while $situation. Stopping the view. Here is a detailed report:", cause)
    this.stop()


  private final class ModelState(initialState: Model) extends Actor:
    //noinspection ActorMutableStateInspection
    private var isActive = true           // one-way flag
    //noinspection ActorMutableStateInspection
    private var state    = initialState

    @annotation.nowarn("msg=pattern selector should be an instance of Matchable")
    def receive: PartialFunction[Any, Unit] =
      case message: Message => if this.isActive then this.handleIfHandlerEnabled(message)
      case unexpected       => warn("Unexpected event: " + unexpected)

    private def handleIfHandlerEnabled(message: Message): Unit =
      val enabledHandlers = this.handlersFor(message).filter( _.isEnabled )

      def applyAllEnabledHandlers(): Unit =
        val handlersForMessage = enabledHandlers.map((h: Handler) => (oldState: Model) => h(message, oldState))
        this.state = handlersForMessage.reduce( _ compose _ )(this.state)
        Try(view.sound(this.state)) match
          case Success(Some(sound))       => sound.play()
          case Success(None)              => // silence
          case Failure(crashInClientCode) => view.error("using the method sound on the view", crashInClientCode)
        Try(view.isDone(this.state)) match
          case Success(true)              => this.isActive = false; view.onDone()
          case Success(false)             => // continue
          case Failure(crashInClientCode) => view.error("using the method isDone on the view", crashInClientCode)
        view.renderIfAppropriate(this.state)

      message match
        case refresh: Refresh =>
          view.renderIfAppropriate(this.state)
        case tick: Tick =>
          if enabledHandlers.nonEmpty then
            applyAllEnabledHandlers()
        case message: GUIMessage =>
          if enabledHandlers.nonEmpty then
            this.checkForGUIDelay(message)
            applyAllEnabledHandlers()

    end handleIfHandlerEnabled

    private def checkForGUIDelay(message: GUIMessage): Unit =
      if message.isDelayed then
        val description = message.getClass
        warn(s"Response to GUI event ($description) lagging behind.")


    private final class Handler(underlying: PartialFunction[Message, Model => Model]) extends Function2[Message, Model, Model]:
      private var hasDefaulted = false
      private var hasCrashed   = false

      def apply(message: Message, oldState: Model): Model =
        if this.underlying.isDefinedAt(message) then
          val callClientMethod = this.underlying(message)
          Try(callClientMethod(oldState)) match
            case Success(newState) => newState
            case Failure(NoHandlerDefined)  =>
              this.hasDefaulted = true
              oldState
            case Failure(crashInClientCode) =>
              this.hasCrashed = true
              warn("An error occurred in event-handler code. Disabling the handler. Here is a detailed report:")
              crashInClientCode.printStackTrace(System.err)
              oldState
        else
          warn("Unexpected failure to handle message: " + message)
          oldState
      end apply

      def isEnabled: Boolean = !this.hasDefaulted && !this.hasCrashed
    end Handler

    private def handlersFor(message: Message): Seq[Handler] = message match
      case Tick(_)                        => Seq(SimpleHandlers.tick, FullHandlers.tick)
      case Refresh()                      => Seq()
      case GUIMessage(_: MouseEntered)    => Seq(FullHandlers.mouseEnter)
      case GUIMessage(_: MouseExited)     => Seq(FullHandlers.mouseExit)
      case GUIMessage(_: MouseReleased)   => Seq(SimpleHandlers.mouseUp,   FullHandlers.mouseUp)
      case GUIMessage(_: MousePressed)    => Seq(SimpleHandlers.mouseDown, FullHandlers.mouseDown)
      case GUIMessage(_: MouseMoved)      => Seq(SimpleHandlers.mouseMove, FullHandlers.mouseMove)
      case GUIMessage(_: MouseDragged)    => Seq(SimpleHandlers.mouseDrag, FullHandlers.mouseDrag)
      case GUIMessage(_: MouseWheelMoved) => Seq(SimpleHandlers.wheel,     FullHandlers.wheel)
      case GUIMessage(_: MouseClicked)    => Seq(SimpleHandlers.click,     FullHandlers.click)
      case GUIMessage(_: KeyPressed)      => Seq(SimpleHandlers.keyDown,   FullHandlers.keyDown)
      case GUIMessage(_: KeyReleased)     => Seq(SimpleHandlers.keyUp,     FullHandlers.keyUp)
      case GUIMessage(_: KeyTyped)        => Seq(SimpleHandlers.typed,     FullHandlers.typed)
      case GUIMessage(unexpected: InputEvent) => warn("No valid handlers for event " + unexpected); Seq.empty

    private object SimpleHandlers:
      val tick      = Handler({case Tick(_) => (s: Model) => view.onTick(s)})
      val mouseMove = Handler({case GUIMessage(event: MouseMoved)      => (s: Model) => view.onMouseMove(s, Pos(event.point))})
      val mouseDrag = Handler({case GUIMessage(event: MouseDragged)    => (s: Model) => view.onMouseDrag(s, Pos(event.point))})
      val mouseDown = Handler({case GUIMessage(event: MousePressed)    => (s: Model) => view.onMouseDown(s, Pos(event.point))})
      val mouseUp   = Handler({case GUIMessage(event: MouseReleased)   => (s: Model) => view.onMouseUp  (s, Pos(event.point))})
      val wheel     = Handler({case GUIMessage(event: MouseWheelMoved) => (s: Model) => view.onWheel    (s, event.rotation)})
      val click     = Handler({case GUIMessage(event: MouseClicked)    => (s: Model) => view.onClick    (s, Pos(event.point))})
      val keyDown   = Handler({case GUIMessage(event: KeyPressed)      => (s: Model) => view.onKeyDown  (s, event.key)})
      val keyUp     = Handler({case GUIMessage(event: KeyReleased)     => (s: Model) => view.onKeyUp    (s, event.key)})
      val typed     = Handler({case GUIMessage(event: KeyTyped)        => (s: Model) => view.onType     (s, event.char)})

    private object FullHandlers:
      val tick       = Handler({case Tick(time) => (s: Model) => view.onTick(s, time)})
      val mouseEnter = Handler({case GUIMessage(event: MouseEntered)    => (s: Model) => view.onMouseEnter(s, event)})
      val mouseExit  = Handler({case GUIMessage(event: MouseExited)     => (s: Model) => view.onMouseExit (s, event)})
      val mouseUp    = Handler({case GUIMessage(event: MouseReleased)   => (s: Model) => view.onMouseUp   (s, event)})
      val mouseDown  = Handler({case GUIMessage(event: MousePressed)    => (s: Model) => view.onMouseDown (s, event)})
      val mouseMove  = Handler({case GUIMessage(event: MouseMoved)      => (s: Model) => view.onMouseMove (s, event)})
      val mouseDrag  = Handler({case GUIMessage(event: MouseDragged)    => (s: Model) => view.onMouseDrag (s, event)})
      val wheel      = Handler({case GUIMessage(event: MouseWheelMoved) => (s: Model) => view.onWheel     (s, event)})
      val click      = Handler({case GUIMessage(event: MouseClicked)    => (s: Model) => view.onClick     (s, event)})
      val keyDown    = Handler({case GUIMessage(event: KeyPressed)      => (s: Model) => view.onKeyDown   (s, event)})
      val keyUp      = Handler({case GUIMessage(event: KeyReleased)     => (s: Model) => view.onKeyUp     (s, event)})
      val typed      = Handler({case GUIMessage(event: KeyTyped)        => (s: Model) => view.onType      (s, event)})

  end ModelState

end ViewComponentImpl


private final class Latest[State <: Matchable](val state: State, val pic: Option[Pic]):
  val timestamp: Long = System.currentTimeMillis

