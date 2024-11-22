package o1.gui

import o1.gui.event.*
import View.*
import o1.util.pass
import viewimpl.*
import o1.sound.sampled.Sound


/** This object holds miscellaneous utilities (traits, constants, etc.) that can be
  * used in combinations with `View`s. These utilities are generic to the [[o1.gui.mutable]]
  * and [[o1.gui.immutable]] `View` variants.
  *
  * **Note to students: You’re unlikely to need this for anything in O1.** */
object View:

  private[o1] trait Controls[Model]:

    private[gui] def onDone(): Unit = pass
    def simulate(tickLimit: Int): Unit
    def start(): Unit

    private[gui] def makePic(state: Model): Pic
    private[gui] def onStop() = pass
    private[gui] def isDone(state: Model) = false
    private[gui] def isPaused = false
    private[gui] def sound(state: Model): Option[Sound] = None

    private[gui] def onTick(previousState: Model): Model
    private[gui] def onTick(previousState: Model, time: Long): Model

    // Simple GUI handlers:
    private[gui] def onMouseMove(state: Model, position: Pos)  : Model
    private[gui] def onMouseDrag(state: Model, position: Pos)  : Model
    private[gui] def onMouseDown(state: Model, position: Pos)  : Model
    private[gui] def onMouseUp  (state: Model, position: Pos)  : Model
    private[gui] def onWheel    (state: Model, rotation: Int)  : Model
    private[gui] def onClick    (state: Model, position: Pos)  : Model
    private[gui] def onKeyDown  (state: Model, key: Key)       : Model
    private[gui] def onKeyUp    (state: Model, key: Key)       : Model
    private[gui] def onType     (state: Model, character: Char): Model

    // Full GUI handlers:
    private[gui] def onMouseMove (state: Model, event: MouseMoved     ): Model
    private[gui] def onMouseDrag (state: Model, event: MouseDragged   ): Model
    private[gui] def onMouseEnter(state: Model, event: MouseEntered   ): Model
    private[gui] def onMouseExit (state: Model, event: MouseExited    ): Model
    private[gui] def onMouseUp   (state: Model, event: MouseReleased  ): Model
    private[gui] def onMouseDown (state: Model, event: MousePressed   ): Model
    private[gui] def onWheel     (state: Model, event: MouseWheelMoved): Model
    private[gui] def onClick     (state: Model, event: MouseClicked   ): Model
    private[gui] def onKeyDown   (state: Model, event: KeyPressed     ): Model
    private[gui] def onKeyUp     (state: Model, event: KeyReleased    ): Model
    private[gui] def onType      (state: Model, event: KeyTyped       ): Model

    private[gui] def maxLifeSpan: Long = Long.MaxValue

  end Controls

  private[gui] object NoHandlerDefined extends Throwable("no event handler defined",
        /*cause=*/ o1.gui.swingops.Null, /*enableSuppression=*/ true, /*writableStackTrace=*/ false)
  private[gui] given CanEqual[NoHandlerDefined.type, Throwable] = CanEqual.derived

  /** Add this trait on a `View` to give it a pause toggle. You’ll still need to call `togglePause`
    * on whichever event you want to pause the view (e.g., user hitting space bar). */
  trait HasPauseToggle:
    self: Controls[?] =>

    /** Determines whether the view should be paused at the current state. */
    override def isPaused = this.pauseToggle   // N.B. This line generates a (harmless) false-positive error in IJ if type-aware highlighting is enabled.

    /** Whether the view starts in a paused state. By default, always returns `false`. */
    def startsPaused = false

    private var pauseToggle = this.startsPaused

    /** Tells the view to pause if unpaused and vice versa. */
    def togglePause() =
      this.pauseToggle = !this.pauseToggle

  end HasPauseToggle


  /** The number of clock ticks (24) that a `View` aims to generate per clock tick,
    * unless otherwise specified. */
  val TicksPerSecondDefault = 24
  private[gui] val TicksPerSecondMax = 1000

  /** A supertype for the different policies for updating the image visible in a `View` in
    * response to a change in the model.
    *
    * **Note to students: You’re unlikely to need this for anything in O1.** */
  trait RefreshPolicy:
    /** Returns a `Boolean` that indicates whether the `View` should try to update itself
      * after observing a given state of the model.
      * @param from  the model object at the previous update
      * @param to    the current model object (possibly the same object; possibly identical) */
    def shouldRefresh(from: Matchable, to: Matchable): Boolean
  end RefreshPolicy

  /** This companion object provides predefines instances of class [[RefreshPolicy]].
   *
   * **Note to students: You’re unlikely to need this for anything in O1.** */
  object RefreshPolicy:
    private given CanEqual[Matchable, Matchable] = CanEqual.derived  // allow non-strict equality locally

    /** The [[RefreshPolicy]] of seeking to update the image visible in a `View` as often as possible.
      * (Time-consuming but always safe.)
      *
      * **Note to students: You’re unlikely to need this for anything in O1.** */
    case object Always extends RefreshPolicy:
      /** Returns `true` to indicated that the `View` should try to update itself no matter the
        * current and earlier states of the model. */
      def shouldRefresh(from: Matchable, to: Matchable) = true

    /** The [[RefreshPolicy]] of seeking to update the image visible in a `View` only when the current model
      * object is different in identity than the previous one shown.
      *
      * **Note to students: You’re unlikely to need this for anything in O1.** */
    case object UnlessSameReference extends RefreshPolicy:
      /** Returns `true` if given two references to the same objects or two `AnyVal`s that are equal.
        * @param from  the model object at the previous update
        * @param to    the current model object (possibly the same object; possibly identical) */
      def shouldRefresh(from: Matchable, to: Matchable) =
        (from, to) match
          case (ref1: AnyRef, ref2: AnyRef) => ref1 ne ref2
          case (val1: AnyVal, val2: AnyVal) => val1 != val2

    /** The [[RefreshPolicy]] of seeking to update the image visible in a `View` only when the
      * current  model object is non-identical in terms of `equals` than the previous one shown.
      *
      * **Note to students: You’re unlikely to need this for anything in O1.** */
    case object UnlessIdentical extends RefreshPolicy:
      /** Returns `true` if given two objects aren’t equal (`!=`).
        * @param from  the model object at the previous update
        * @param to    the current model object (possibly the same object; possibly identical) */
      def shouldRefresh(from: Matchable, to: Matchable) = from != to

  end RefreshPolicy

  private[gui] object NothingToDraw extends Throwable("nothing to draw in view",
    /*cause=*/ o1.gui.swingops.Null, /*enableSuppression=*/ true, /*writableStackTrace=*/ false)


  private[gui] given CanEqual[NothingToDraw.type, Throwable] = CanEqual.derived

  export RefreshPolicy.*

end View



///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////// MUTABLE VARIANT /////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////

/** This subpackage contains the version of `View`s that we primarily use in O1: views
  * to *mutable* domain models.
  *
  * The top-level package [[o1]] provides an alias to the [[ViewFrame]] class in this
  * package, so it is available to students as `View` simply by importing `o1.*`.
  *
  * There is an alternative implementation of `View`s in [[o1.gui.immutable]]. */
object mutable:
  import View.*

  private val default = o1.gui.viewimpl.default.mutable

  /** An alias for [[ViewFrame]], which is the default sort of `View`. (Cf. the alternative, [[ViewComponent]].)  */
  type View[Model <: AnyRef] = ViewFrame[Model]

  private[gui] trait HasModelField[Model]:
    private[gui] def initialModel: Model
    /** the model object represented in the view. */
    def model = this.initialModel

  /** Add this trait on a view to enable discarding its model object for another. */
  trait HasReplaceableModel[Model] extends HasModelField[Model]:
    private var currentModel = this.initialModel
    /** the model object most recently set for the view.
      * @see [[model_=]] */
    override def model = this.currentModel
    /** Replaces the model object previously set for the view with the given one. */
    def model_=(replacementModel: Model) =
      this.currentModel = replacementModel


  /** A Swing-embeddable view (complete with a picture, a ticking clock, event handlers, etc.).
    * It works like a [[ViewFrame]] except that it’s a Swing component, not a standalone GUI frame.
    * See [[ViewFrame]] for an overview.
    *
    * @param initialModel      the model to be displayed in the view (the only required parameter).
    *                          It usually makes sense to use a mutable object here and change its
    *                          state via the event handlers (cf. [[o1.gui.immutable.ViewComponent]]).
    * @param tickRate          the clock of the view will tick roughly this many times per second
    *                          (optional; defaults to 24)
    * @param initialDelay      an additional delay in milliseconds between calling [[start]] and the
    *                          clock starting (optional; defaults to 600)
    * @param refreshPolicy     a policy for how eagerly the view should try to update the graphical
    *                          representation of its model (optional; changing this may improve
    *                          efficiency in some circumstances)
    * @tparam Model  the type of the model object */
  abstract class ViewComponent[Model <: AnyRef](private[gui] val initialModel: Model, tickRate: Double = TicksPerSecondDefault, initialDelay: Int = 600, refreshPolicy: RefreshPolicy = Always)
           extends ViewComponentImpl(initialModel, tickRate, initialDelay, refreshPolicy)
           with default.Controls[Model]:

    /** Returns a view that collects of the ticks and GUI events that the `View`’s event handlers
      * process, using the given function to generate that trace. That trace-collecting view, which
      * an instance of the [[Traced]] subclass, delegates the actual event handling to this original
      * view but provides an additional interface for tracing.
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace
      * @see [[traced]], [[tracedPics]] */
    final def tracedWith[TraceData](extractTrace: Model=>TraceData): Traced[TraceData] =
      Traced(extractTrace)

    /** A view that wraps around another, collecting a log or *trace* of events while delegating
      * its actual event-handling to the wrapped view. Provides additional methods for accessing
      * such traces: [[trace]], [[simulateAndGet]], and [[startAndGet]]. A few examples of using
      * these methods are given below.
      *
      * `simulate` 500 clock ticks on the trace-collecing view and print the trace of clock ticks
      * accompanied by descriptions of the view’äs `model`.
      *
      * ```scala
      *   for (traceItem, traceEvent) <- myTracedView.simulateAndGet(500) do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Or, equivalently:
      *
      * ```scala
      *   myTracedView.simulate(500)
      *   for (traceItem, traceEvent) <- myTracedView.trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Alternatively, start the trace-collecting view and run it interactively until it is done or
      * a tick limit is reached. Then print the trace of ticks and GUI events accompanied by
      * descriptions of the view’s `model`.
      *
      * ```scala
      *   val futureTrace = myTracedView.startAndGet(tickLimit=100) recover {
      *     case Aborted(message, partialTrace) => partialTrace
      *   }
      *   for trace <- futureTrace; (traceItem, traceEvent) <- trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace */
    final class Traced[TraceData](private[gui] val extractTrace: Model=>TraceData)
                extends ViewComponent[Model](initialState, ticksPerSecond, initialDelay, refreshPolicy)
                with default.TraceGeneratingView[Model,TraceData]:
      private[gui] val source = ViewComponent.this


  end ViewComponent


  /** This class provides a framework for building simple GUIs. Each instance of the class is a graphical
    * view to an object that represents a particular domain; that object is the [[model]] of the view.
    * A `ViewFrame` displays the model as graphics within a GUI frame.
    *
    * This class is available under the alias `View` in the top-level package [[o1]], so students can
    * access it simply by importing `o1.*`.
    *
    * The key method in the class is [[makePic]], which the view calls automatically and repeatedly to
    * determine which [[Pic]] to display in the frame at each moment in time. Concrete view objects must
    * add an implementation for this abstract method.
    *
    * A view listens to GUI events within the frame, but it doesn’t really do anything when notified
    * of an event; concrete instances of the class can override this behavior by overriding one of the
    * “on methods” (`onClick`, `onMouseMove`, etc.). The view also runs an internal clock and can react
    * to the passing of time (`onTick`).
    *
    * Just creating a view object is not enough to display it onscreen and start the clock; see the
    * [[start]] method.
    *
    * @param initialModel      the model to be displayed in the view (the only required parameter).
    *                          It usually makes sense to use a mutable object here and change its state
    *                          via the event handlers (cf. [[o1.gui.immutable.ViewFrame]]).
    * @param tickRate          the clock of the view will tick roughly this many times per second
    *                          (optional; defaults to 24)
    * @param title             a string to be displayed in the frame’s title bar (optional)
    * @param initialDelay      an additional delay in milliseconds between calling [[start]] and the
    *                          clock starting (optional; defaults to 600)
    * @param terminateOnClose  whether the entire application should exit when the `ViewFrame` is closed
    *                          (optional; defaults to `true`)
    * @param closeWhenDone     whether the `ViewFrame` should be hidden and its clock stopped once the view
    *                          has reached a “done state” (as per [[isDone]]) (optional; defaults to `false`)
    * @param refreshPolicy     a policy for how eagerly the view should try to update the graphical
    *                          representation of its model (optional; changing this may improve
    *                          efficiency in some circumstances)
    * @tparam Model  the type of the model object  */
  abstract class ViewFrame[Model <: AnyRef](private[gui] val initialModel: Model, tickRate: Double = TicksPerSecondDefault, title: String = "", initialDelay: Int = 600,
                                            terminateOnClose: Boolean = true, closeWhenDone: Boolean = false, refreshPolicy: RefreshPolicy = Always)
           extends ViewFrameImpl(initialModel, tickRate, title, initialDelay, terminateOnClose, closeWhenDone, refreshPolicy)
           with default.Controls[Model]:

    /** An alternative constructor. Takes in just the model and the title; uses the defaults for all
      * the other parameters. Please see the multi-parameter constructor for details.
      * @param initialModel      the model to be displayed in the view
      * @param title             a string to be displayed in the frame’s title bar */
    def this(initialModel: Model, title: String) = this(initialModel, TicksPerSecondDefault, title)

    /** An alternative constructor. Takes in just the model and the tick rate; uses the defaults for
      * all the other parameters. Please see the multi-parameter constructor for details.
      * @param initialModel      the model to be displayed in the view
      * @param tickRate          the clock of the view will tick roughly this many times per second */
    def this(initialModel: Model, tickRate: Double) = this(initialModel, tickRate, "")


    /** Returns a view that collects of the ticks and GUI events that the `View`’s event handlers
      * process, using the given function to generate that trace. That trace-collecting view, which
      * an instance of the [[Traced]] subclass, delegates the actual event handling to this original
      * view but provides an additional interface for tracing.
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace
      * @see [[traced]], [[tracedPics]] */
    final def tracedWith[TraceData](extractTrace: Model=>TraceData): Traced[TraceData] =
      Traced(extractTrace)

    /** A view that wraps around another, collecting a log or *trace* of events while delegating
      * its actual event-handling to the wrapped view. Provides additional methods for accessing
      * such traces: [[trace]], [[simulateAndGet]], and [[startAndGet]]. A few examples of using
      * these methods are given below.
      *
      * `simulate` 500 clock ticks on the trace-collecing view and print the trace of clock ticks
      * accompanied by descriptions of the view’äs `model`.
      *
      * ```scala
      *   for (traceItem, traceEvent) <- myTracedView.simulateAndGet(500) do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Or, equivalently:
      *
      * ```scala
      *   myTracedView.simulate(500)
      *   for (traceItem, traceEvent) <- myTracedView.trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Alternatively, start the trace-collecting view and run it interactively until it is done or
      * a tick limit is reached. Then print the trace of ticks and GUI events accompanied by
      * descriptions of the view’s `model`.
      *
      * ```scala
      *   val futureTrace = myTracedView.startAndGet(tickLimit=100) recover {
      *     case Aborted(message, partialTrace) => partialTrace
      *   }
      *   for trace <- futureTrace; (traceItem, traceEvent) <- trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace */
    final class Traced[TraceData](private[gui] val extractTrace: Model=>TraceData)
                extends ViewFrame[Model](initialModel, ticksPerSecond, title, initialDelay, terminateOnClose, closeWhenDone, refreshPolicy)
                with default.TraceGeneratingView[Model,TraceData]:
      private[gui] val source = ViewFrame.this

  end ViewFrame


  /** This class provides a framework for building simple GUIs. It is a further simplified version
    * of [[ViewFrame]]. A `BasicViewFrame` is like a `ViewFrame` except that it doesn’t have an
    * explicit `model` object at all; you don’t need to pass in one as a constructor parameter.
    * (This is useful for certain educational purposes.)
    *
    * For other details, please see the [[ViewFrame]] supertype. */
  abstract class BasicViewFrame(title: String, tickRate: Double = TicksPerSecondDefault, initialDelay: Int = 600,
                               terminateOnClose: Boolean = true, closeWhenDone: Boolean = false)
          extends ViewFrame[BasicViewFrame.ImplicitModel](BasicViewFrame.ImplicitModel, tickRate, title, initialDelay,
                                                          terminateOnClose, closeWhenDone, RefreshPolicy.Always):
    /** Returns a brief textual description of the view. */
    override def toString: String =
      if this.title.isEmpty then s"an unnamed view" else s"""a view titled "$title""""
  end BasicViewFrame


  /** This companion object just provides a singleton value that is used as a `model` by
    * `BasicViewFrame`s that aren’t explictly given a `model` via a constructor parameter. */
  object BasicViewFrame:
    case object ImplicitModel
    type ImplicitModel = ImplicitModel.type
  end BasicViewFrame


  // This class essentially no-ops makePic and provides an interface for custom graphics without the Pic class.
  // Later: refactor to make this possible more elegantly.
  private[o1] abstract class ViewComponentWithCustomRendering[Model <: AnyRef](initialModel: Model,
                    val width: Int, val height: Int, tickRate: Double = TicksPerSecondDefault, initialDelay: Int = 600)
                    extends ViewComponent[Model](initialModel, tickRate, initialDelay, RefreshPolicy.Always):
    import scala.swing.Graphics2D

    private val initialPic = Pic.emptyCanvas(width, height)

    def render(myGraphics: Graphics2D): Unit

    final override def paintComponent(myGraphics: Graphics2D): Unit =
      render(myGraphics)

    final def makePic = this.initialPic

    final override private[gui] def render(stateToDraw: Model): Unit =
      this.repaint()

  end ViewComponentWithCustomRendering


end mutable


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
/////////////////// IMMUTABLE VARIANT /////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////


/** This subpackage contains a version of `View`s that is not much used in O1: views to *immutable*
  * domain models. In O1, the other implementation in [[o1.gui.mutable]] is more relevant. */
object immutable:

  private val default = o1.gui.viewimpl.default.immutable

  /** An alias for [[ViewFrame]], which is the default sort of `View`. (Cf. the alternative, [[ViewComponent]].)  */
  type View[Model <: Matchable] = ViewFrame[Model]


  /** A Swing-embeddable view (complete with a picture, a ticking clock, event handlers, etc.).
    * It works like a [[ViewFrame]] except that it’s a Swing component, not a standalone GUI frame.
    * See [[ViewFrame]] for an overview.
    *
    * @param initialState      the initial state of the model to be displayed in the view (the only
    *                          required parameter). This class has been designed to work conveniently
    *                          with immutable model objects (cf. [[o1.gui.mutable.ViewComponent]]).
    * @param tickRate          the clock of the view will tick roughly this many times per second
    *                          (optional; defaults to 24)
    * @param initialDelay      an additional delay in milliseconds between calling [[start]] and the
    *                          clock starting (optional; defaults to 600)
    * @param refreshPolicy     a policy for how eagerly the view should try to update the graphical
    *                          representation of its model (optional; changing this may improve
    *                          efficiency in some circumstances)
    * @tparam Model  the type of the states of the model */
  abstract class ViewComponent[Model <: Matchable](initialState: Model, tickRate: Double = TicksPerSecondDefault,
           initialDelay: Int = 600, refreshPolicy: RefreshPolicy = Always)
                 extends ViewComponentImpl(initialState, tickRate, initialDelay, refreshPolicy)
                 with default.Controls[Model]:


    /** Returns a view that collects of the ticks and GUI events that the `View`’s event handlers
      * process, using the given function to generate that trace. That trace-collecting view, which
      * an instance of the [[Traced]] subclass, delegates the actual event handling to this original
      * view but provides an additional interface for tracing.
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace
      * @see [[traced]], [[tracedPics]] */
    final def tracedWith[TraceData](extractTrace: Model=>TraceData): Traced[TraceData] =
      Traced(extractTrace)

    /** A view that wraps around another, collecting a log or *trace* of events while delegating
      * its actual event-handling to the wrapped view. Provides additional methods for accessing
      * such traces: [[trace]], [[simulateAndGet]], and [[startAndGet]]. A few examples of using
      * these methods are given below.
      *
      * `simulate` 500 clock ticks on the trace-collecing view and print the trace of clock ticks
      * accompanied by descriptions of the view’äs `model`.
      *
      * ```scala
      *   for (traceItem, traceEvent) <- myTracedView.simulateAndGet(500) do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Or, equivalently:
      *
      * ```scala
      *   myTracedView.simulate(500)
      *   for (traceItem, traceEvent) <- myTracedView.trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Alternatively, start the trace-collecting view and run it interactively until it is done or
      * a tick limit is reached. Then print the trace of ticks and GUI events accompanied by
      * descriptions of the view’s `model`.
      *
      * ```scala
      *   val futureTrace = myTracedView.startAndGet(tickLimit=100) recover {
      *     case Aborted(message, partialTrace) => partialTrace
      *   }
      *   for trace <- futureTrace; (traceItem, traceEvent) <- trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace */
    final class Traced[TraceData](private[gui] val extractTrace: Model => TraceData)
                extends ViewComponent[Model](initialState, ticksPerSecond, initialDelay, refreshPolicy)
                with default.TraceGeneratingView[Model,TraceData]:
      private[gui] val source = ViewComponent.this


  end ViewComponent


  /** This class provides a framework for building simple GUIs. Each instance of the class is a graphical
    * view to objects that represent the states of a domain model; those states can be (but are not required
    * to be) immutable objects. A `ViewFrame` displays the model as graphics within a GUI frame.
    *
    * **Note to students: this is not the view class that we commonly use in O1 but an alternative
    * implementation. For the usual `View`, see [[o1.gui.mutable.ViewFrame here]].**
    *
    * The key method in the class is [[makePic]], which the view calls automatically and repeatedly to
    * determine which [[Pic]] to display in the frame at each moment in time. Concrete view objects must
    * add an implementation for this abstract method.
    *
    * A view listens to GUI events within the frame, but it doesn’t really do anything when notified
    * of an event; concrete instances of the class can override this behavior by overriding one of the
    * “on methods” (`onClick`, `onMouseMove`, etc.). The view also runs an internal clock and can react
    * to the passing of time (`onTick`).
    *
    * Just creating a view object is not enough to display it onscreen and start the clock; see the
    * [[start]] method.
    *
    * Please note that even though this class is designed to work with immutable model states, the
    * actual `ViewFrame` is not itself immutable.
    *
    * @param initialState      the initial state of the model to be displayed in the view (the only
    *                          required parameter). This class has been designed to work conveniently
    *                          with immutable model objects (cf. [[o1.gui.mutable.ViewFrame]]).
    * @param tickRate          the clock of the view will tick roughly this many times per second
    *                          (optional; defaults to 24)
    * @param title             a string to be displayed in the frame’s title bar (optional)
    * @param initialDelay      an additional delay in milliseconds between calling [[start]] and the
    *                          clock starting (optional; defaults to 600)
    * @param terminateOnClose  whether the entire application should exit when the `ViewFrame` is closed
    *                          (optional; defaults to `true`)
    * @param closeWhenDone     whether the `ViewFrame` should be hidden and its clock stopped once the view
    *                          has reached a “done state” (as per [[isDone]]) (optional; defaults to `false`)
    * @param refreshPolicy     a policy for how eagerly the view should try to update the graphical
    *                          representation of its model (optional; changing this may improve
    *                          efficiency in some circumstances)
    * @tparam Model  the type of the states of the model */
  abstract class ViewFrame[Model <: Matchable](initialState: Model, tickRate: Double = TicksPerSecondDefault,
          title: String = "", initialDelay: Int = 600, terminateOnClose: Boolean = true,
          closeWhenDone: Boolean = false, refreshPolicy: RefreshPolicy = Always)
               extends ViewFrameImpl(initialState, tickRate, title, initialDelay, terminateOnClose, closeWhenDone, refreshPolicy)
               with default.Controls[Model]:

    /** An alternative constructor. Takes in just the initial state and the title; uses the defaults
      * for all the other parameters. Please see the multi-parameter constructor for details.
      * @param initialState      the initial state of the model to be displayed in the view
      * @param title             a string to be displayed in the frame’s title bar */
    def this(initialState: Model, title: String) = this(initialState, TicksPerSecondDefault, title)

    /** An alternative constructor. Takes in just the initial state and the tick rate; uses the defaults
      * for all the other parameters. Please see the multi-parameter constructor for details.
      * @param initialState      the initial state of the model to be displayed in the view
      * @param tickRate          the clock of the view will tick roughly this many times per second */
    def this(initialState: Model, tickRate: Double) = this(initialState, tickRate, "")

    /** Returns a view that collects of the ticks and GUI events that the `View`’s event handlers
      * process, using the given function to generate that trace. That trace-collecting view, which
      * an instance of the [[Traced]] subclass, delegates the actual event handling to this original
      * view but provides an additional interface for tracing.
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace
      * @see [[traced]], [[tracedPics]] */
    final def tracedWith[TraceData](extractTrace: Model=>TraceData): Traced[TraceData] =
      Traced(extractTrace)

    /** A view that wraps around another, collecting a log or *trace* of events while delegating
      * its actual event-handling to the wrapped view. Provides additional methods for accessing
      * such traces: [[trace]], [[simulateAndGet]], and [[startAndGet]]. A few examples of using
      * these methods are given below.
      *
      * `simulate` 500 clock ticks on the trace-collecing view and print the trace of clock ticks
      * accompanied by descriptions of the view’äs `model`.
      *
      * ```scala
      *   for (traceItem, traceEvent) <- myTracedView.simulateAndGet(500) do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Or, equivalently:
      *
      * ```scala
      *   myTracedView.simulate(500)
      *   for (traceItem, traceEvent) <- myTracedView.trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * Alternatively, start the trace-collecting view and run it interactively until it is done or
      * a tick limit is reached. Then print the trace of ticks and GUI events accompanied by
      * descriptions of the view’s `model`.
      *
      * ```scala
      *   val futureTrace = myTracedView.startAndGet(tickLimit=100) recover {
      *     case Aborted(message, partialTrace) => partialTrace
      *   }
      *   for trace <- futureTrace; (traceItem, traceEvent) <- trace do
      *     println(traceEvent + ": " + traceItem)
      * ```
      *
      * @param  extractTrace  a function that determines how to describe a model state in the generated trace
      * @tparam TraceData     the type of the model-state descriptions in the trace */
    final class Traced[TraceData](private[gui] val extractTrace: Model=>TraceData)
                extends ViewFrame[Model](initialState, ticksPerSecond, title, initialDelay, terminateOnClose, closeWhenDone, refreshPolicy)
                with default.TraceGeneratingView[Model,TraceData]:
      private[gui] val source = ViewFrame.this

  end ViewFrame


end immutable

