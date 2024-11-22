package o1.gui.viewimpl

import o1.gui.View
import scala.concurrent.{Future, Promise}

/** This object provides internal tooling that enable [[o1.gui]]’s `View`s to
  * record “traces” of ticks and GUI events. */
object tracing:

  trait GeneratesTrace[Model,TraceData] extends View.Controls[Model]:

    private[gui] val source: View.Controls[Model]
    private[gui] val initialState: Model
    private[gui] val extractTrace: Model=>TraceData
    private var tickLimitForStartAndGet = Long.MaxValue
    override private[gui] def maxLifeSpan = this.tickLimitForStartAndGet

    /** Indicates whether the view is paused. This implementation delegates
      * to the underlying `View` that is being traced. */
    final override def isPaused: Boolean = source.isPaused

    /** Returns a brief textual description of the view. */
    final override def toString = "traced " + super.toString

    private val traceBuilder =
      val builder = List.newBuilder[(TraceData, TraceEvent)]
      builder += extractTrace(this.initialState) -> TraceEvent.Init
      builder

    /** Returns a trace of the events processed by this view. The trace comes in a collection
      * of pairs, each of which is composed of a `TraceData` value that describes at the
      * time of the event and a [[TraceEvent]] value that describes the event itself. */
    final def trace: Seq[(TraceData, TraceEvent)] = this.traceBuilder.result()

    inline private[gui] def log(state: Model, traceEvent: TraceEvent) =
      this.traceBuilder += extractTrace(state) -> traceEvent
      state

    /** Simulates this trace-generating view with [[simulate]] and returns the
      * resulting trace. This is equivalent to calling first [[simulate]], then
      * [[trace]]. See also [[startAndGet]].
      * @param tickLimit  the maximum number of ticks to simulate; `Int.MaxValue`
      *                   (which is the default) means there is no such limit */
    final def simulateAndGet(tickLimit: Int = Int.MaxValue): Seq[(TraceData, TraceEvent)] =
      this.simulate(tickLimit)
      this.trace

    /** Starts this trace-generating view with [[start]] and returns a [[scala.concurrent.Future Future]]
      * that evaluates to the resulting trace. The `Future` succeeds when the view is done;
      * if the view isn’t done after a given number of clock ticks, the `Future` fails with
      * [[TraceAborted]], producing a partial trace. See also [[simulateAndGet]].
      * @param tickLimit  the maximum number of ticks to process before the future completes
      *                   with a failure; defaults to `Long.MaxValue` */
    final def startAndGet(tickLimit: Long = Long.MaxValue): Future[Seq[(TraceData, TraceEvent)]] =
      this.tickLimitForStartAndGet = tickLimit
      this.start()
      this.promiseOfTrace.future

    private val promiseOfTrace = Promise[Seq[(TraceData, TraceEvent)]]

    private[gui] override def onDone(): Unit =
      super.onDone()
      this.promiseOfTrace.success(this.trace)

    /** Causes an additional effect when the view is stopped (with `stop()`).
      * This implementation delegates to the underlying `View` that is being traced.
      * In addition, if the traced view had been started with `startAndGet` and reaches
      * its tick limit before being done, this method causes the returned future to
      * complete with a failure. */
    final override def onStop(): Unit =
      this.promiseOfTrace.tryFailure(TraceAborted("view did not reach done state in " + this.tickLimitForStartAndGet + " ticks and was aborted; partial trace collected", this.trace))
      source.onStop()

  end GeneratesTrace


  /** Describes a single tick or GUI event recorded while tracing a `View`.
    * See the [[TraceEvent$ `TraceEvent` companion object]] for specific subtypes. */
  sealed trait TraceEvent:
    /** a short description of the event */
    val name: String

  /** This companion object of the sealed `TraceEvent` trait provides implementations
    * for the trait. They can be used for tracing different kinds of events. */
  object TraceEvent:

    /** A `TraceEvent` that marks the start of a trace. */
    case object Init extends TraceEvent:
      val name = "init"

    /** A `TraceEvent` that records a clock tick.
      * @param time  the number of the tick, if available */
    final case class Tick(val time: Option[Long]) extends TraceEvent:
      val name = "tick"
      override def toString = this.name + this.time.map( t => s"($t)" ).getOrElse("")
    end Tick
    object Tick:
      private[o1] def apply(): Tick = new Tick(None)
      private[o1] def apply(time: Long): Tick = new Tick(Some(time))
    end Tick

    /** A `TraceEvent` that records a GUI event.
      * @param  name  a short description of the GUI event, such as `"click"`, `"key-down"`, or `"mouse-move"`
      * @param  info  additional information about the event
      * @tparam Info  the type of the additional information, such as `Pos`, `Key`, or `MouseMoved` */
    final case class Input[Info](val name: String, val info: Info) extends TraceEvent:
      override def toString = s"$name ($info)"

  end TraceEvent


  /** Represents situations where a `View` hasn’t reached its done state in the allotted number of ticks.
    * @param  partialTrace  the trace collected until the view was aborted.
    * @tparam TraceData     the type of the model-state descriptions in the trace */
  case class TraceAborted[TraceData](message: String, val partialTrace: Seq[(TraceData, TraceEvent)]) extends RuntimeException(message)

end tracing

