package o1.util

import java.awt.event.ActionEvent
import Ticker.*
import javax.swing.{AbstractAction, Action, Timer}
import o1.util.nice.number.*


private[o1] object Ticker:

  sealed trait TickState derives CanEqual
  final case class TickEvery(interval: Int) extends TickState
  case object NotTicking extends TickState

  object TickState:
    def fromTickRate(rate: Double): TickState = fromDouble(1000 / rate)
    def fromDouble(interval: Double): TickState =
      if interval.isInfinity then NotTicking
      else TickEvery((interval atLeast 0 atMost Int.MaxValue).toInt)

end Ticker


private[o1] final class Ticker private(additionalInitialDelay: Int, private var tickState: TickState, private val timedAction: Action):

  def this(additionalInitialDelay: Int, interval: TickState)(timedBlock: => Unit) =
    this(additionalInitialDelay, interval, new AbstractAction:
      def actionPerformed(tick: ActionEvent) = timedBlock
    )

  private val javaTimer =
    val interval = this.tickState match
      case TickEvery(interval) => interval
      case NotTicking          => 0
    val timer = Timer(interval, this.timedAction)
    timer.setRepeats(true)
    timer.setInitialDelay(additionalInitialDelay + interval)
    timer

  def adjust(newState: TickState): Unit =
    this.javaTimer.setInitialDelay(0)
    val wasStill = this.tickState == NotTicking
    this.tickState = newState
    newState match
      case TickEvery(interval) =>
        this.javaTimer.setDelay(interval)
        if wasStill then
          this.start()
      case NotTicking =>
        this.stop()

  def start(): Unit =
    this.tickState match
      case TickEvery(_) =>
        this.javaTimer.start()
      case NotTicking =>
        pass

  def stop(): Unit =
    this.javaTimer.stop()

  def isRunning: Boolean = this.javaTimer.isRunning

end Ticker

