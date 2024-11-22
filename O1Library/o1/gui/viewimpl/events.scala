package o1.gui.viewimpl

import o1.gui.*
import o1.gui.View.TicksPerSecondDefault
import o1.util.firstTimeOnly

import scala.swing.event.InputEvent

import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.typesafe.config.Config
import akka.dispatch.*
import akka.util.StablePriorityBlockingQueue


private[gui] object events:
  private val messageLagWarning = firstTimeOnly( warn("Failing to compute states fast enough. Discarding some clock ticks.") )

  object Message:
    type Kind = String

  sealed trait Message:
    val start: Long = System.currentTimeMillis()
    def delay: Long = System.currentTimeMillis() - this.start
    def isDelayed: Boolean = this.delay > 800
    def isBadlyDelayed: Boolean = this.delay > 3000

  final case class Tick(time: Long) extends Message
  final case class Refresh() extends Message
  final case class GUIMessage(event: InputEvent) extends Message

  object Mailbox:
    val BacklogSizeThreshold: Int = TicksPerSecondDefault * 100

    @annotation.nowarn("msg=pattern selector should be an instance of Matchable")
    val Priorities = PriorityGenerator {
      case tick: Tick => 100
      case guiMessage => 1
    }

    final class Queue extends QueueBasedMessageQueue:
      final val queue = StablePriorityBlockingQueue(100, Priorities)

      def dequeue(): Envelope =
        if this.hasMessages then
          this.queue.remove()
        else
          o1.gui.swingops.Null

      @annotation.nowarn("msg=pattern selector should be an instance of Matchable")
      def enqueue(receiver: ActorRef, envelope: Envelope): Unit =
        envelope.message match
          case Tick(_)         => this.addTickUnlessSwamped(envelope)
          case anyOtherMessage => this.queue.add(envelope)

      private def addTickUnlessSwamped(envelope: Envelope): Unit =
        val queueFull = this.queue.size >= Mailbox.BacklogSizeThreshold
        if queueFull then
          messageLagWarning()
        else
          this.queue.add(envelope) // later: send to DeadLetters?
    end Queue

  end Mailbox


  final case class Mailbox() extends MailboxType:
    def this(settings: ActorSystem.Settings, config: Config) = this()
    override def create(owner: Option[ActorRef], system: Option[ActorSystem]) =
      Mailbox.Queue()

end events

