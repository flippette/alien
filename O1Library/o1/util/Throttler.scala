package o1.util

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private[o1] final class DiscardingThrottler:

  private var isIdle = true

  def ifIdle[Result](possiblyHeavyComputation: => Result) =
    if this.isIdle then
      this.isIdle = false
      val worker = Future(possiblyHeavyComputation)
      worker.onComplete( _ => this.isIdle = true )
      worker
    else
      Future.failed(DiscardingThrottler.Busy)

end DiscardingThrottler


private[o1] object DiscardingThrottler:
  object Busy extends RuntimeException("discarding throttler busy; ignoring work request")

