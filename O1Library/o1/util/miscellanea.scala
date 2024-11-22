/** The package `o1.util` contains miscellaneous tools that are used internally by some of the
  * given programs in O1 for added convenience.
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this package works
  * or can be used.**
  *
  * This documentation lists only some of the tools in the package. The listed tools are largely
  * a mix of:
  *
  *  - functions for simple I/O from files and URLs;
  *  - aliases for easy access (via `import o1.util.*`)
  *    to some of the tools from `scala.util`; and
  *  - implicit classes that add a few convenience methods
  *    to selected types from the Scala API.  */
package o1.util



/** A label that can be used as an empty method body or other “no-op” block. */
val DoNothing: Unit = ()


/** Performs a given computation and checks to see if it crashes with a `NotImplementedError`.
  * Returns the result of the computation in an `Option`; returns `None` only in the
  * computation wasn’t implemented. Any other exception is thrown.
  * @param  computation  a computation that may or may not be implemented
  * @tparam Result       the type of the given computation  */
def ifImplemented[Result](computation: =>Result): Option[Result] =
  Try(computation) match
    case Success(result)                       => Some(result)
    case Failure(missing: NotImplementedError) => None
    case Failure(miscProblem)                  => throw miscProblem


/** Performs a given computation and determines whether it crashes with a `NotImplementedError`.
  * Returns `true` if it did and `false` otherwise.
  * @param  computation  a computation that may or may not be implemented
  * @tparam Result       the type of the given computation  */
def isImplemented[Result](computation: =>Result): Boolean =
  ifImplemented(computation).isDefined


private[o1] def editDistance(text1: String, text2: String, threshold: Int): Int =
  if text1.isEmpty                 then if text2.length <= threshold then text2.length else Int.MaxValue
  else if text2.isEmpty            then if text1.length <= threshold then text1.length else Int.MaxValue
  else if text1.head == text2.head then editDistance(text1.tail, text2.tail, threshold)
  else if threshold == 0           then Int.MaxValue
  else
    val deletion     = editDistance(text1.tail, text2,      threshold - 1)
    val insertion    = editDistance(text1,      text2.tail, threshold - 1)
    val substitution = editDistance(text1.tail, text2.tail, threshold - 1)
    val shortest = Seq(deletion, insertion, substitution).min
    if shortest == Int.MaxValue then Int.MaxValue else shortest + 1


private[o1] def repeatEvery(interval: Int)(timedBlock: => Unit): Ticker =
  val ticker = Ticker(interval, Ticker.TickEvery(interval))(timedBlock)
  ticker.start()
  ticker


private[o1] object Program: // not entirely reliable, but good enough for some purposes

  lazy val isRunningInScalaREPL: Boolean =
    Try(Class.forName("dotty.tools.repl.ReplDriver")).isSuccess

  lazy val isRunningInGTK: Boolean =
    javax.swing.UIManager.getSystemLookAndFeelClassName.endsWith("GTKLookAndFeel")

end Program


private[o1] final class FirstTimeEffect(effect: => Unit):
  private lazy val storedEffect = effect
  def apply(): Unit = storedEffect

private[o1] object firstTimeOnly:
  def apply(effect: => Unit): FirstTimeEffect = FirstTimeEffect(effect)

//noinspection UnitMethodIsParameterless
private[o1] inline def pass: Unit = ()

