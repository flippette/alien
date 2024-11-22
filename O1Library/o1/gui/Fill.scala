package o1.gui

import o1.gui.colors.Transparent
import o1.world.Pos
import o1.util.nice.number.*

import java.awt

/** A supertype for objects that “can be used as paint” to define the fill color(s) of a
  * shape. The simplest sort of fill is a [[Color]], but gradients and other more complex
  * fills may also be defined. */
trait Fill:

  /** Returns the corresponding `Paint` object for using this fill in Swing GUIs. */
  def swingPaint: Option[awt.Paint]

  /** Returns true if and only if the fill is completely transparent. */
  private[o1] def isTransparent: Boolean

  /** The fill’s average color. (For single-color fills, this will be the fill
    * object itself.) */
  private[o1] def averageColor: Color

end Fill


/** This companion object of class [[Fill]] provides a singleton object for
  * the special case where there is no fill at all. */
object Fill:

  /** The `Fill.None` singleton represents a fully transparent fill ---
    * that is, no visible fill at all. */
  object None extends Fill:
    /** For `Fill.None`, `isTransparent` is always `true`. */
    private[o1] val isTransparent = true
    /** The average color of `Fill.None` is [[o1.gui.colors.Transparent]]. */
    private[o1] val averageColor = Transparent
    /** `None`, since there is nothing to paint. */
    val swingPaint = scala.None

end Fill


