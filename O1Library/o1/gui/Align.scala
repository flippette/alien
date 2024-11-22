package o1.gui

import Align.Generic.*

/** The `Align` singleton provides tools for describing the horizontal and vertical
  * alignment of graphical elements. */
object Align:

  /** Signals that elements should be vertically top-aligned. This is an alias for [[Align.Vertical.Top]]. */
  val Top     = Vertical.Top
  /** Signals that elements should be vertically centered. This is an alias for [[Align.Vertical.Middle]]. */
  val VMiddle = Vertical.Middle
  /** Signals that elements should be vertically bottom-aligned. This is an alias for [[Align.Vertical.Bottom]]. */
  val Bottom  = Vertical.Bottom

  /** Signals that elements should be horizontally left-aligned. This is an alias for [[Align.Horizontal.Left]]. */
  val Left    = Horizontal.Left
  /** Signals that elements should be horizontally centered. This is an alias for [[Align.Horizontal.Middle]]. */
  val HMiddle = Horizontal.Middle
  /** Signals that elements should be horizontally right-aligned. This is an alias for [[Align.Horizontal.Right]]. */
  val Right   = Horizontal.Right

  /** An enumeration of the three schemes for vertical alignment. */
  enum Vertical private(private[o1] val toGeneric: Generic) derives CanEqual:
    /** Signals that elements should be vertically top-aligned. */
    case Top    extends Vertical(TopOrLeft)
    /** Signals that elements should be vertically centered. */
    case Middle extends Vertical(EitherMiddle)
    /** Signals that elements should be vertically bottom-aligned. */
    case Bottom extends Vertical(BottomOrRight)

  /** An enumeration of the three schemes for horizontal alignment. */
  enum Horizontal private(private[o1] val toGeneric: Generic) derives CanEqual:
    /** Signals that elements should be horizontally left-aligned. */
    case Left   extends Horizontal(TopOrLeft)
    /** Signals that elements should be horizontally centered. */
    case Middle extends Horizontal(EitherMiddle)
    /** Signals that elements should be horizontally right-aligned. */
    case Right  extends Horizontal(BottomOrRight)

  /** An enumeration of the three generic schemes for aligning visual components,
    * without specifying whether the horizontal or vertical dimension is involved.
    * These generic values can be converted to either horizontal or vertical alignments. */
  enum Generic derives CanEqual:
    case TopOrLeft, EitherMiddle, BottomOrRight

    /** the [[Align.Horizontal horizontal alignment]] that corresponds to this generic alignment */
    lazy val toHorizontal: Horizontal = this match
      case TopOrLeft      => Horizontal.Left
      case EitherMiddle   => Horizontal.Middle
      case BottomOrRight  => Horizontal.Right

    /** the [[Align.Vertical vertical alignment]] that corresponds to this generic alignment */
    lazy val toVertical: Vertical = this match
      case TopOrLeft      => Vertical.Top
      case EitherMiddle   => Vertical.Middle
      case BottomOrRight  => Vertical.Bottom

end Align

