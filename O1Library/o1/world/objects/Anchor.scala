package o1.world.objects

import o1.world.Pos

/** An `Anchor` is a point where a two-dimensional object, such as a [[o1.gui.Pic Pic]], attaches to
  * other such objects; you can also think of it as a “pin” that attaches a `Pic` to a background.
  *
  * For instance, a `Pic` may have its `Anchor` at its center; the `Pic` may then be attached
  * to a larger image at that image’s `TopLeft` (another anchor), so that the center of the
  * first `Pic` is located at the top left-hand corner of the second `Pic`.
  *
  * The [[Anchor$ companion object]] of this class provides several standard `Anchor`s and
  * a class [[Anchor$.Absolute `Anchor.Absolute`]] for creating new ones.
  *
  * `Anchor` objects are immutable.
  *
  * This trait has an alias in the top-level package [[o1]], so it’s accessible to students simply
  * via `import o1.*`.
  *
  * @see [[HasAnchor]] */
trait Anchor derives CanEqual:

  /** Returns the x coordinate of the anchoring point within the given `Pic` or other anchorable object,
    * relative to the object’s left edge. */
  def internalXWithin(anchored: HasAnchor): Double

  /** Returns the y coordinate of the anchoring point within the given `Pic` or other anchorable object,
    * relative to the object’s top edge. */
  def internalYWithin(anchored: HasAnchor): Double

  /** Returns the [[Pos]] of the anchoring point within the given `Pic` or other anchorable object,
    * relative to the object’s top left-hand corner. */
  def internalPosWithin(anchored: HasAnchor): Pos = Pos(this.internalXWithin(anchored), this.internalYWithin(anchored))

  /** Returns an [[Anchor$.Absolute absolute anchor]] the [[Pos]] of the anchoring point within the
    * given `Pic` or other anchorable object, relative to the object’s top left-hand corner. */
  def toAbsoluteWithin(anchored: HasAnchor): Anchor.Absolute = Anchor.Absolute(this.internalPosWithin(anchored))

end Anchor



/** This companion object of [[Anchor class `Anchor`]] contains a number of standard anchors
  * (`TopLeft`, `Center`, etc.) and a class for creating anchors in a custom location.
  *
  * The object has an alias in the top-level package [[o1]], so it’s accessible to students
  * simply via `import o1.*`.
  *
  * @see [[HasAnchor]] */
object Anchor:

  private[world] transparent trait Left    { def internalXWithin(anchored: HasAnchor) = 0.0 }
  private[world] transparent trait HCenter { def internalXWithin(anchored: HasAnchor) = anchored.centerFromTopLeft.x }
  private[world] transparent trait Right   { def internalXWithin(anchored: HasAnchor) = anchored.width }
  private[world] transparent trait Top     { def internalYWithin(anchored: HasAnchor) = 0.0 }
  private[world] transparent trait VCenter { def internalYWithin(anchored: HasAnchor) = anchored.centerFromTopLeft.y }
  private[world] transparent trait Bottom  { def internalYWithin(anchored: HasAnchor) = anchored.height }

  /** An anchor at the top left-hand corner. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object TopLeft extends Anchor, Top, Left

  /** An anchor at the middle of the top edge. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object TopCenter extends Anchor, Top, HCenter

  /** An anchor at the top right-hand corner. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object TopRight extends Anchor, Top, Right

  /** An anchor at the middle of the left edge. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object CenterLeft extends Anchor, VCenter, Left

  /** An anchor at the middle. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object Center extends Anchor, VCenter, HCenter

  /** An anchor at the middle of the right edge. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object CenterRight extends Anchor, VCenter, Right

  /** An anchor at the bottom left-hand corner. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object BottomLeft extends Anchor, Bottom, Left

  /** An anchor at the middle of the bottom edge. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object BottomCenter extends Anchor, Bottom, HCenter

  /** An anchor at the bottom right-hand corner. This object has an alias in the top-level
    * package [[o1]], so it’s accessible to students simply via `import o1.*`. */
  case object BottomRight extends Anchor, Bottom, Right


  /** An anchor at a custom [[Pos]].
    * @param deltaFromTopLeft  the [[Pos]] of the anchor within the anchored object,
    *                          relative to the object’s top left-hand corner */
  final case class Absolute(private val deltaFromTopLeft: Pos) extends Anchor:

    /** Returns the x coordinate of the anchoring point within the given `Pic` 
      * or other anchorable object, relative to the object’s left edge. */
    def internalXWithin(anchored: HasAnchor) = this.deltaFromTopLeft.x

    /** Returns the y coordinate of the anchoring point within the given `Pic` 
      * or other anchorable object, relative to the object’s top edge. */
    def internalYWithin(anchored: HasAnchor) = this.deltaFromTopLeft.y

    /** Returns a `String` description of the absolute anchor. */
    override def toString = s"$deltaFromTopLeft from top left"

  end Absolute

end Anchor

