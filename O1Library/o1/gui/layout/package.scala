/** The package `o1.gui.layout` contains utilities that make it more convenient to lay out
  * components in simple [[scala.swing.GridBagPanel GridBagPanel]]s from Scala’s Swing library.
  * These utilities are used internally by some of the given GUIs in O1.
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this package works
  * or can be used.** */
package o1.gui.layout

import scala.swing.GridBagPanel.Anchor.*
import scala.swing.*
import java.awt.Insets
import scala.language.adhocExtensions // enable extension of Swing classes

type Anchor = GridBagPanel.Anchor.Value

val OneSlot:   (Int, Int) = (1, 1)
val TwoWide:   (Int, Int) = (2, 1)
val ThreeWide: (Int, Int) = (3, 1)
val TwoHigh:   (Int, Int) = (1, 2)
val ThreeHigh: (Int, Int) = (1, 3)

type Fill = (GridBagPanel.Fill.Value, Int, Int)

def NoFill(xWeight: Int, yWeight: Int): Fill   = (GridBagPanel.Fill.None,       xWeight, yWeight)
def FillVertical(weight: Int): Fill            = (GridBagPanel.Fill.Vertical,         0,  weight)
def FillHorizontal(weight: Int): Fill          = (GridBagPanel.Fill.Horizontal,  weight,       0)
def FillBoth(xWeight: Int, yWeight: Int): Fill = (GridBagPanel.Fill.Both,       xWeight, yWeight)

val Slight: Fill = NoFill(0, 0)

val NoBorder: (Int, Int, Int, Int) = (0, 0, 0, 0)


/** The methods on this trait make it easier to place elements in a Swing container.
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this trait
  * works or can be used.** */
trait EasyLayout extends LayoutContainer:

  def constraintsFor(component: Component, xy: (Int, Int), size: (Int, Int), anchor: Anchor, fill: Fill, border: (Int, Int, Int, Int)): Constraints

  def place(component: Component, xy: (Int, Int), size: (Int, Int), anchor: Anchor, fill: Fill, border: (Int, Int, Int, Int)): Unit =
    layout += component -> constraintsFor(component, xy, size, anchor, fill, border)

  def placeC(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, Center, fill, border)

  def placeN(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, North, fill, border)

  def placeE(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, East, fill, border)

  def placeS(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, South, fill, border)

  def placeW(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, West, fill, border)

  def placeNW(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, NorthWest, fill, border)

  def placeNE(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, NorthEast, fill, border)

  def placeSW(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, SouthWest, fill, border)

  def placeSE(component: Component, xy: (Int, Int), size: (Int, Int), fill: Fill, border: (Int, Int, Int, Int)): Unit =
    this.place(component, xy, size, SouthEast, fill, border)

end EasyLayout


/** The methods on this trait make it easier to place elements in a [[scala.swing.GridBagPanel]].
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this trait works or can
  * be used.** */
trait EasyPanel extends GridBagPanel, EasyLayout:
  def constraintsFor(component: Component, xy: (Int, Int), size: (Int, Int), anchor: Anchor, fill: Fill, border: (Int, Int, Int, Int)): Constraints =
    val insets = Insets(border(0), border(1), border(2), border(3))
    Constraints(gridx = xy(0), gridy = xy(1), gridwidth = size(0), gridheight = size(1),
                weightx = fill(1), weighty = fill(2), anchor.id, fill(0).id, insets, ipadx = 0, ipady = 0)

