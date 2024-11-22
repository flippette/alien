package o1.gui

import scala.swing.Swing.*
import scala.swing.*
import scala.swing.event.*

import o1.grid.{Grid, GridPos}
import o1.util.nice.number.*
import o1.gui.swingops.*
import scala.annotation.targetName
import scala.language.adhocExtensions // enable extension of Swing classes


/** A `BasicGridDisplay` object is a graphics component that displays a rectangular grid
  * as defined by a [[Grid]] object and provides mouse controls for manipulating the grid.
  *
  * **NOTE TO STUDENTS: In this course, you don’t need to understand how this class works
  * or can be used.**
  *
  * @param grid          the grid to be displayed
  * @param maxSquareSize the maximum width of a single square in the grid, in pixels.
  *                      The `BasiGridView` object scales itself to use most of the
  *                      available screen space, but won’t go over this limit */
private[o1] abstract class BasicGridDisplay[GridType <: Grid[Element], Element](val grid: GridType, maxSquareSize: Int) extends Component:
  display =>

  val popup: Popup

  val width: Int = grid.width
  val height: Int = grid.height

  val squareSize: Int = this.determineSquareSize(maxSquareSize)

  private val cells = this.createCells

  this.border = CompoundBorder(EmptyBorder(10, 10, 10, 10), EtchedBorder)
  this.preferredSize = Dimension(squareSize * width, squareSize * height)

  this.listenTo(this.mouse.clicks, this.mouse.moves)
  this.reactions += {

    case MouseReleased(_, point, modifiers, _, triggersPopup) =>
      if triggersPopup then
        popup.show(display, point.x, point.y)
      else if (modifiers & Key.Modifier.Meta) == 0 then
        for element <- this.elementAt(point) do
          this.elementClicked(element)
          this.update()

    case MousePressed(_, point, _, _, triggersPopup) =>
      if triggersPopup then
        popup.show(display, point.x, point.y)

    case MouseMoved(_, point, _) =>
      this.tooltip = this.elementAt(point).flatMap(this.tooltipFor).orNull

  }


  def elementClicked(element: Element): Unit


  private def determineSquareSize(upperLimit: Int) =
    val screenSize = this.toolkit.getScreenSize
    val widthMax = (screenSize.width - 175) / this.width
    val heightMax = (screenSize.height - 175) / this.height
    widthMax atMost heightMax atMost upperLimit atLeast 1

  private def createCells: Array[Array[Cell]] = Array.tabulate(height, width)( Cell(_, _) )


  override def paint(graphics: Graphics2D): Unit =
    for row <- this.cells; cell <- row do
      if graphics.getClip.intersects(cell.bounds) then
        cell.render(graphics)

  def update(): Unit =
    for row <- this.cells; cell <- row do
      if cell.updatePics() then
        this.repaint(cell.bounds)


  def elementVisuals(element: Element): Array[BufferedImage | Null]
  def missingElementVisuals: Array[BufferedImage | Null]

  def elementAt(point: Point): Option[Element] =
    this.posAtPoint(point).map( pos => this.grid(pos) )

  def posAtPoint(point: Point): Option[GridPos] =
    val pos = GridPos(point.x / this.squareSize, point.y / this.squareSize)
    if this.grid.contains(pos) then Some(pos) else None

  def tooltipFor(element: Element): Option[String] = None

  def scale(original: BufferedImage): BufferedImage = this.scale(original, this.squareSize)

  def scale(original: BufferedImage, targetSize: Int): BufferedImage =
    import java.awt.RenderingHints.*
    import java.awt.Transparency.*
    import BufferedImage.*

    def nextStep(dim: Int, target: Int) = target.max(dim / 2)

    val width = nextStep(original.getWidth, targetSize)
    val height = nextStep(original.getHeight, targetSize)

    val colorScheme = if original.getTransparency == OPAQUE then TYPE_INT_RGB else TYPE_INT_ARGB
    val scaled = BufferedImage(width, height, colorScheme)
    val tempGraphics = scaled.createGraphics()

    tempGraphics.setRenderingHint(KEY_INTERPOLATION, VALUE_INTERPOLATION_BICUBIC)
    original.draw(tempGraphics, x = 0, y = 0, width, height)
    tempGraphics.dispose()

    if width == targetSize && height == targetSize then scaled else this.scale(scaled, targetSize)
  end scale


  private class Cell(row: Int, column: Int):
    val pos = GridPos(column, row)
    val bounds = Rectangle(display.squareSize * pos.x, display.squareSize * pos.y, display.squareSize, display.squareSize)
    private var pics = Array[BufferedImage]()

    def element = Option(display.grid.elementAt(pos))

    def updatePics(): Boolean =
      val newPics = this.element.map(display.elementVisuals).getOrElse(display.missingElementVisuals)
      val hasChanged = !(newPics sameElements this.pics)
      if hasChanged then
        this.pics = newPics
      hasChanged

    def render(gridGraphics: Graphics2D): Unit =
      for
        cellPic <- this.pics
        if Option(cellPic).isDefined
      do
        val cellX = this.bounds.x + ((this.bounds.width - cellPic.width) / 2)
        val cellY = this.bounds.y + ((this.bounds.height - cellPic.getHeight) / 2)
        cellPic.draw(gridGraphics, cellX, cellY)

  end Cell


  protected open class Popup extends PopupMenu:
    protected var chosenGridPos: Option[GridPos] = None

    abstract class PopupAction(name: String) extends MenuItem(name):
      def applyAndUpdateView() =
        for c <- chosenGridPos do
          PopupAction.this.apply(c)
          display.update()
      this.action = Action(name)( applyAndUpdateView() )
      def isApplicable(pos: GridPos): Boolean
      def apply(pos: GridPos): Unit
    end PopupAction

    object PopupAction:
      val AlwaysApplicable: Any => Boolean = (a: Any) => true
    end PopupAction


    open class PosAction(name: String, val applies: GridPos => Boolean)(val perform: GridPos => Unit)
        extends PopupAction(name):

      override def isApplicable(pos: GridPos) = this.applies(pos)

      def apply(pos: GridPos): Unit =
        this.perform(pos)

    end PosAction


    open class ElementAction(name: String, val applies: Element => Boolean)(val perform: Element => Unit)
        extends PopupAction(name):

      override def isApplicable(pos: GridPos) = this.applies(display.grid(pos))

      def apply(pos: GridPos): Unit =
        this.perform(display.grid(pos))

    end ElementAction

    @targetName("add")
    def += (menuItem: Component): contents.type =
      this.contents += menuItem

    @targetName("addAll") 
    def ++= (menuActions: Seq[Component]): contents.type =
      this.contents ++= menuActions

    def chooseEnabled(): Unit =
      for here <- this.chosenGridPos; elem <- this.contents do elem match
        case popupAction: PopupAction => elem.enabled = popupAction.isApplicable(here)
        case otherwise                => // nothing

    override def show(component: Component, x: Int, y: Int): Unit =
      this.chosenGridPos = posAtPoint(Point(x, y))
      this.chooseEnabled()
      super.show(component, x, y)

  end Popup

end BasicGridDisplay

