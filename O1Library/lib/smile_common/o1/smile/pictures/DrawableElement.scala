package o1.smile.pictures

import o1.world.Pos

private[pictures] trait DrawableElement extends PictureElement:

  override def copy(newPosition: Pos): DrawableElement

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): DrawableElement

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double, relativityPoint: Pos): DrawableElement

  override def rotateAround(pivot: Pos, degrees: Double): DrawableElement

  override def rotateAroundOrigin(degrees: Double): DrawableElement

end DrawableElement
