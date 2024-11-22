package o1.smile.infrastructure

import o1.smile.pictures.{Bitmap, Picture, PictureElement}

object Renderer:
  def apply(): Renderer = PlatformSpecific.renderer

trait Renderer:
  def createBitmapFrom(elements: PictureElement*): Bitmap
  def createBitmapFrom(picture: Picture): Bitmap
