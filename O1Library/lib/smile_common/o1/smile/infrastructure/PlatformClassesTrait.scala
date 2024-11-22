package o1.smile.infrastructure

import o1.smile.modeling.Bounds
import o1.smile.pictures.Text

trait PlatformClassesTrait:
  def bufferAdapter(width: Int, height: Int): BufferAdapter[?]

  val resourceFactory: ResourceFactory[?]

  val renderer: Renderer

  def textBoundaryCalculator(text: Text): Bounds
