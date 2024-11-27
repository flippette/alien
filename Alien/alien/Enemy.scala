package alien

class Enemy(val name: String, val killDescription: String):
  def takeTurn(): Unit = ()
  override def toString: String = this.name
end Enemy