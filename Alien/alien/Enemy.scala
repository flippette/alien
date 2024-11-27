package alien

class Enemy(val name: String, val killDescription: String):
  override def toString: String = this.name
end Enemy