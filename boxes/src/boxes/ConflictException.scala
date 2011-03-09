package boxes

class ConflictException(r:Reaction, b:Box) extends RuntimeException {

  override def toString = "Conflicting reaction " + r + " on box " + b

}