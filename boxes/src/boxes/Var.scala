package boxes

object Var {
  def apply[T](t:T) = new VarDefault(t).asInstanceOf[Var[T]]
}

object Path {

  class PathBIDIReaction[T](v:Var[T], path : =>Var[T], name:String) extends Reaction {
    def respond : (()=>Unit) = {

      //First work out the end of the path
      val e = path

      //If there is no change, nothing to do
      val eContents = e()
      val vContents = v()
      if (eContents == vContents) {
        {()=>()}

      //Something changed
      } else {

        //Now find which of v and e has changed first (if either)
        val vIndex = v.writeIndex
        val eIndex = e.writeIndex

        println("vIndex = " + vIndex + ", eIndex = " + eIndex)

        val fromEtoV = vIndex match {
          //We have a write to v
          case Some(vIndexValue) => eIndex match {

            //We also have a write to e - if it came first, go from e to v
            case Some(eIndexValue) => eIndexValue < vIndexValue

            //We have a write to v, but not to e, so go from v to e
            case None => false
          }

          //No write to v, so go from e to v
          case None => true
        }

        if (fromEtoV) {
          println("Copying from v to e");
          {() => (v() = eContents)}
        } else {
          println("Copying from e to v");
          {() => (e() = vContents)}
        }
      }
    }

    def isView = false

    override def toString = "PathBIDI: " + name

  }

  def apply[T](path : =>Var[T], name:String = "Unnamed") = {
    //Find endpoint of path, and create new Var with same contained value
    val e = path
    val v = Var(e())
    val r = new PathBIDIReaction[T](v, path, name)
    Box.registerReaction(r)
    //v.retainReaction(r)
    v
  }
}

/**
 * Ref which is known to be mutable, and exposes mutator
 */
trait Var[T] extends Ref[T] {
  def update(newT:T)
}

private class VarDefault[T] (private var t:T) extends Var[T] {

  def update(newT:T) = {
    Box.beforeWrite(this)
    try {
      if (newT != t) {
        t = newT
        Box.commitWrite(this, newT)
      }
    } finally {
      Box.afterWrite(this)
    }
  }

  def apply():T = {
    Box.beforeRead(this)
    try {
      return t
    } finally {
      Box.afterRead(this)
    }
  }

  override def toString = "Var(" + t + ")"
}