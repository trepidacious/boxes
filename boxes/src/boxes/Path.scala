package boxes

class PathBIDIReaction[T](v:Var[T], path : => Option[Var[T]], defaultValue:T) extends Reaction {
  def respond : (()=>Unit) = {

    //First work out the end of the path
    val optionE = path

    path match {
      //If the path is disconnected, revert to default
      case None => {()=>(v() = defaultValue)}

      //Otherwise do the mirroring
      case Some(e) => {
        val eContents = e()
        val vContents = v()

        //If there is no change, nothing to do
        if (eContents == vContents) {
          {()=>()}

        //Something changed
        } else {

          //Now find which of v and e has changed first (if either)
          val vIndex = v.writeIndex
          val eIndex = e.writeIndex

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
            {() => (v() = eContents)}
          } else {
            {() => (e() = vContents)}
          }
        }
      }
    }
  }

  def isView = false

  override def toString = "PathBIDI"

}

class PathBIDIOptionReaction[T](v:Var[Option[T]], path : => Option[Var[T]]) extends Reaction {
  def respond : (()=>Unit) = {

    //First work out the end of the path
    val optionE = path

    path match {
      case None => {()=>(v() = None)}

      //Otherwise do the mirroring
      case Some(e) => {
        val eContents = e()
        val vContents = v()

        //If there is no change, nothing to do
        if (eContents == vContents) {
          {()=>()}

        //Something changed
        } else {

          //Now find which of v and e has changed first (if either)
          val vIndex = v.writeIndex
          val eIndex = e.writeIndex

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

          //We can always copy from e to v
          if (fromEtoV) {
            {() => (v() = Some(eContents))}

          //If v is None, we can't copy to e
          } else {
            vContents match {
              case None => {()=>()}
              case Some(vValue) => {() => (e() = vValue)}
            }
          }
        }
      }
    }
  }

  def isView = false

  override def toString = "PathBIDI"

}

object Path {

  def apply[T](path : =>Var[T]) = {
    //Find endpoint of path, and create new Var with same contained value
    val e = path
    val eVal = e()
    val v = Var(eVal)
    val r = new PathBIDIReaction[T](v, Some(path), eVal)  //Note that in this case, default doesn't matter
                                                          //since Option is always Some
    Box.registerReaction(r)
    v
  }

}

object PathWithDefault {
  def apply[T](path : => Option[Var[T]], defaultValue:T) = {
    val v = Var(defaultValue)
    val r = new PathBIDIReaction[T](v, path, defaultValue)
    Box.registerReaction(r)
    v
  }
}

object PathWithOption {
  def apply[T](path : => Option[Var[T]]) = {
    val v:Var[Option[T]] = Var(None)
    val r = new PathBIDIOptionReaction[T](v, path)
    Box.registerReaction(r)
    v
  }
}
