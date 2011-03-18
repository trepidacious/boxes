package boxes

object Cal {
  def apply[T](c: =>T) = {
    val v = new Cal[T](c)
    Reaction(v, c)
    v.asInstanceOf[Ref[T]]
  }
}

/**
 * A Var that will accept writes only from Reactions.
 * May well be exposed as a plain Ref, with Reactions already
 * applied.
 */
class Cal[T] (private var t:T) extends Var[T] {

  def update(newT:T) = {
    try {
      if (!Box.beforeWrite(this)) {
        throw new RuntimeException("Can only write to a Cal from a Reaction");
      }
      if (newT != t) {
        t = newT
        Box.commitWrite(this, newT)
      }
    } finally {
      Box.afterWrite(this)
    }
  }

  def apply():T = {
    try {
      Box.beforeRead(this)
      return t
    } finally {
      Box.afterRead(this)
    }
  }

  override def toString = "Cal(" + t + ")"

}