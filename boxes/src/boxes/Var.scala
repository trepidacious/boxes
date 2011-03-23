package boxes

object Var {
  def apply[T](t:T) = new VarDefault(t).asInstanceOf[Var[T]]
}

/**
 * Ref which is known to be mutable, and exposes mutator
 */
trait Var[T] extends Ref[T] {
  def update(newT:T)
}

private class VarDefault[T] (private var t:T) extends Var[T] {

  def update(newT:T) = {
    try {
      Box.beforeWrite(this)
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

  override def toString = "Var(" + t + ")"
}