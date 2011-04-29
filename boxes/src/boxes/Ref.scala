package boxes

/**
 * A Box containing a single value that can be read using apply()
 */
trait Ref[T] extends Box[T] {
  def apply():T
}

object Cal {
  def apply[T](c: =>T) = {
    val v = Var(c)
    Reaction(v, c)
    v.asInstanceOf[Ref[T]]
  }
}

object Val {
  def apply[T](t:T) = new ValDefault(t).asInstanceOf[Val[T]]
}

/**
 * Ref which is guaranteed immutable
 * Useful e.g. for fixed values for names, etc. when
 * a Ref is expected.
 */
trait Val[T] extends Ref[T]

private class ValDefault[T] (private val t:T) extends Val[T] {

  def apply():T = {
    try {
      Box.beforeRead(this)
      return t
    } finally {
      Box.afterRead(this)
    }
  }

  override def toString = "Val(" + t + ")"
}

object Var {
  def apply[T](t:T) = new VarDefault(t).asInstanceOf[Var[T]]
}

/**
 * Ref which is known to be mutable, and exposes mutator
 */
trait Var[T] extends Ref[T]{
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