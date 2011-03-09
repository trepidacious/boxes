package boxes

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
    Box.beforeRead(this)
    try {
      return t
    } finally {
      Box.afterRead(this)
    }
  }

  override def toString = "Val(" + t + ")"
}