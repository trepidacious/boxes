package boxes

trait RefGeneral[T, C] extends Box[C] {
  def apply():T
}

trait ValGeneral[T, C] extends RefGeneral[T, C]

trait VarGeneral[T, C] extends RefGeneral[T, C]{
  def update(newT:T)
}

trait Ref[T] extends RefGeneral[T, SingleChange[T]]
trait Var[T] extends VarGeneral[T, SingleChange[T]] with Ref[T]
trait Val[T] extends ValGeneral[T, SingleChange[T]] with Ref[T]

case class SingleChange[T] (newValue:T)

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
object Val {
  def apply[T](t:T) = new ValDefault(t).asInstanceOf[Val[T]]
}

private class VarDefault[T] (private var t:T) extends Var[T] {

  def update(newT:T) = {
    try {
      Box.beforeWrite(this)
      if (newT != t) {
        t = newT
        Box.commitWrite(this, SingleChange(newT))
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
object Var {
  def apply[T](t:T) = new VarDefault(t).asInstanceOf[Var[T]]
}

object Cal {
  def apply[T](c: =>T) = {
    val v = Var(c)
    Reaction(v, c)
    v.asInstanceOf[Ref[T]]
  }
}



