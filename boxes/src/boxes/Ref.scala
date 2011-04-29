package boxes

trait Ref[T, C] extends Box[C] {
  def apply():T
}

trait Val[T, C] extends Ref[T, C]

trait Var[T, C] extends Ref[T, C]{
  def update(newT:T)
}

trait RefSingle[T] extends Ref[T, ChangeSingle[T]]
trait VarSingle[T] extends Var[T, ChangeSingle[T]] with RefSingle[T]
trait ValSingle[T] extends Val[T, ChangeSingle[T]] with RefSingle[T]

case class ChangeSingle[T] (newValue:T)

private class ValSingleDefault[T] (private val t:T) extends ValSingle[T] {
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
  def apply[T](t:T) = new ValSingleDefault(t).asInstanceOf[ValSingle[T]]
}

private class VarSingleDefault[T] (private var t:T) extends VarSingle[T] {

  def update(newT:T) = {
    try {
      Box.beforeWrite(this)
      if (newT != t) {
        t = newT
        Box.commitWrite(this, ChangeSingle(newT))
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
  def apply[T](t:T) = new VarSingleDefault(t).asInstanceOf[VarSingle[T]]
}

object Cal {
  def apply[T](c: =>T) = {
    val v = Var(c)
    Reaction(v, c)
    v.asInstanceOf[RefSingle[T]]
  }
}



