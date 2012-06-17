package boxes

//A box which is known always to produce the same instance
//from apply()
trait ValBox[+T, C] extends Box[T, C]

//A box which is known to be mutable, with update method to mutate
//Also has convenience methods for applying reactions
trait VarBox[T, C] extends Box[T, C]{
  def update(newT:T)
  def <<(c: =>T) = Reaction(this, c)
  def <<?(c: =>Option[T]) = OptionalReaction(this, c)
}

trait Ref[T] extends Box[T, SingleChange[T]]
trait Var[T] extends VarBox[T, SingleChange[T]] with Ref[T]
trait Val[T] extends ValBox[T, SingleChange[T]] with Ref[T]

case class SingleChange[T] (oldValue: T, newValue: T)

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
      val oldT = t
      if (newT != t) {
        t = newT
        Box.commitWrite(this, SingleChange(oldT, newT))
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



