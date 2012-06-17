package boxes.util

//TODO Consider using Either[Box[T], Box[Option[T]] for the model type, then cases instead of converter

//Type G may be Option[T] or the bare type T.
//toOption gets us from G to DEFINITELY an Option[T]
//toG gets us from the bare type T to G
//This means we can have a Var[Option[T]] or a Var[T] in
//a view, and use the converter to treat it as a Var[Option[T]]
//in a fairly straight forward way.
trait GConverter[G, T] {
  def toOption(g:G):Option[T]
  def toG(t:T):G
}

//GConverter for where G is just T
class TConverter[T] extends GConverter[T, T] {
  override def toOption(g:T):Option[T] = Some(g)
  override def toG(t:T):T = t
}

//GConverter for where G is Option T
class OptionTConverter[T] extends GConverter[Option[T], T] {
  override def toOption(g:Option[T]):Option[T] = g
  override def toG(t:T):Option[T] = Some(t)
}