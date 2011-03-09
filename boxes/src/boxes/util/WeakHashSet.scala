package boxes.util

import scala.collection.mutable.WeakHashMap

class WeakHashSet[A] extends scala.collection.mutable.Set[A] {
  val _map = new WeakHashMap[A,AnyRef]
  def contains(key: A): Boolean = _map.contains(key)
  def iterator: Iterator[A] = _map.keysIterator
  def +=(elem: A): this.type = { _map(elem) = Nil; this }
  def -=(elem: A): this.type = { _map -= elem; this }
  override def empty: this.type = { _map.empty; this }
  override def size = _map.size
}