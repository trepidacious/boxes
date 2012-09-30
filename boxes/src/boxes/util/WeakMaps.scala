package boxes.util

import scala.collection._
import scala.ref.WeakReference
import scala.ref.ReferenceQueue
import scala.collection.mutable.WeakHashMap

private class WeakValueReference[K, V <: AnyRef](val key: K, v: V, q: Option[ReferenceQueue[V]]=None) 
  extends WeakReference[V](v, q.getOrElse(null))

class WeakValuesHash[K, V <: AnyRef] {

  private val map = mutable.Map[K, WeakValueReference[K, V]]()
  private val refQueue = new ReferenceQueue[V]()

  def get(key: K) = {
    clearQueue()
    map.get(key).flatMap(_.get)
  }

  def put(key: K, v: V) = {
    clearQueue()

    //Note we use the reference queue only here, since we only
    //care about these references being GCed. Others are just
    //used temporarily for lookup purposes.
    val vRef = new WeakValueReference(key, v, Some(refQueue))
    map.put(key, vRef).flatMap(_.get)
  }

  def remove(key: K) = {
    clearQueue()
    map.remove(key).flatMap(_.get)
  }

  //Remove the mappings for all collected references
  def clearQueue() {
    val toClear = Iterator.continually(refQueue.poll).takeWhile(_ != None)
    toClear.foreach(_.foreach{wvr => map.remove(wvr.asInstanceOf[WeakValueReference[K, V]].key)})
  }

}

class WeakKeysBIDIMap[K <: AnyRef, V <: AnyRef] {
  private val forward = new WeakHashMap[K, V]
  private val backward = new WeakValuesHash[V, K]()

  def toValue(key: K) = forward.get(key)
  def toKey(value: V) = backward.get(value)

  def put(key: K, value: V) = {
    (backward.put(value, key), forward.put(key, value))
  }
  def removeKey(key: K) = {
    val v = forward.remove(key)
    v.foreach(backward.remove(_))
    v
  }
}
