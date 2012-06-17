package boxes.view
import scala.collection.mutable.WeakHashMap
import boxes.Box
import boxes.VarBox
import boxes.Cal

object Annotation{
  def apply[K, A](key: Box[K, _], annotationSource: Box[K=>A, _]) {
    val map = new WeakHashMap[K, A]()
    Cal{
      val k = key()
      val a = map.get(k).getOrElse{
        val newA = annotationSource().apply(k)
        map.put(k, newA)
        newA
      }
      a
    }
  }
}