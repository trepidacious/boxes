package boxes

/**
 * A Box containing a single value that can be read using apply()
 */
trait Ref[T] extends Box[T] {
  def apply():T
}