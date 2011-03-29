package boxes.util

import math.Numeric

trait Sequence[T] {
  def previous(t:T):T
  def next(t:T):T
}

class StepSequence[N](step:N)(implicit n:Numeric[N]) extends Sequence[N] {
  override def previous(a:N) = n.minus(a, step)
  override def next(a:N) = n.plus(a, step)
}

//d_m: scalabot: { import math.Numeric; def next[T](a:T)(implicit n:Numeric[T]) = n.plus(a, n.one); (next(3), next(3.0)) }
//[22:54] bens left the chat room. (Ping timeout: 252 seconds)
//[22:54] scalabot: (Int, Double) = (4,4.0)
