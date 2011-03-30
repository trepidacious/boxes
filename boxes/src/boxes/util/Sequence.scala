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
