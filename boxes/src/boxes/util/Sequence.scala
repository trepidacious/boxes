package boxes.util

import math.Numeric
import scala.math._

trait Sequence[T] {
  def previous(t:T):T
  def next(t:T):T
}

object Step {
  def apply[N](step:N)(implicit n:Numeric[N]) = new Step[N](step, n)
}

object LogStep {
  def apply(divisor:Double, minStep:Double = 0.01)= new LogStep(divisor, minStep)
}

class Step[N](step:N, n:Numeric[N]) extends Sequence[N] {
  override def previous(a:N) = n.minus(a, step)
  override def next(a:N) = n.plus(a, step)
}

class LogStep(divisor:Double, minStep:Double) extends Sequence[Double] {
  val logDivisor = log10(abs(divisor))
  val logTwo = log10(2*divisor)
  val min = math.abs(minStep * divisor)
  def increment(x:Double) = {
    val a = max(abs(x), min)
    val l = log10(a)
    val f = l.floor

    //This is the "exact" increment, but if we are close
    //to the next increment, go up to it. Specifically, if even
    //half of the exact increment would get us up to a larger
    //increment, then use the larger increment. So for example
    //99.9999999 should be treated as 100, even though it has a
    //log10 slightly less than 2, so would increment like 99. This
    //avoids counting like 99.8, 99.9, 99.9999999999 (displayed as 100), 100.1, 101.1 etc.
    //and gives correct sequence 99.8, 99.9, 99.99999999999, 101, 102, etc.
    //TODO There must be a simpler way!
    val i = pow(10, f-logDivisor)
    if (a + i/2 > pow(10, f+1)) pow(10, f+1-logDivisor) else i
  }
  override def previous(x:Double) = x - increment(x)
  override def next(x:Double) = x + increment(x)
}

