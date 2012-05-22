package boxes

import scala.collection.mutable._

object Reaction {

  class SingleTargetReaction[T](v:VarGeneral[T, _], result: =>T) extends Reaction {

    def respond : (()=>Unit) = {
      //First apply the function, so that any reads are performed now
      val r = result

      //The write will be performed later
      {() => (v() = r)}
    }

    def isView = false

  }

  def apply[T](v:VarGeneral[T, _], result: =>T) = {
    val r = new SingleTargetReaction(v, result)
    Box.registerReaction(r)
    v.retainReaction(r)
    r
  }
}

object OptionalReaction {
  
  class OptionalSingleTargetReaction[T](v:VarGeneral[T, _], result: =>Option[T]) extends Reaction {

    def respond : (()=>Unit) = {
      //First apply the function, so that any reads are performed now
      val r = result

      //If there is a result, apply it later, otherwise do nothing
      result.map{ rVal => 
        {() => (v() = rVal)}
      }.getOrElse({() => ()})
    }

    def isView = false

  }

  def apply[T](v:VarGeneral[T, _], result: =>Option[T]) = {
    val r = new OptionalSingleTargetReaction(v, result)
    Box.registerReaction(r)
    v.retainReaction(r)
    r
  }

}

/**
 * One part of the boxes system, the other is Box.
 *
 * NEVER EVER change Reaction equals/hashCode methods - Reactions
 * MUST only be equal when they are the same (identical) Reaction.
 * This is because sets and maps are used to track them.
 */
trait Reaction {

  def respond : (()=>Unit)
  def isView : Boolean

  private[boxes] val sources = Set[Box[_]]()
  private[boxes] val targets = Set[Box[_]]()

}




