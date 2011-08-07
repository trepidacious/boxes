package boxes

import scala.collection.mutable._

object Reaction {

  class SingleTargetReaction[T](v:VarGeneral[T, _], result: =>T, name:String = "Unnamed Reaction") extends Reaction {

    def respond : (()=>Unit) = {
      //First apply the function, so that any reads are performed now
      val r = result

      //The write will be performed later
      {() => (v() = r)}
    }

    def isView = false

    override def toString = "STR: " + name

  }

  def apply[T](v:VarGeneral[T, _], result: =>T, name:String = "Unnamed Reaction") = {
    val r = new SingleTargetReaction(v, result, name)
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




