package boxes

import scala.collection.mutable._

object Reaction {

  class SingleTargetReaction[T](v:Var[T], result: =>T, name:String = "Unnamed Reaction") extends Reaction {

    def respond : (()=>Unit) = {
      //First apply the function, so that any reads are performed now
      val r = result

      //The write will be performed later
      {() => (v() = r)}
    }

    def isView = false

    override def toString = "STR: " + name

  }

  def apply[T](v:Var[T], result: =>T, name:String = "Unnamed Reaction") = {
    val r = new SingleTargetReaction(v, result, name)
    Box.registerReaction(r)
    //v.retainReaction(r)
    r
  }

}

trait Reaction {

  def respond : (()=>Unit)
  def isView : Boolean

  private[boxes] val sources = Set[Box]()
  private[boxes] val targets = Set[Box]()

}


