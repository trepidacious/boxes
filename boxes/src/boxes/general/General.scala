package boxes.general

import boxes._

object RadioReaction {
  def apply(options:Var[Boolean]*) = {
    val r = new RadioReaction(options:_*)
    Box.registerReaction(r)
    options.foreach(o => o.retainReaction(r))
    r
  }
}

class RadioReaction(options:Var[Boolean]*) extends Reaction {

  def aBeforeB(a:Box[_,_], b:Box[_,_]) = {
    val aIndex = a.firstChangeIndex
    val bIndex = b.firstChangeIndex

    bIndex match {

      //We have a write to b, compare a's change
      case Some(bIndexValue) => aIndex match {
        //We also have a write to a, which means a is first unless it has a later write
        case Some(aIndexValue) => aIndexValue <= bIndexValue

        //We have a write to b but not to a, so b is first
        case None => false
      }

      //No write to b, so a is first by default (even if it has no change)
      case None => true
    }
  }

  def respond : (()=>Unit) = {

    val activeOptions = options.filter(o => o())

    if (!activeOptions.isEmpty) {
      //Find the best option to leave enabled - it defaults to the first, but prefers the earliest-changed option, so that
      //user edits "stick"
      val best = activeOptions.foldLeft(activeOptions.head){(best, next) => if (aBeforeB(best, next)) best else next}

      {() => (activeOptions.foreach{ao => if (ao ne best) ao() = false})}
    } else {
      {() => ()}
    }
  }
  def isView = false
  def react {respond.apply()}
}

object TrueOp {
  def apply[T](v:VarBox[Boolean,_], canApply:Box[Boolean, _] = Val(true)) = new TrueOp[T](v, canApply)
}

class TrueOp[T](v:VarBox[Boolean,_], val canApply:Box[Boolean, _] = Val(true)) extends Op {
  def apply() {
    Box.transact{
      v() = true
    }
  }
}

object SetOp {
  def apply[T](v:VarBox[T, _], s:Box[T, _]) = new SetOp[T](v, s)
}

class SetOp[T](v:VarBox[T, _], s:Box[T, _]) extends Op {
  def apply() {
    Box.transact{
      v() = s()
    }
  }
  val canApply = Cal{v() != s()}
}
