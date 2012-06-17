package boxes.list

import boxes._
import scala.math.{min, max}

class ListMoveOp[T](l:ListVar[T], i:VarBox[Option[Int],_], val up:Boolean) extends Op {

  def applicable() =
    i() match {
      case None => false
      case Some(someI) => (up && someI > 0) || (!up && someI < l().size-1)
    }

  val canApply = Cal{applicable}

  def apply() = {
    Box.transact{
      if (applicable) {
        i().foreach(firstI => {
          val secondI = firstI + (if(up) -1 else 1)
          val list = l()

          val minI = min(firstI, secondI)
          val maxI = max(firstI, secondI)

          l() = list.take(minI) ::: List(list(maxI)) ::: List(list(minI)) ::: list.takeRight(list.size-minI-2)
          i() = Some(secondI)
        })
      }
    }
  }
}

class ListMultiMoveOp[T](l:ListVar[T], i:VarBox[Set[Int],_], val up:Boolean) extends Op {

  def applicable() =
    if (i().size == 1) {
      val someI = i().head
      (up && someI > 0) || (!up && someI < l().size-1)
    } else {
      false
    }

  val canApply = Cal{applicable}

  def apply() = {
    if (applicable) {
      val firstI = i().head
      val secondI = firstI + (if(up) -1 else 1)

      val list = l()

      val minI = min(firstI, secondI)
      val maxI = max(firstI, secondI)

      l() = list.take(minI) ::: List(list(maxI)) ::: List(list(minI)) ::: list.takeRight(list.size-minI-2)
      i() = Set(secondI)
    }
  }
}

class ListAddOp[T](l:ListVar[T], i:VarBox[Option[Int],_], source: => Option[T]) extends Op {

  val canApply = Val(true)

  def apply() = {
    val t = source

    Box.transact{
      for (someT <- t) {
        val insertion = i() match {
          case Some(someI) => someI + 1
          case None => l().size
        }
        l.insert(insertion, someT)
        i() = Some(insertion)
      }
    }
    t
  }
}

class ListMultiAddOp[T](l:ListVar[T], i:VarBox[Set[Int],_], source: => Option[T]) extends Op {

  val canApply = Val(true)

  def apply() = {
    val t = source

    Box.transact{
      for (someT <- t) {
        val insertion = if (i().isEmpty) {
          l().size
        } else {
          i().toList.sorted.last + 1
        }
        l.insert(insertion, someT)
        i() = i() + insertion
      }
    }
    t
  }
}

class ListDeleteOp[T](l:ListVar[T], i:VarBox[Option[Int],_], target:T => Unit) extends Op {

  val canApply = Cal{i() != None}

  def apply() = {
    val removed = Box.transact{
      i() match {
        case Some(someI) => {
          val t = l(someI)
          l.remove(someI, 1)
          target(t)
        }
        case None => None
      }
    }
    removed
  }
}

class ListMultiDeleteOp[T](l:ListVar[T], i:VarBox[Set[Int],_], target:T => Unit) extends Op {

  val canApply = Cal{!i().isEmpty}

  def apply() = {
    Box.transact{
      val indices = i()
      if (!indices.isEmpty) {
        //Need to work backwards to preserve indices
        val sortedIndices = indices.toList.sorted.reverse
        sortedIndices.foreach(index => {
          val t = l(index)
          l.remove(index, 1)
          target(t)
        })
      }
    }
  }
}