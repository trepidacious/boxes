package boxes.swing

import boxes.{Box, ListVar, VarGeneral}

class ListAddAction[T](l:ListVar[T], i:VarGeneral[Option[Int],_], source: => Option[T]) {
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

class ListMultiAddAction[T](l:ListVar[T], i:VarGeneral[Set[Int],_], source: => Option[T]) {
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

class ListDeleteAction[T](l:ListVar[T], i:VarGeneral[Option[Int],_], target:T => Unit) {
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

class ListMultiDeleteAction[T](l:ListVar[T], i:VarGeneral[Set[Int],_], target:T => Unit) {
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