package boxes.list

import boxes._

class ListIndexReaction[T, LR<:ListRef[T]](listRef:RefGeneral[LR,_], i:Var[Int]) extends Reaction {

  private var lastProcessedChangeIndex = -1L

  private def updateIndex(i:Int, size:Int, change:ListChange) = change match {

    case ReplacementListChange(_,_) => i

    case InsertionListChange(insertion, count) => if (insertion <= i) i+count else i

    case RemovalListChange(removal, count) => {
      //Deletion after selected index, no effect
      if (removal > i) {
        i

      //Deletion entirely before selected index
      } else if (removal + count <= i) {
        i-count

      //Deletion including the selected index, select first index
      //after deletion, but still within list
      } else {
        if (removal >= size) size - 1 else removal
      }
    }

    case CompleteListChange() => 0
  }

  def respond : (()=>Unit) = {
    var newI = i()
    val list = listRef()
    val size = list().size

    for {
      queue <- list.changes
      indexAndChange <- queue
    } {
      //Only process changes newer than the last one we've already processed.
      //Note that we know we will get changes to the list in the order they are made, with
      //increasing indices.
      if (indexAndChange._1 > lastProcessedChangeIndex) {
        lastProcessedChangeIndex = indexAndChange._1
        newI = updateIndex(newI, size, indexAndChange._2)
      }
    }

    //If we are now pointing at a completely new ListRef, then reset selection
    listRef.changes match {
      case Some(_) => newI = 0
      case None => {}
    }

    //Check selection is in range
    if (newI < 0) newI = 0
    //Note that if size = 0, we will have index -1, no selection. This is intended.
    if (newI >= size) newI = size - 1

    val r = newI

    //The write will be performed later
    {() => (i() = r)}
  }

  def isView = false
}


object ListIndex {
  def apply[T, LR<:ListRef[T]](listRef:RefGeneral[LR,_]) = {
    val i = Var(0)
    val r = new ListIndexReaction[T, LR](listRef, i)
    i.retainReaction(r)
    Box.registerReaction(r)
    i
  }
}