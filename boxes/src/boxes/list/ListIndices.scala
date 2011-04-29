package boxes.list

import boxes._
import util.WeakHashSet

class ListIndexReaction[T, LR<:ListRef[T]](listRef:Ref[LR], i:Var[Int]) extends Reaction {

  private val processedChanges = new WeakHashSet[ListChange]()

  def respond : (()=>Unit) = {
    println("ListReaction responding")
    var newI = i()
    val list = listRef()
    val size = list().size
    list.changes match {
      case None => {}
      case Some(queue) => {
        queue.foreach(listChange => {
          if (!processedChanges.contains(listChange)) {
            processedChanges.add(listChange)
            listChange match {
              case ReplacementListChange(f, l) => {}
              case InsertionListChange(ii, c) => if (ii <= newI) newI+=c
              case RemovalListChange(ii, c) => {
                //Deletion after selected index, no effect
                if (ii > newI) {
                  //Deletion entirely before selected index
                } else if (ii + c <= newI) {
                  newI -= c

                  //Deletion including the selected index, select first index
                  //after deletion, but still within list
                } else {
                  newI = ii
                  if (newI >= size) newI = size - 1
                }
              }
              case CompleteListChange() => newI = 0
            }
          }
        })
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
  def apply[T, LR<:ListRef[T]](listRef:Ref[LR]) = {
    val i = Var(0)
    val r = new ListIndexReaction[T, LR](listRef, i)
    i.retainReaction(r)
    Box.registerReaction(r)
    i
  }
}