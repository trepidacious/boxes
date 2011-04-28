package boxes.list

import boxes._
import util.WeakHashSet

class ListIndexReaction[T](list:ListRef[T], i:Var[Int]) extends Reaction {

  private val processedChanges = new WeakHashSet[ListChange]()

  def respond : (()=>Unit) = {
    println("ListReaction responding")
    var newI = i()
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
                println("removal ii " + ii + " c " + c + ", newI " + newI)
                //Deletion after selected index, no effect
                if (ii > newI) {
                  println("After selected")
                  //Deletion entirely before selected index
                } else if (ii + c <= newI) {
                  println("Before selected")
                  newI -= c

                  //Deletion including the selected index, select first index
                  //after deletion, but still within list
                } else {
                  println("Including selected")
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
  def apply[T](list:ListRef[T]) = {
    val i = Var(0)
    val r = new ListIndexReaction[T](list, i)
    i.retainReaction(r)
    Box.registerReaction(r)
    i
  }
}