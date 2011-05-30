package boxes.list

import boxes._

class ListIndexReaction[T](list:ListRef[T], i:Var[Option[Int]], loseIndexOnDeletion:Boolean, selectFirstRatherThanNone:Boolean) extends Reaction {

  private var lastProcessedChangeIndex = -1L

  private def updateIndex(optionI:Option[Int], change:ListChange) = optionI match {

    //No selection remains no selection, under all list changes
    case None => None

    //Some selection is updated
    case Some(i) => change match {
      case ReplacementListChange(_,_) => Some(i)

      case InsertionListChange(insertion, count) => if (insertion <= i) Some(i+count) else Some(i)

      case RemovalListChange(removal, count) => {
        //Deletion after selected index, no effect
        if (removal > i) {
          Some(i)

        //Deletion entirely before selected index
        } else if (removal + count <= i) {
          Some(i-count)

        //Deletion including the selected index
        } else {
          if (loseIndexOnDeletion) {
            None
          //Select the next index AFTER the deleted range.
          //Note that this may put the index AFTER the end of the list,
          //if the deletion covered from the selected index to
          //the end of the list. This case is still valid, since our
          //selection will remain after then end of the list during
          //any possible subsequent changes. After changes have been applied,
          //we will then check for this case, and respond by moving the
          //selection to the end of the list, or None if the list is empty.
          //This is the desired behaviour, since it allows us not to lose the
          //index on such deletions, instead selecting the nearest still
          //existing index.
          } else {
            Some(removal)
          }
        }
      }

      //For a complete change, we reset the index
      case CompleteListChange() => None
    }

  }

  def respond : (()=>Unit) = {
    var newI = i()
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
        newI = updateIndex(newI, indexAndChange._2)
      }
    }

    newI match {
      //Override selection of None if requested and possible
      case None => if (selectFirstRatherThanNone && size > 0) newI = Some(0)

      //Check value
      case Some(newIValue) => {
        if (newIValue < 0) throw new RuntimeException("Index should never adjust to less than 0")

        //Empty list can't have a selection
        if (size == 0) {
          newI = None

        //Selection after end of list indicates a deletion from selection to end of list, response
        //is to select end of list - see above in updateIndex for more details
        } else if (newIValue >= size) {
          newI = Some(size-1)
        }
      }
    }

    val r = newI

    //The write will be performed later
    {() => (i() = r)}
  }

  def isView = false
}


object ListIndex {
  def apply[T](listRef:ListRef[T], initialIndex:Option[Int] = Some(0), loseIndexOnDeletion:Boolean = false, selectFirstRatherThanNone:Boolean = true) = {
    val i = Var(initialIndex)
    val r = new ListIndexReaction[T](listRef, i, loseIndexOnDeletion, selectFirstRatherThanNone)
    i.retainReaction(r)
    Box.registerReaction(r)
    i
  }
}

object ListSelection {
  def apply[T](l:ListRef[T], i:Var[Option[Int]]) = Cal(for (index <- i() if index < l().size) yield l(index))
}

class ListIndicesReaction[T](list:ListRef[T], indices:Var[Set[Int]], selectFirstRatherThanNone:Boolean) extends Reaction {

  private var lastProcessedChangeIndex = -1L

  private def updateIndices(is:Set[Int], change:ListChange) = {

    change match {

      case ReplacementListChange(_,_) => is

      case InsertionListChange(insertion, count) => is.map{
        i => if (insertion <= i) i+count else i
      }

      //TODO
      //TODO we should have a special case so that if the last selected index
      //is deleted, and we don't want to lose index on deletion, then we select after the
      //deletion range. Only do this when we run out of selections though, otherwise it could
      //be irritating

      //filter out any indices in the deleted range, then adjust those after the range to move them back
      case RemovalListChange(removal, count) => is.collect{
        //Before deletion, no effect
        case i if removal > i => i
        //After deletion, move index back
        case i if (removal + count <= i) => i-count
        //Throw stuff in deletion range away
      }

      //For a complete change, we lose the selection
      case CompleteListChange() => Set[Int]()
    }
  }

  def respond : (()=>Unit) = {
    var newIndices = indices()
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
        newIndices = updateIndices(newIndices, indexAndChange._2)
      }
    }

    //Empty list has no selection
    if (size == 0) {
      newIndices = Set[Int]()

    //If empty, select if requested
    } else if (newIndices.isEmpty && selectFirstRatherThanNone) {
      newIndices = Set[Int](0)
    }


    //Filter out invalid selections
    newIndices = newIndices.filter(i => i >= 0 && i < size)

    val r = newIndices

    //The write will be performed later
    {() => (indices() = r)}
  }

  def isView = false
}

object ListIndices {
  def apply[T](listRef:ListRef[T], initialIndices:Set[Int] = Set(0), selectFirstRatherThanNone:Boolean = true) = {
    val i = Var(initialIndices)
    val r = new ListIndicesReaction[T](listRef, i, selectFirstRatherThanNone)
    i.retainReaction(r)
    Box.registerReaction(r)
    i
  }
}
