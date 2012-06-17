package boxes.list

import collection._
import boxes._

//TODO this should have the old and new instances, there is no reason for it not to
//extend singlechange.
trait ListChange

/**
 * Anything in the list may have changed
 */
case class CompleteListChange extends ListChange

/**
 * Elements of the List may have been replaced, starting at firstIndex
 * and extending to lastIndex, inclusive. Note that not while elements
 * outside this range have NOT changed, elements inside the range may
 * not have changed.
 */
case class ReplacementListChange(firstIndex:Int, lastIndex:Int) extends ListChange

/**
 * Elements have been inserted into the list at a known index.
 */
case class InsertionListChange(index:Int, count:Int) extends ListChange

/**
 * Elements have been removed from the list at a known index
 */
case class RemovalListChange(index:Int, count:Int) extends ListChange

trait ListRef[T] extends Box[List[T], ListChange] {
  def apply(i:Int):T
  def map[U](f:T=>U) = {
    val u = ListVar(this().map(f))
    val r = new ListMapReaction(this, u, f)
    Box.registerReaction(r)
    u.retainReaction(r)
    u: ListRef[U]
  }
  def flatMap[U](f:T=>List[U]) = {
    val u = ListVar(this().flatMap(f))
    val r = new ListFlatMapReaction(this, u, f)
    Box.registerReaction(r)
    u.retainReaction(r)
    u: ListRef[U]
  }

}

trait ListVar[T] extends ListRef[T] with VarBox[List[T], ListChange] {
  def updateWithChanges(newT:List[T], c:ListChange*)
  def update(i:Int, e:T)
  def insert(i:Int, e:T*)
  def remove(i:Int, c:Int)
  override def <<(c: =>List[T]) = Reaction(this, c)
  override def <<?(c: =>Option[List[T]]) = OptionalReaction(this, c)
}

trait ListVal[T] extends ListRef[T] with ValBox[List[T], ListChange]

object ListUtils {
  def insert[T](l:List[T], i:Int, t:T*):List[T] = {
    val lb = mutable.ListBuffer(l:_*)
    lb.insert(i, t:_*)
    lb.toList
  }
  def remove[T](l:List[T], i:Int, c:Int):List[T] = {
    val lb = mutable.ListBuffer(l:_*)
    lb.remove(i, c)
    lb.toList
  }
  def replace[T](l:List[T], i:Int, t:T):List[T] = {
    val lb = mutable.ListBuffer(l:_*)
    lb.update(i, t)
    lb.toList
  }

}

object ListVal {
  def apply[T](elems:T*) = new ListValDefault[T](elems.toList).asInstanceOf[ListVal[T]]
  def apply[T](l:List[T]) = new ListValDefault[T](l).asInstanceOf[ListVal[T]]
}

private class ListValDefault[T] (private val t:List[T]) extends ListVal[T] {

  def apply():List[T] = {
    try {
      Box.beforeRead(this)
      return t
    } finally {
      Box.afterRead(this)
    }
  }
  def apply(i:Int):T = {
    try {
      Box.beforeRead(this)
      return t(i)
    } finally {
      Box.afterRead(this)
    }
  }
  override def toString = "ListVal(" + t + ")"
}

object ListVar {
  def apply[T](elems:T*) = new ListVarDefault[T](elems.toList).asInstanceOf[ListVar[T]]
  def apply[T](l:List[T]) = new ListVarDefault[T](l).asInstanceOf[ListVar[T]]
}

private class ListVarDefault[T] (private var t:List[T]) extends ListVar[T] {

  private def update(u : (List[T] => Option[(List[T], ListChange)])) = {
    try {
      Box.beforeWrite(this)
      u.apply(t) match {
        case None => {}
        case Some(listAndChange) => {
          t = listAndChange._1
          Box.commitWrite(this, listAndChange._2)
        }
      }
    } finally {
      Box.afterWrite(this)
    }
  }

  def updateWithChanges(newT:List[T], c:ListChange*) = {
    try {
      Box.beforeWrite(this)
      if (newT != t) {
        t = newT
        if (c.isEmpty) {
          Box.commitWrite(this, CompleteListChange())
        } else {
          Box.commitWrite(this, c:_*)
        }
      }
    } finally {
      Box.afterWrite(this)
    }
  }


  def insert(i:Int, e:T*) = update(list => Some((ListUtils.insert(list, i, e:_*), InsertionListChange(i, e.size))))

  def remove(i:Int, c:Int) = update(list => Some((ListUtils.remove(list, i, c), RemovalListChange(i, c))))

  def update(i:Int, e:T) {
    update(list => {
      if (e != list(i)) {
        Some((t.updated(i, e), ReplacementListChange(i, i)))
      } else {
        None
      }
    })
  }

  def update(newT:List[T]) {
    update(list => {
      if (newT != list) {
        Some((newT, CompleteListChange()))
      } else {
        None
      }
    })
  }

  def apply():List[T] = {
    try {
      Box.beforeRead(this)
      return t
    } finally {
      Box.afterRead(this)
    }
  }

  def apply(i:Int):T = {
    try {
      Box.beforeRead(this)
      return t(i)
    } finally {
      Box.afterRead(this)
    }
  }

  override def toString = "ListVar(" + t + ")"
}

trait ListReactionIncremental extends Reaction {

  private var lastProcessedChangeIndex = -1L

  protected def unprocessedChanges(b:Box[_,ListChange]) = {
    b.changes match {
      case None => immutable.Queue[ListChange]()
      case Some(q) => q.filter(indexAndChange => {
        if (indexAndChange._1 > lastProcessedChangeIndex) {
          lastProcessedChangeIndex = indexAndChange._1
          true
        } else {
          false
        }
      }).map(indexAndChange => indexAndChange._2)
    }
  }
  
}

class ListMapReaction[S, T](in: ListRef[S], out:ListVar[T], f : S => T) extends ListReactionIncremental {
  def isView = false
  //Note that changes can be passed through unaltered - they have the same scope
  def react() = out.updateWithChanges(in().map(f), unprocessedChanges(in):_*);
}

class ListFlatMapReaction[S, T](in: ListRef[S], out:ListVar[T], f : S => List[T]) extends ListReactionIncremental {
  def isView = false
  def react() = out.update(in().flatMap(f));
}

object ListCal {
  def apply[T](c: =>List[T]) = {
    val v = ListVar(c)
    Reaction(v, c)
    v.asInstanceOf[ListRef[T]]
  }
}
