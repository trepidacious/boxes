package boxes

import util._
import collection._

private object PathUtils {
  def eToV(e:Box[_], v:Box[_]) = {
    val vIndex = v.firstChangeIndex
    val eIndex = e.firstChangeIndex

    vIndex match {
      //We have a write to v
      case Some(vIndexValue) => eIndex match {
        //We also have a write to e - if it came first, go from e to v
        case Some(eIndexValue) => eIndexValue < vIndexValue
        //We have a write to v, but not to e, so go from v to e
        case None => false
      }
      //No write to v, so go from e to v
      case None => true
    }
  }
}

class PathReaction[T, G, CT, CG](v:VarGeneral[G, CG], path : => Option[VarGeneral[T, CT]], defaultValue:G, c:GConverter[G, T]) extends Reaction {
  def respond : (()=>Unit) = {
    path match {
      //If the path is disconnected, revert to default
      case None => {()=>(v() = defaultValue)}

      //Otherwise do the mirroring
      case Some(e) => {
        //e() is type T, and v() is type G, so to compare we need to lift e() to type G
        val eContents = c.toG(e())
        val vContents = v()

        //If there is no change, nothing to do
        if (eContents == vContents) {
          {()=>()}

        } else {
          if (PathUtils.eToV(e, v)) {
            {() => (v() = eContents)}
          } else {
            //Take vContents of type G up to a definite Option, then we
            //can check whether it is None, and otherwise extract its
            //value as type T to copy to e
            c.toOption(vContents) match {
              case None => {() => (v() = eContents)}
              case Some(vValue) => {() => (e() = vValue)}
            }
          }
        }
      }
    }
  }
  def isView = false
}

object Path {

  def apply[T](path : =>Var[T]) = {
    //Find endpoint of path, and create new Var with same contained value
    val e = path
    val eVal = e()
    val v = Var(eVal)

    //Here we have both v and endpoint as parametric type T, so no need for
    //any converstion - use a TConverter. We do raise the path to an Option, but
    //since it always works we just use Some(path). Default value doesn't matter since
    //it is never used. Apologies for the null, but it really is NEVER used. Could
    //use eVal instead, but this potentially creates a memory leak.
    val r = new PathReaction[T, T, SingleChange[T], SingleChange[T]](v, Some(path), null.asInstanceOf[T], new TConverter[T])
    Box.registerReaction(r)
    v.retainReaction(r)
    v
  }

}

/**
 * Creates paths that go TO a Var[Option[T]], AND may also go VIA an option.
 * If the path goes via an option, it may yield None, in which case the
 * produced Var will contain None.
 * If the path yields Some(Var[Option[T]]) then the produced Var will
 * contain the value of the Var[Option[T]], which may be Some(tValue) or
 * None.
 *
 * Note this is (slightly confusingly) equivalent to PathWithDefault(path, None),
 * it just makes explicit that T from pathWithDefault is now Option[T], and the
 * defaultValue is None. This is probably the most common way of using a path
 * that leads to a Var[Option[T]].
 */
object PathToOption {
  def apply[T](path : => Option[Var[Option[T]]]) = {
    val v:Var[Option[T]] = Var(None)
    //Not going to pretent this isn't confusing... here we use a TConverter
    //because we are producing v with parametric type Option[T], and using
    //a path to an endpoint with parametric type Option[T]. There is hence no
    //mismatch in levels of Option, and we don't need to convert anything. So
    //the "T" in "TConverter" is actually "Option[T]" in this case.
    val r = new PathReaction[Option[T], Option[T], SingleChange[Option[T]], SingleChange[Option[T]]](v, path, None, new TConverter[Option[T]])
    Box.registerReaction(r)
    v.retainReaction(r)
    v
  }
}

/**
 * Creates paths that go VIA an option, but lead to a Var that contains a
 * nonoptional value. So this covers the case where following the path
 * may yield either None, OR a Var[T] for some non-optional type T.
 * To allow for this, the returned Var is a Var[Option[T]], which contains
 * Some(tValue) when the path yields a Var[T], and None when the
 * path yields None.
 */
object PathViaOption {
  def apply[T](path : => Option[Var[T]]) = {
    val v:Var[Option[T]] = Var(None)
    //Here we have to raise values in our endpoint Var (of parametric type T)
    //up to Option[T], so we use an OptionTConverter.
    val r = new PathReaction[T, Option[T], SingleChange[T], SingleChange[Option[T]]](v, path, None, new OptionTConverter[T])
    Box.registerReaction(r)
    v.retainReaction(r)
    v
  }
}

class ListPathReaction[T](v:ListVar[T], path : => Option[ListVar[T]]) extends Reaction {

  private var lastE:Option[ListVar[T]] = None
  private var lastProcessedChangeIndex = -1L

  private def unprocessedChanges(b:Box[ListChange]) = {
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

  def respond : (()=>Unit) = {

    path match {
      //If the path is disconnected, revert to default
      case None => {
        lastE = None;
        {()=>(v() = List[T]())}
      }

      //Otherwise do the mirroring
      case Some(e) => {
        //See whether we have changed endpoint ListVar since last time
        val eChanged = lastE match {
          case None => true
          case Some(lastEVal) => lastEVal ne e
        }
        lastE = Some(e)

        val eContents = e()
        val vContents = v()

        //If there is no change, nothing to do
        if (eContents.sameElements(vContents)) {
          {()=>()}


        //TODO remove all the duplication in following section
        } else {
          //If we have a new endpoint, just copy directly
          //and lose incremental changes - we are dealing with unsynced lists
          //so we cannot use incremental changes to get them up to date
          if (eChanged) {
            if (PathUtils.eToV(e, v)) {
              {() => (v() = eContents)}
            } else {
              {() => (e() = vContents)}
            }
          //Otherwise we just need to use any new incremental changes
          //to update from one list to the other
          } else {
            if (PathUtils.eToV(e, v)) {
              val changes = unprocessedChanges(e);
              {() => v.updateWithChanges(eContents, changes:_*)}
            } else {
              val changes = unprocessedChanges(v);
              {() => e.updateWithChanges(vContents, changes:_*)}
            }
          }
        }
      }
    }
  }
  def isView = false
}

/**
 * As for Path, but with a ListVar
 */
object ListPath {
  def apply[T](path : =>ListVar[T]) = ListPathViaOption(Some(path))
}

/**
 * As for PathViaOption, but with a ListVar
 */
object ListPathViaOption {
  def apply[T](path : => Option[ListVar[T]]) = {
    val e = path
    val v = ListVar[T](List())
    val r = new ListPathReaction[T](v, path)
    Box.registerReaction(r)
    v.retainReaction(r)
    v
  }
}

