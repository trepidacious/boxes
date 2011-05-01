package boxes

import util._

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

//When called with c as a TConverter, this is equivalent to PathReaction. With an OptionTConverter it
//is equivalent to PathOptionReaction
class PathReaction[T, G](v:VarGeneral[G,_], path : => Option[VarGeneral[T,_]], defaultValue:G, c:GConverter[G, T]) extends Reaction {
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

  def apply[T](path : =>VarGeneral[T,_]) = {
    //Find endpoint of path, and create new Var with same contained value
    val e = path
    val eVal = e()
    val v = Var(eVal)

    //Here we have both v and endpoint as parametric type T, so no need for
    //any converstion - use a TConverter. We do raise the path to an Option, but
    //since it always works we just use Some(path). Default value doesn't matter since
    //it is never used. Apologies for the null, but it really is NEVER used. Could
    //use eVal instead, but this potentially creates a memory leak.
    val r = new PathReaction[T, T](v, Some(path), null.asInstanceOf[T], new TConverter[T])
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
  def apply[T](path : => Option[VarGeneral[Option[T],_]]) = {
    val v:Var[Option[T]] = Var(None)
    //Not going to pretent this isn't confusing... here we use a TConverter
    //because we are producing v with parametric type Option[T], and using
    //a path to an endpoint with parametric type Option[T]. There is hence no
    //mismatch in levels of Option, and we don't need to convert anything. So
    //the "T" in "TConverter" is actually "Option[T]" in this case.
    val r = new PathReaction[Option[T], Option[T]](v, path, None, new TConverter[Option[T]])
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
  def apply[T](path : => Option[VarGeneral[T,_]]) = {
    val v:Var[Option[T]] = Var(None)
    //Here we have to raise values in our endpoint Var (of parametric type T)
    //up to Option[T], so we use an OptionTConverter.
    val r = new PathReaction[T, Option[T]](v, path, None, new OptionTConverter[T])
    Box.registerReaction(r)
    v.retainReaction(r)
    v
  }
}
