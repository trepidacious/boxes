package boxes

import collection._
import java.lang.reflect.{Method, Modifier}

object Node {

  private val classToMethods = new mutable.HashMap[Class[_], Map[String, Method]]

  def accessors(n:AnyRef) = accessorsOfClass(n.getClass)

  def accessorsOfClass(c:Class[_]) = {
    this.synchronized {
      classToMethods.get(c) match {
        case None => {
          val m = accessorMap(c)
          classToMethods.put(c, m)
          m
        }
        case Some(m) => m
      }
    }
  }

  private def accessorMap(c:Class[_]) = {
    //We want methods that take no parameters and return a VarBox, and are not
    //static, nor private, nor abstract
    val methods = c.getMethods.toList.filter(m =>
                      classOf[VarBox[_,_]].isAssignableFrom(m.getReturnType)
                      && m.getParameterTypes.length == 0
                      && !Modifier.isStatic(m.getModifiers)
                      && !Modifier.isPrivate(m.getModifiers)
                      && !Modifier.isAbstract(m.getModifiers)
    )
    //Map from name to accessor method            //
    val map = Map(methods.map(m => m.getName -> m): _*)
    map
  }
}

//Nodes contain Refs provided read-only accessor methods, and these constitute the whole of
//the node's state. Any state that must be persisted is in Vars.
//Allows for retaining/releasing of reactions that are required to keep the Node in
//a valid state, or synchronise it with storage, etc.
trait Node {
  private val retainedReactions = mutable.Set[Reaction]()

  def retainReaction(r:Reaction) = retainedReactions.add(r)
  def releaseReaction(r:Reaction) = retainedReactions.remove(r)
}