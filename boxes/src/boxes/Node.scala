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
    //We want methods that take no parameters and return a VarGeneral, and are not
    //static, nor private, nor abstract
    val methods = c.getDeclaredMethods.toList.filter(m =>
                      classOf[VarGeneral[_,_]].isAssignableFrom(m.getReturnType)
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

//Just a marker trait for things that want to be serialised as Nodes, using accessors
trait Node