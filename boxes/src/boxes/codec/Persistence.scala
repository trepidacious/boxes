package boxes.codec

import collection._
import java.lang.reflect.{Method, Modifier}
import boxes.Var

object Persistence {

  private val classToMethods = new mutable.HashMap[Class[_], Map[String, Method]]

//  def varsOf(n:AnyRef) = Map(accessors(n).map(entry => entry._1 -> entry._2.invoke(n).asInstanceOf[Var[_]]): _*)

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
    //We want methods that take no parameters and return a Var, and are not
    //static, nor private, nor abstract
    val methods = c.getDeclaredMethods.toList.filter(m =>
                      classOf[Var[_]].isAssignableFrom(m.getReturnType)
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

/**
 * Codes objects to a DataTarget, and
 * decodes them from a DataSource
 */
trait Codec[T]{
  def decode(target : DataSource) :T
  def code(t : T, target : DataTarget) : Unit
}


/**
 * Handles encoding of an entire object
 */
class Coder(target:DataTarget) {
  def code(n:Any) = {
    target.putUTF(n.toString)
  }
}

object DefaultCodecs {
  implicit object IntCodec extends Codec[Int] {
    override def decode(target : DataSource) = target.getInt
    override def code(t : Int, target : DataTarget) = target.putInt(t)
  }
  implicit object ShortCodec extends Codec[Short] {
    override def decode(target : DataSource) = target.getShort
    override def code(t : Short, target : DataTarget) = target.putShort(t)
  }
  implicit object ByteCodec extends Codec[Byte] {
    override def decode(target : DataSource) = target.getByte
    override def code(t : Byte, target : DataTarget) = target.putByte(t)
  }
  implicit object LongCodec extends Codec[Long] {
    override def decode(target : DataSource) = target.getLong
    override def code(t : Long, target : DataTarget) = target.putLong(t)
  }
  implicit object FloatCodec extends Codec[Float] {
    override def decode(target : DataSource) = target.getFloat
    override def code(t : Float, target : DataTarget) = target.putFloat(t)
  }
  implicit object DoubleCodec extends Codec[Double] {
    override def decode(target : DataSource) = target.getDouble
    override def code(t : Double, target : DataTarget) = target.putDouble(t)
  }
  implicit object CharCodec extends Codec[Char] {
    override def decode(target : DataSource) = target.getChar
    override def code(t : Char, target : DataTarget) = target.putChar(t)
  }
  implicit object StringCodec extends Codec[String] {
    override def decode(target : DataSource) = target.getUTF
    override def code(t : String, target : DataTarget) = target.putUTF(t)
  }

}