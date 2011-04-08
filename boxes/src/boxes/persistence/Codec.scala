package boxes.persistence

import collection._
import boxes.Var

/**
* Codes objects to a DataTarget, and
* decodes them from a DataSource
*/
trait Codec[T] {
  def decode(target : DataSource) : T
  def code(t : T, target : DataTarget) : Unit
}

trait Node

class CodecByClass extends Codec[Any] {

  private val root = new CodecNode(AnyCodec, classOf[Any])
  private val codecs = mutable.Set[Codec[_]]()

  {
    val int:Int = 1
    val short:Short = 1
    val byte:Byte = 1
    val long:Long = 1
    val float:Float = 1
    val char:Char = 1
    add(IntCodec, int.asInstanceOf[AnyRef].getClass)
    add(ShortCodec, short.asInstanceOf[AnyRef].getClass)
    add(ByteCodec, byte.asInstanceOf[AnyRef].getClass)
    add(LongCodec, long.asInstanceOf[AnyRef].getClass)
    add(FloatCodec, float.asInstanceOf[AnyRef].getClass)
    add(CharCodec, char.asInstanceOf[AnyRef].getClass)
    add(StringCodec, classOf[String])
    add(new ListCodec(this), classOf[List[_]])
    add(new NodeCodec(this), classOf[Node])
    add(new OptionCodec(this), classOf[Option[_]])
  }

  def add(codec:Codec[_], clazz:Class[_]) = {
    //Don't add the same codec twice
    if (!codecs.contains(codec)) {
      codecs.add(codec)

      //Add a new node under the most specific superclass of the new node
      mostSpecific(root, clazz).subNodes.append(new CodecNode(codec, clazz))
    }
  }

  def get(clazz:Class[_]) = mostSpecific(root, clazz).codec

  override def decode(target : DataSource) = null  //Need to look up class from tag, then use appropriate codec

  override def code(t : Any, target : DataTarget) = {
    val tClass = t.asInstanceOf[AnyRef].getClass
    target.openTag(tClass.getCanonicalName)
    get(tClass).asInstanceOf[Codec[Any]].code(t, target)
    target.closeTag
  }

  private def mostSpecific(node:CodecNode, clazz:Class[_]):CodecNode = {
    node.subNodes.find(subNode => subNode.clazz.isAssignableFrom(clazz)) match {
      case None => node
      case Some(suitableSubNode) => mostSpecific(suitableSubNode, clazz)
    }
  }

  case class CodecNode(codec:Codec[_], clazz:Class[_]) {
    val subNodes = mutable.ListBuffer[CodecNode]()
  }

}

object AnyCodec extends Codec[Any] {
  override def decode(target : DataSource) = throw new RuntimeException("Can't decode Any")
  override def code(t : Any, target : DataTarget) = throw new RuntimeException("Can't code Any")
}
object IntCodec extends Codec[Int] {
  override def decode(target : DataSource) = target.getInt
  override def code(t : Int, target : DataTarget) = target.putInt(t.asInstanceOf[Int])
}
object ShortCodec extends Codec[Short] {
  override def decode(target : DataSource) = target.getShort
  override def code(t : Short, target : DataTarget) = target.putShort(t.asInstanceOf[Short])
}
object ByteCodec extends Codec[Byte] {
  override def decode(target : DataSource) = target.getByte
  override def code(t : Byte, target : DataTarget) = target.putByte(t.asInstanceOf[Byte])
}
object LongCodec extends Codec[Long] {
  override def decode(target : DataSource) = target.getLong
  override def code(t : Long, target : DataTarget) = target.putLong(t.asInstanceOf[Long])
}
object FloatCodec extends Codec[Float] {
  override def decode(target : DataSource) = target.getFloat
  override def code(t : Float, target : DataTarget) = target.putFloat(t.asInstanceOf[Float])
}
object DoubleCodec extends Codec[Double] {
  override def decode(target : DataSource) = target.getDouble
  override def code(t : Double, target : DataTarget) = target.putDouble(t.asInstanceOf[Double])
}
object CharCodec extends Codec[Char] {
  override def decode(target : DataSource) = target.getChar
  override def code(t : Char, target : DataTarget) = target.putChar(t.asInstanceOf[Char])
}
object StringCodec extends Codec[String] {
  override def decode(target : DataSource) = target.getUTF
  override def code(t : String, target : DataTarget) = target.putUTF(t.asInstanceOf[String])
}

class OptionCodec(delegate:Codec[Any]) extends Codec[Option[_]] {
  override def decode(target : DataSource) = None
  override def code(o : Option[_], target : DataTarget) = {
    o match {
      case None => {
        target.openTag("None")
        target.closeTag
      }
      case Some(s) => {
        target.openTag("Some")
        delegate.code(s, target)
        target.closeTag
      }
    }
  }
}

class ListCodec(delegate:Codec[Any]) extends Codec[List[_]] {
  override def decode(target : DataSource) = List[Any]()
  override def code(list : List[_], target : DataTarget) = {
    target.openTag("Length")
    target.putInt(list.length)
    target.closeTag
    list.foreach(e => delegate.code(e, target))
  }
}

class NodeCodec(delegate:Codec[Any]) extends Codec[Node] {
  override def decode(target : DataSource) = new Node(){}
  override def code(n : Node, target : DataTarget) = {
    NodeAccessors.accessors(n).foreach(entry => {
      target.openTag(entry._1)
      delegate.code(entry._2.invoke(n).asInstanceOf[Var[_]].apply, target)
      target.closeTag
    })
  }
}

