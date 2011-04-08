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
    add(IntCodec, classOf[java.lang.Integer])
    add(ShortCodec, classOf[java.lang.Short])
    add(ByteCodec, classOf[java.lang.Byte])
    add(LongCodec, classOf[java.lang.Long])
    add(FloatCodec, classOf[java.lang.Float])
    add(CharCodec, classOf[java.lang.Character])
    add(BooleanCodec, classOf[java.lang.Boolean])
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

  //Need to look up class from tag, then use appropriate codec
  override def decode(source : DataSource) = {
    val c = source.peekOpenClassTag
    val codec = get(c)
    codec.decode(source)
  }

  override def code(t : Any, target : DataTarget) = {
    val tClass = t.asInstanceOf[AnyRef].getClass
    get(tClass).asInstanceOf[Codec[Any]].code(t, target)
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
  override def decode(source : DataSource) = throw new RuntimeException("Can't decode Any")
  override def code(t : Any, target : DataTarget) = throw new RuntimeException("Can't code Any")
}

class OptionCodec(delegate:Codec[Any]) extends Codec[Option[_]] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[Option[_]])
    val t = source.getOpenTag match {
      case "None" => None
      case "Some" => Some(delegate.decode(source))
    }
    source.getCloseTag
    t
  }
  override def code(o : Option[_], target : DataTarget) = {
    target.openClassTag(classOf[Option[_]])
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
    target.closeTag
  }
}

class ListCodec(delegate:Codec[Any]) extends Codec[List[_]] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[List[_]])
    val lb = mutable.ListBuffer[Any]()
    while (!source.peekCloseTag) {
      lb.append(delegate.decode(source))
    }
    source.getCloseTag
    List(lb:_*)
  }
  override def code(list : List[_], target : DataTarget) = {
    target.openClassTag(classOf[List[_]])
    list.foreach(e => delegate.code(e, target))
    target.closeTag
  }
}

class NodeCodec(delegate:Codec[Any]) extends Codec[Node] {
  override def decode(source : DataSource) = {
    val c = source.getOpenClassTag
    val n = c.newInstance
    val accMap = NodeAccessors.accessorsOfClass(c)
    while (!source.peekCloseTag) {
      val accessorName = source.getOpenTag
      val accessorValue = delegate.decode(source)
      accMap.get(accessorName) match {
        case None => {}
        case Some(m) => m.invoke(n).asInstanceOf[Var[Any]].update(accessorValue)
      }
      source.getCloseTag
    }
    n.asInstanceOf[Node]
  }
  override def code(n : Node, target : DataTarget) = {
    target.openClassTag(n.getClass)
    NodeAccessors.accessors(n).foreach(entry => {
      target.openTag(entry._1)
      delegate.code(entry._2.invoke(n).asInstanceOf[Var[_]].apply, target)
      target.closeTag
    })
    target.closeTag
  }
}

object IntCodec extends Codec[Int] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Integer])
    val t = source.getInt
    source.getCloseTag
    t
  }
  override def code(t : Int, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Integer])
    target.putInt(t)
    target.closeTag
  }
}
object ShortCodec extends Codec[Short] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Short])
    val t = source.getShort
    source.getCloseTag
    t
  }
  override def code(t : Short, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Short])
    target.putShort(t)
    target.closeTag
  }
}
object ByteCodec extends Codec[Byte] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Byte])
    val t = source.getByte
    source.getCloseTag
    t
  }
  override def code(t : Byte, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Byte])
    target.putByte(t)
    target.closeTag
  }
}
object LongCodec extends Codec[Long] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Long])
    val t = source.getLong
    source.getCloseTag
    t
  }
  override def code(t : Long, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Long])
    target.putLong(t)
    target.closeTag
  }
}
object FloatCodec extends Codec[Float] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Float])
    val t = source.getFloat
    source.getCloseTag
    t
  }
  override def code(t : Float, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Float])
    target.putFloat(t)
    target.closeTag
  }
}
object DoubleCodec extends Codec[Double] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Double])
    val t = source.getDouble
    source.getCloseTag
    t
  }
  override def code(t : Double, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Double])
    target.putDouble(t)
    target.closeTag
  }
}
object CharCodec extends Codec[Char] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Character])
    val t = source.getChar
    source.getCloseTag
    t
  }
  override def code(t : Char, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Character])
    target.putChar(t)
    target.closeTag
  }
}
object BooleanCodec extends Codec[Boolean] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.Boolean])
    val t = source.getBoolean
    source.getCloseTag
    t
  }
  override def code(t : Boolean, target : DataTarget) = {
    target.openClassTag(classOf[java.lang.Boolean])
    target.putBoolean(t)
    target.closeTag
  }
}

object StringCodec extends Codec[String] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[java.lang.String])
    val t = source.getUTF
    source.getCloseTag
    t
  }
  override def code(t : String, target : DataTarget) = {
    target.openClassTag(classOf[String])
    target.putUTF(t)
    target.closeTag
  }
}

