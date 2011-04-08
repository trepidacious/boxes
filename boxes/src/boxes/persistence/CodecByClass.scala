package boxes.persistence

import collection._

/**
* Codes objects to a DataTarget, and
* decodes them from a DataSource
*/
 trait Codec{
 def decode(target : DataSource) : Any
 def code(t : Any, target : DataTarget) : Unit
 }

 trait Node


class CodecByClass extends Codec {

  private val root = new CodecNode(AnyCodec, classOf[Any])
  private val codecs = mutable.Set[Codec]()

  {
    add(IntCodec, classOf[Int])
    add(ShortCodec, classOf[Short])
    add(ByteCodec, classOf[Byte])
    add(LongCodec, classOf[Long])
    add(FloatCodec, classOf[Float])
    add(CharCodec, classOf[Char])
    add(StringCodec, classOf[String])
    add(new ListCodec(this), classOf[List[_]])
  }

  def add(codec:Codec, clazz:Class[_]) = {
    //Don't add the same codec twice
    if (!codecs.contains(codec)) {
      codecs.add(codec)

      //Add a new node under the most specific superclass of the new node
      mostSpecific(root, clazz).subNodes.append(new CodecNode(codec, clazz))
    }
  }

  def get(clazz:Class[_]) = mostSpecific(root, clazz).codec

  override def decode(target : DataSource) = null  //Need to look up class from tag, then use appropriate codec
  override def code(t : Any, target : DataTarget) = get(t.asInstanceOf[AnyRef].getClass).code(t, target)


  private def mostSpecific(node:CodecNode, clazz:Class[_]):CodecNode = {
    node.subNodes.find(subNode => subNode.clazz.isAssignableFrom(clazz)) match {
      case None => node
      case Some(suitableSubNode) => mostSpecific(suitableSubNode, clazz)
    }
  }

  case class CodecNode(codec:Codec, clazz:Class[_]) {
    val subNodes = mutable.ListBuffer[CodecNode]()
  }

}

object AnyCodec extends Codec {
  override def decode(target : DataSource) = throw new RuntimeException("Can't decode Any")
  override def code(t : Any, target : DataTarget) = throw new RuntimeException("Can't code Any")
}
object IntCodec extends Codec {
  override def decode(target : DataSource) = target.getInt
  override def code(t : Any, target : DataTarget) = target.putInt(t.asInstanceOf[Int])
}
object ShortCodec extends Codec {
  override def decode(target : DataSource) = target.getShort
  override def code(t : Any, target : DataTarget) = target.putShort(t.asInstanceOf[Short])
}
object ByteCodec extends Codec {
  override def decode(target : DataSource) = target.getByte
  override def code(t : Any, target : DataTarget) = target.putByte(t.asInstanceOf[Byte])
}
object LongCodec extends Codec {
  override def decode(target : DataSource) = target.getLong
  override def code(t : Any, target : DataTarget) = target.putLong(t.asInstanceOf[Long])
}
object FloatCodec extends Codec {
  override def decode(target : DataSource) = target.getFloat
  override def code(t : Any, target : DataTarget) = target.putFloat(t.asInstanceOf[Float])
}
object DoubleCodec extends Codec {
  override def decode(target : DataSource) = target.getDouble
  override def code(t : Any, target : DataTarget) = target.putDouble(t.asInstanceOf[Double])
}
object CharCodec extends Codec{
  override def decode(target : DataSource) = target.getChar
  override def code(t : Any, target : DataTarget) = target.putChar(t.asInstanceOf[Char])
}
object StringCodec extends Codec {
  override def decode(target : DataSource) = target.getUTF
  override def code(t : Any, target : DataTarget) = target.putUTF(t.asInstanceOf[String])
}
class ListCodec(store:CodecByClass) extends Codec {
  override def decode(target : DataSource) = List[AnyRef]()
  override def code(t : Any, target : DataTarget) = {
    val list = t.asInstanceOf[List[_]]
    target.openTag("List")
    target.openTag("Length")
    target.putInt(list.length)
    target.closeTag
    list.foreach(e => {
      store.get(e.asInstanceOf[AnyRef].getClass).code(e, target)
    })
    target.closeTag
  }
}



