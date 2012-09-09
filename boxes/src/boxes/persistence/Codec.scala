package boxes.persistence

import collection._
import boxes._

/**
* Codes objects to a DataTarget, and
* decodes them from a DataSource
*/
trait Codec[T] {
  def decode(target : DataSource) : T
  def code(t : T, target : DataTarget) : Unit
}

trait CodecWithClass[T] extends Codec[T]{
  //Note this isn't Class[T] because of type erasure
  def clazz():Class[_]
}

//import ValCodecs._
//object ValCodec {
//  def apply[T]()(implicit codec:CodecWithClass[T]) = codec
//}

class CodecByClass extends Codec[Any] {

  private val root = new CodecNode(AnyCodec, classOf[Any])
  private val codecs = mutable.Set[Codec[_]]()


  {
    //FIXME reinstate this shorter version if I ever work out why implicits
    //aren't working
//    add(ValCodec[Int])
//    add(ValCodec[Short])
//    add(ValCodec[Byte])
//    add(ValCodec[Long])
//    add(ValCodec[Float])
//    add(ValCodec[Double])
//    add(ValCodec[Char])
//    add(ValCodec[Boolean])
//    add(ValCodec[String])

    //Common default codecs
    add(ValCodecs.IntCodec)
    add(ValCodecs.ShortCodec)
    add(ValCodecs.ByteCodec)
    add(ValCodecs.LongCodec)
    add(ValCodecs.FloatCodec)
    add(ValCodecs.DoubleCodec)
    add(ValCodecs.CharCodec)
    add(ValCodecs.BooleanCodec)
    add(ValCodecs.StringCodec)


    add(new ListCodec(this), classOf[List[_]])
    add(new MapCodec(this), classOf[Map[_,_]])
    add(new SetCodec(this), classOf[Set[_]])
    add(new NodeCodec(this), classOf[Node])
    add(new OptionCodec(this), classOf[Option[_]])
  }

  def add(codec:CodecWithClass[_]):Unit = {
    add(codec, codec.clazz)
  }

  def add(codec:Codec[_], clazz:Class[_]):Unit = {
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
    val c = source.getOpenClassTag(false).clazz
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
    source.assertOpenClassTag(ClassTag(classOf[Option[_]]))
    val t = source.getOpenTag(consume=true).text match {
      case "None" => None
      case "Some" => Some(delegate.decode(source))
    }
    source.getCloseTag  //Some/None
    source.getCloseTag  //Option
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
    source.assertOpenClassTag(ClassTag(classOf[List[_]]))
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

class SetCodec(delegate:Codec[Any]) extends Codec[Set[_]] {
  override def decode(source : DataSource) = {
    source.assertOpenClassTag(ClassTag(classOf[Set[_]]))
    val lb = mutable.ListBuffer[Any]()
    while (!source.peekCloseTag) {
      lb.append(delegate.decode(source))
    }
    source.getCloseTag
    Set(lb:_*)
  }
  override def code(list : Set[_], target : DataTarget) = {
    target.openClassTag(classOf[Set[_]])
    list.foreach(e => delegate.code(e, target))
    target.closeTag
  }
}


class MapCodec(delegate:Codec[Any]) extends Codec[Map[_,_]] {
  override def decode(source : DataSource) = {
    source.assertOpenClassTag(ClassTag(classOf[Map[_,_]]))
    val entries = mutable.ListBuffer[(Any,Any)]()
    while (!source.peekCloseTag) {
      if (source.getOpenTag(consume=true).text != "key") throw new RuntimeException("Expected key tag for map")
      val key = delegate.decode(source)
      source.getCloseTag
      if (source.getOpenTag(consume=true).text != "value") throw new RuntimeException("Expected value tag for map")
      val value = delegate.decode(source)
      source.getCloseTag
      entries.append((key, value))
    }
    source.getCloseTag
    Map(entries:_*)
  }

  override def code(map : Map[_,_], target : DataTarget) = {
    target.openClassTag(classOf[Map[_,_]])
    map.foreach(entry => {
      target.openTag("key")
      delegate.code(entry._1, target)
      target.closeTag
      target.openTag("value")
      delegate.code(entry._2, target)
      target.closeTag
    })
    target.closeTag
  }
}


class NodeCodec(delegate:Codec[Any]) extends Codec[Node] {
  override def decode(source : DataSource) = {
    val tag = source.getOpenClassTag(consume=true)
    val c = tag.clazz

    tag.ref match {
      //If we have no ref, we are a new object, so create it
      case None => {
        val n = c.newInstance

        //Cache the object for any future refs to it, if it has an id
        //We do this early to allow nodes to refer to themselves in their
        //own Vars, if really necessary!
        tag.id match {
          case None => throw new RuntimeException("No ref for " + n + ", so assumed we would have an id, but none found")
          case Some(id) => source.cache(id, n)
        }

        //Fill out the node's Vars
        val accMap = Node.accessorsOfClass(c)
        while (!source.peekCloseTag) {
          val accessorName = source.getOpenTag(consume=true).text
          val accessorValue = delegate.decode(source)
          accMap.get(accessorName) match {
            case None => {}
            case Some(m) => m.invoke(n).asInstanceOf[VarBox[Any, Change[Any]]].update(accessorValue)
          }
          source.getCloseTag
        }
        source.getCloseTag

        n.asInstanceOf[Node]
      }
      //If we have a ref, then we just look ourselves up from cache
      case Some(ref) => {
        val o = source.retrieveCached(ref)
        //Still need to close the tag - there is nothing inside the tag, since we are just a ref
        source.getCloseTag
        o.asInstanceOf[Node]
      }
    }

  }
  override def code(n : Node, target : DataTarget) = {

    //First, see if we are new
    target.cache(n) match {
      //We were cached, just write out as a ref
      case Cached(ref) => {
        target.openClassTag(n.getClass, None, Some(ref))
        target.closeTag
      }

      //We are new, write out as normal, and include the id
      case New(id) => {
        target.openClassTag(n.getClass, Some(id), None)
        Node.accessors(n).foreach(entry => {
          target.openTag(entry._1)
          delegate.code(entry._2.invoke(n).asInstanceOf[VarBox[_,_]].apply, target)
          target.closeTag
        })
        target.closeTag
      }
    }

  }
}

object ValCodecs {
  implicit object BooleanCodec extends CodecWithClass[Boolean] {
    override def clazz = classOf[java.lang.Boolean]
    override def code(t : Boolean, target : DataTarget) = target.putBoolean(t)
    override def decode(source : DataSource) = source.getBoolean
  }
  implicit object IntCodec extends CodecWithClass[Int] {
    override def clazz = classOf[java.lang.Integer]
    override def code(t : Int, target : DataTarget) = target.putInt(t)
    override def decode(source : DataSource) = source.getInt
  }
  implicit object ShortCodec extends CodecWithClass[Short] {
    override def clazz = classOf[java.lang.Short]
    override def code(t : Short, target : DataTarget) = target.putShort(t)
    override def decode(source : DataSource) = source.getShort
  }
  implicit object ByteCodec extends CodecWithClass[Byte] {
    override def clazz = classOf[java.lang.Byte]
    override def code(t : Byte, target : DataTarget) = target.putByte(t)
    override def decode(source : DataSource) = source.getByte
  }
  implicit object LongCodec extends CodecWithClass[Long] {
    override def clazz = classOf[java.lang.Long]
    override def code(t : Long, target : DataTarget) = target.putLong(t)
    override def decode(source : DataSource) = source.getLong
  }
  implicit object FloatCodec extends CodecWithClass[Float] {
    override def clazz = classOf[java.lang.Float]
    override def code(t : Float, target : DataTarget) = target.putFloat(t)
    override def decode(source : DataSource) = source.getFloat
  }
  implicit object DoubleCodec extends CodecWithClass[Double] {
    override def clazz = classOf[java.lang.Double]
    override def code(t : Double, target : DataTarget) = target.putDouble(t)
    override def decode(source : DataSource) = source.getDouble
  }
  implicit object CharCodec extends CodecWithClass[Char] {
    override def clazz = classOf[java.lang.Character]
    override def code(t : Char, target : DataTarget) = target.putChar(t)
    override def decode(source : DataSource) = source.getChar
  }
  implicit object StringCodec extends CodecWithClass[String] {
    override def clazz = classOf[java.lang.String]
    override def code(t : String, target : DataTarget) = target.putUTF(t)
    override def decode(source : DataSource) = source.getUTF
  }

}





