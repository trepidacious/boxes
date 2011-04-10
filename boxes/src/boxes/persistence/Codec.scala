package boxes.persistence

import collection._
import boxes.Var
import boxes.Node

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

//FIXME use ids and refs to ensure we only code each mutable object once. Multiples of immutable
//objects are not so important. Really we are only concerned about Nodes.

class CodecByClass extends Codec[Any] {

  private val root = new CodecNode(AnyCodec, classOf[Any])
  private val codecs = mutable.Set[Codec[_]]()

  //Common default codecs

  {
    add(new ValCodec[Int](ValHandlers.IntHandler))
    add(new ValCodec[Short](ValHandlers.ShortHandler))
    add(new ValCodec[Byte](ValHandlers.ByteHandler))
    add(new ValCodec[Long](ValHandlers.LongHandler))
    add(new ValCodec[Float](ValHandlers.FloatHandler))
    add(new ValCodec[Char](ValHandlers.CharHandler))
    add(new ValCodec[Boolean](ValHandlers.BooleanHandler))
    add(new ValCodec[String](ValHandlers.StringHandler))

    //FIXME reinstate this shorter version if I ever work out why implicits
    //aren't working
//    add(ValCodec[Int])
//    add(ValCodec[Short])
//    add(ValCodec[Byte])
//    add(ValCodec[Long])
//    add(ValCodec[Float])
//    add(ValCodec[Char])
//    add(ValCodec[Boolean])
//    add(ValCodec[String])


    add(new ListCodec(this), classOf[List[_]])
    add(new MapCodec(this), classOf[Map[_,_]])
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
    val c = source.peekOpenClassTag._1
    println("Decoding " + c)
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
    val t = source.getOpenTag._1 match {
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

class MapCodec(delegate:Codec[Any]) extends Codec[Map[_,_]] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(classOf[Map[_,_]])
    println("Decoding map")
    val entries = mutable.ListBuffer[(Any,Any)]()
    while (!source.peekCloseTag) {
      println("Got a map entry")
      if (source.getOpenTag._1 != "key") throw new RuntimeException("Expected key tag for map")
      val key = delegate.decode(source)
      source.getCloseTag
      if (source.getOpenTag._1 != "value") throw new RuntimeException("Expected value tag for map")
      val value = delegate.decode(source)
      source.getCloseTag
      entries.append((key, value))
      println("Loaded entry " + (key, value))
    }
    source.getCloseTag
    Map(entries:_*)
  }

  override def code(map : Map[_,_], target : DataTarget) = {
    target.openClassTag(classOf[Map[_,_]])
    println("Coding map")
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
    val tagStuff = source.getOpenClassTag
    val c = tagStuff._1
    //TODO use ref/id to track
    val n = c.newInstance
    val accMap = NodeAccessors.accessorsOfClass(c)
    while (!source.peekCloseTag) {
      val accessorName = source.getOpenTag._1
      val accessorValue = delegate.decode(source)
      accMap.get(accessorName) match {
        case None => {}
        case Some(m) => m.invoke(n).asInstanceOf[Var[Any]].update(accessorValue)
      }
      source.getCloseTag
    }
    source.getCloseTag
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

object ValCodec {
  def apply[T]()(implicit handler:ValHandler[T]) = new ValCodec[T](handler)
}

class ValCodec[T](handler:ValHandler[T]) extends CodecWithClass[T] {
  override def decode(source : DataSource) = {
    source.getOpenClassTag(handler.clazz)
    val t = handler.get(source)
    source.getCloseTag
    t
  }
  override def code(t : T, target : DataTarget) = {
    target.openClassTag(handler.clazz)
    handler.put(target, t)
    target.closeTag
  }
  override def clazz() = handler.clazz
}
