package boxes.persistence

import collection._
import boxes._

/**
* Writes objects to a TokenWriter, and
* reads them from a TokenReader
*/
trait Codec[T] {
  def read(reader : TokenReader) : T
  def write(t : T, writer : TokenWriter) : Unit
}

trait CodecWithClass[T] extends Codec[T]{
  //Note this isn't Class[T] because of type erasure
  def clazz():Class[_]
}

class CodecByClass extends Codec[Any] {

  private val root = new CodecNode(AnyCodec, classOf[Any])
  private val codecs = mutable.Set[Codec[_]]()

  {
    //Common default codecs
    add(ValCodecs.IntCodec)
    add(ValCodecs.LongCodec)
    add(ValCodecs.FloatCodec)
    add(ValCodecs.DoubleCodec)
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
  override def read(reader : TokenReader) = {
    val t = reader.peek
    val c = t match {
      case OpenObj(clazz, _)           => clazz
      case BooleanToken(p: Boolean) 	 => ValCodecs.BooleanCodec.clazz 
      case IntToken(p: Int) 			     => ValCodecs.IntCodec.clazz 
      case LongToken(p: Long) 			   => ValCodecs.LongCodec.clazz 
      case FloatToken(p: Float) 		   => ValCodecs.FloatCodec.clazz 
      case DoubleToken(p: Double) 		 => ValCodecs.DoubleCodec.clazz 
      case StringToken(p: String) 		 => ValCodecs.StringCodec.clazz 
      case OpenArr 						         => ListCodec.clazz 

      case _ => throw new RuntimeException("Expected OpenObj, OpenArr, or value Token got " + t)
    }
    
    val codec = get(c)
    codec.read(reader)
  }

  override def write(t : Any, writer: TokenWriter) = {
    val tClass = t.asInstanceOf[AnyRef].getClass
    get(tClass).asInstanceOf[Codec[Any]].write(t, writer)
  }

  private def mostSpecific(node:CodecNode, clazz:Class[_]):CodecNode = {
    node.subNodes.find(subNode => subNode.clazz.isAssignableFrom(clazz)) match {
      case None => node
      case Some(suitableSubNode) => mostSpecific(suitableSubNode, clazz)
    }
  }

  private case class CodecNode(codec:Codec[_], clazz:Class[_]) {
    val subNodes = mutable.ListBuffer[CodecNode]()
  }

}

object AnyCodec extends Codec[Any] {
  override def read(reader: TokenReader) = throw new RuntimeException("Can't read Any")
  override def write(t : Any, writer: TokenWriter) = throw new RuntimeException("Can't write Any")
}

class OptionCodec(delegate:Codec[Any]) extends Codec[Option[_]] {
  override def read(reader: TokenReader) = {
    reader.pullAndAssert(OpenObj(classOf[Option[_]]))
    
    val t = reader.pull
    t match {
      case CloseObj => None						//None is just an empty Option obj
      case OpenField("Some") => {				//Some has a single field, "Some". Remember to get the object close tag afterwards
        val s = Some(delegate.read(reader))
        reader.pullAndAssert(CloseObj)
        s
      }
      case _ => throw new RuntimeException("Expected CloseObj or OpenField(Some), got " + t)
    }
  }
  override def write(o : Option[_], writer: TokenWriter) = {
    writer.write(OpenObj(classOf[Option[_]]))
    o match {
      case None => {}
      case Some(s) => {
        writer.write(OpenField("Some"))
        delegate.write(s, writer)
      }
    }
    writer.write(CloseObj)
  }
}

object ListCodec {
  val clazz = classOf[List[_]]
}
class ListCodec(delegate:Codec[Any]) extends CodecWithClass[List[_]] {
  override def read(reader: TokenReader) = {
    reader.pullAndAssert(OpenArr)
    val lb = mutable.ListBuffer[Any]()
    while (reader.peek != CloseArr) {
      lb.append(delegate.read(reader))
    }
    reader.pullAndAssert(CloseArr)
    List(lb:_*)
  }
  override def write(list : List[_], writer: TokenWriter) = {
    writer.write(OpenArr)
    list.foreach(e => delegate.write(e, writer))
    writer.write(CloseArr)
  }
  override def clazz = ListCodec.clazz
}

class SetCodec(delegate:Codec[Any]) extends CodecWithClass[Set[_]] {
  override def read(reader: TokenReader) = {
	reader.pullAndAssert(OpenObj(classOf[Set[_]]))
    reader.pullAndAssert(OpenArr)
    val lb = mutable.ListBuffer[Any]()
    while (reader.peek != CloseArr) {
      lb.append(delegate.read(reader))
    }
    reader.pullAndAssert(CloseArr)
    reader.pullAndAssert(CloseObj)
    Set(lb:_*)
  }
  override def write(set : Set[_], writer : TokenWriter) = {
    writer.write(OpenObj(classOf[Set[_]]))
    writer.write(OpenArr)
    set.foreach(e => delegate.write(e, writer))
    writer.write(CloseArr)
    writer.write(CloseObj)
  }
  override def clazz = classOf[Set[_]]
}

class MapCodec(delegate:Codec[Any]) extends CodecWithClass[Map[_,_]] {
  override def read(reader : TokenReader) = {
    val entries = mutable.ListBuffer[(Any,Any)]()
	reader.pullAndAssert(OpenObj(classOf[Map[_, _]]))
    reader.pullAndAssert(OpenField("entries"))
    reader.pullAndAssert(OpenArr)
    val lb = mutable.ListBuffer[Any]()
    while (reader.peek != CloseArr) {
      reader.pullAndAssert(OpenArr)
      val key = delegate.read(reader)
      val value = delegate.read(reader)
      entries.append((key, value))
      reader.pullAndAssert(CloseArr)
    }
    reader.pullAndAssert(CloseArr)
    reader.pullAndAssert(CloseObj)

    Map(entries:_*)
  }

  override def write(map : Map[_,_], writer: TokenWriter) = {
    writer.write(OpenObj(classOf[Map[_,_]]))
    writer.write(OpenField("entries"))
    writer.write(OpenArr)
    map.foreach(entry => {
      writer.write(OpenArr)
      delegate.write(entry._1, writer)
      delegate.write(entry._2, writer)
      writer.write(CloseArr)
    })
    writer.write(CloseArr)
    writer.write(CloseObj)
  }
  override def clazz = classOf[Map[_, _]]
}


class NodeCodec(delegate:Codec[Any]) extends Codec[Node] {
  
  override def read(reader: TokenReader) = {
    val t = reader.pull
    val tag = t match {
      case OpenObj(c, l) => OpenObj(c, l)
      case _ => throw new RuntimeException("Expected OpenObj token, got " + t)
    }
    val c = tag.clazz

    tag.link match {
      case LinkRef(id) => {
        val o = reader.retrieveCached(id)
        //Still need to close the tag - there is nothing inside the tag, since we are just a ref
        reader.pullAndAssert(CloseObj)
        o.asInstanceOf[Node]
      }
      case LinkId(id) => {

        val n = c.newInstance
        reader.cache(id, n)
        
        //Fill out the node's Vars
        val accMap = Node.accessorsOfClass(c)
        while (reader.peek != CloseObj) {
          val t = reader.pull
          val accessorName = t match {
            case OpenField(n) => n
            case _ => throw new RuntimeException("Expected OpenField, got " + t)
          }
          val accessorValue = delegate.read(reader)
          accMap.get(accessorName) match {
            case None => {}
            case Some(m) => m.invoke(n).asInstanceOf[VarBox[Any, Change[Any]]].update(accessorValue)
          }
        }
        reader.pullAndAssert(CloseObj)

        n.asInstanceOf[Node]
      }
      case LinkEmpty => throw new RuntimeException("A Node has neither ref nor id, which should not happen.")
    }


  }
  override def write(n : Node, writer: TokenWriter) = {

    //First, see if we are new
    writer.cache(n) match {
      //We were cached, just write out as a ref
      case Cached(ref) => {
        writer.write(OpenObj(n.getClass, LinkRef(ref)))
        writer.write(CloseObj)
      }

      //We are new, write out as normal, and include the id
      case New(id) => {
        writer.write(OpenObj(n.getClass, LinkId(id)))
        Node.accessors(n).foreach(entry => {
          writer.write(OpenField(entry._1))
          delegate.write(entry._2.invoke(n).asInstanceOf[VarBox[_,_]].apply, writer)
        })
        writer.write(CloseObj)
      }
    }

  }
}

//TODO share code, via implicits? Make exception print the token we got instead of expected one.
object ValCodecs {
  implicit object BooleanCodec extends CodecWithClass[Boolean] {
    override def clazz = classOf[java.lang.Boolean]
    override def write(t : Boolean, writer: TokenWriter) = writer.write(BooleanToken(t))
    override def read(reader: TokenReader) = reader.pull match {
      case BooleanToken(t) => t
      case _ => throw new RuntimeException("Expected Boolean token")
    }
  }
  implicit object IntCodec extends CodecWithClass[Int] {
    override def clazz = classOf[java.lang.Integer]
    override def write(t : Int, writer: TokenWriter) = writer.write(IntToken(t))
    override def read(reader: TokenReader) = reader.pull match {
      case IntToken(t) => t
      case _ => throw new RuntimeException("Expected Int token")
    }
  }
  implicit object LongCodec extends CodecWithClass[Long] {
    override def clazz = classOf[java.lang.Long]
    override def write(t : Long, writer: TokenWriter) = writer.write(LongToken(t))
    override def read(reader: TokenReader) = reader.pull match {
      case LongToken(t) => t
      case _ => throw new RuntimeException("Expected Long token")
    }
  }
  implicit object FloatCodec extends CodecWithClass[Float] {
    override def clazz = classOf[java.lang.Float]
    override def write(t : Float, writer: TokenWriter) = writer.write(FloatToken(t))
    override def read(reader: TokenReader) = reader.pull match {
      case FloatToken(t) => t
      case _ => throw new RuntimeException("Expected Float token")
    }
  }
  implicit object DoubleCodec extends CodecWithClass[Double] {
    override def clazz = classOf[java.lang.Double]
    override def write(t : Double, writer: TokenWriter) = writer.write(DoubleToken(t))
    override def read(reader: TokenReader) = reader.pull match {
      case DoubleToken(t) => t
      case _ => throw new RuntimeException("Expected Double token")
    }
  }
  implicit object StringCodec extends CodecWithClass[String] {
    override def clazz = classOf[java.lang.String]
    override def write(t : String, writer: TokenWriter) = writer.write(StringToken(t))
    override def read(reader: TokenReader) = reader.pull match {
      case StringToken(t) => t
      case _ => throw new RuntimeException("Expected String token")
    }
  }

}





