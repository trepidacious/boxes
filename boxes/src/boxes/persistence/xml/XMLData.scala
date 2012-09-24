package boxes.persistence

//import scala.xml.pull._
//import scala.io.Source
//import collection._
//import scala.xml.Node
//import java.io.{OutputStreamWriter, InputStream, Writer, OutputStream}
//import boxes.persistence.ValCodecs._
//
//trait AnyTag
//case class Tag(text:String, id:Option[Int]=None, ref:Option[Int]=None) extends AnyTag
//case class ClassTag(clazz:Class[_], id:Option[Int]=None, ref:Option[Int]=None) extends AnyTag
//
//class XMLDataSource(s:Source, aliases:ClassAliases) {
//
//  val events = new XMLEventReader(s)
//  var nextEvent:Option[XMLEvent] = None
//
//  private def pullEvent(tag:Boolean) = {
//    //Text events are only wanted if we are NOT looking for
//    //a tag (and so may want whitespace text), OR the text
//    //is not only whitespace
//    events.find(event => {
//      event match {
//        case text:EvText => !tag || !text.text.trim.isEmpty
//        case _ => true
//      }
//    }) match {
//      case None => throw new RuntimeException("No more events")
//      case Some(event) => event
//    }
//  }
//
//  private def getNextEvent(consume:Boolean, tag:Boolean = true) = {
//    if (consume) {
//      val e = nextEvent.getOrElse(pullEvent(tag))
//      nextEvent = None
//      e
//    } else {
//      nextEvent match {
//        case None => {
//          val e = pullEvent(tag)
//          nextEvent = Some(e)
//          e
//        }
//        case Some(e) => e
//      }
//    }
//  }
//
//  private def getText() = {
//    //Peek next event, note we are NOT looking for a tag,
//    //so we will not ignore whitespace text
//    //If we get a close tag, then we treat this as an empty string,
//    //but don't consume the close tag, since it is meant for someone else.
//    //If we get actual text, we consume and return it
//    getNextEvent(consume=false, tag=false) match {
//      case text:EvText => {
//        getNextEvent(consume=true, tag=false)
//        text.text
//      }
//      case end:EvElemEnd => ""
//      case wrong:Any => throw new RuntimeException("Next event is not text, it is " + wrong)
//    }
//  }
//
//  //Might seem odd to use codec, but we are just looking up the class to use for our "internal"
//  //tag
//  private def getPrimitive[P](c: (String)=>P)(implicit codec:CodecWithClass[P]):P = {
//    assertOpenClassTag(ClassTag(codec.clazz))
//    val t = c(getText)
//    getCloseTag
//    t
//  }
//  
//  override def getBoolean() = getPrimitive(_.trim.toBoolean)
//  override def getInt() = getPrimitive(_.trim.toInt)
//  override def getLong() = getPrimitive(_.trim.toLong)
//  override def getFloat() = getPrimitive(_.trim.toFloat)
//  override def getDouble() = getPrimitive(_.trim.toDouble)
//  override def getUTF():String = getPrimitive((s)=>s)
//
//  override def close() = {
//    s.close
//  }
//
//  def intAttr(e:EvElemStart, name:String) = {
//    e.attrs(name) match {
//      case null => None
//      case id:Seq[Node] => Some(id(0).text.toInt)
//    }
//  }
//
//  override def getOpenTag(consume:Boolean) = {
//    //TODO check for prohibited tag names, the names of primitive types used exclusively to tag primitives "internally"
//    getNextEvent(consume) match {
//      case start:EvElemStart => Tag(start.label, intAttr(start, "id"), intAttr(start, "ref"))
//      case wrong:Any => throw new RuntimeException("Next tag is not an open tag, it is " + wrong)
//    }
//  }
//
//  override def getOpenClassTag(consume:Boolean):ClassTag = {
//    //TODO check for prohibited tag names, the names of primitive types used exclusively to tag primitives "internally"
//    val ot = getOpenTag(consume)
//    ClassTag(aliases.forAlias(ot.text), ot.id, ot.ref)
//  }
//
//  override def assertOpenClassTag(expectedTag:ClassTag):Unit = {
//    val observedTag = getOpenClassTag(consume=true)
//    if (observedTag != expectedTag) {
//      throw new RuntimeException("Expected tag " + expectedTag + " but found " + observedTag)
//    }
//  }
//
//  override def getCloseTag() = {
//    getNextEvent(consume=true) match {
//      case end:EvElemEnd => {}
//      case wrong:Any => throw new RuntimeException("Next tag is not a close tag, it is " + wrong)
//    }
//  }
//  override def peekCloseTag():Boolean = {
//    getNextEvent(consume=false) match {
//      case end:EvElemEnd => true
//      case _ => false
//    }
//  }
//
//}
//
//class XMLDataTarget(writer:Writer, aliases:ClassAliases) extends DataTarget {
//
//  //Stack of info for open tags. String is the tag label, Boolean is whether
//  //formatting should be skipped when closing the tag.
//  private val tagStack = mutable.ListBuffer[(String, Boolean)]()
//
//  private val cache = mutable.Map[Any, Int]()
//  private var nextId = 0
//
//  override def cache(thing:Any) = {
//    cache.get(thing) match {
//      case None => {
//        val id = nextId
//        nextId = nextId + 1
//        cache.put(thing, id)
//        New(id)
//      }
//      case Some(ref) => Cached(ref)
//    }
//  }
//
//  private def printWithFormatting(s:String, newline:Boolean = true, tabs:Boolean = true) = {
//    if (tabs) tagStack.foreach(_ => writer.write("  "))
//    writer.write(s)
//    if (newline) writer.write("\n")
//  }
//  private def print(s:String) = {
//    writer.write(s)
//  }
//
//  //Might seem odd to use codec, but we are just looking up the class to use for our "internal"
//  //tag
//  private def putTaggedPrimitive[P](p:P)(implicit codec:CodecWithClass[P]) {
//    openClassTag(codec.clazz, None, None, true)
//    print("" + p)
//    closeTag()
//  }
//  
//  def putBoolean(b:Boolean) =   putTaggedPrimitive(b)
//  def putByte(i:Byte) =         putTaggedPrimitive(i)
//  def putShort(i:Short) =       putTaggedPrimitive(i)
//  def putChar(i:Char) =         putTaggedPrimitive(i)
//  def putInt(i:Int) =           putTaggedPrimitive(i)
//  def putLong(l:Long) =         putTaggedPrimitive(l)
//  def putFloat(f:Float) =       putTaggedPrimitive(f)
//  def putDouble(d:Double) =     putTaggedPrimitive(d)
//  def putUTF(s:String) =        putTaggedPrimitive(s)
//
//  def openTag(s:String) = {
//    printWithFormatting("<" + s + ">")
//    tagStack.append((s, false))
//  }
//
//  def openClassTag(c:Class[_], id:Option[Int], ref:Option[Int], condensed:Boolean) = {
//    var label = aliases.forClass(c)
//    var s = label
//
//    id.foreach(id => s = s + " id='" + id + "'")
//    ref.foreach(ref => s = s + " ref='" + ref + "'")
//
//    //When opening a condensed class tag, don't print a newline
//    if (condensed) {
//      printWithFormatting("<" + s + ">", false)
//      tagStack.append((label, true))
//    } else {
//      printWithFormatting("<" + s + ">")
//      tagStack.append((label, false))
//    }
//  }
//  
//  def openClassTag(c:Class[_], id:Option[Int], ref:Option[Int]) = openClassTag(c, id, ref, false)
//
//  def closeTag() = {
//    val tag = tagStack.remove(tagStack.size-1)
//    val name = tag._1
//    val skipTabs = tag._2
//
//    if (skipTabs) {
//      printWithFormatting("</" + name +">", true, false)
//    } else {
//      printWithFormatting("</" + name +">")
//    }
//  }
//
//  def flush() = {
//    writer.flush
//  }
//
//  def close() = {
//    writer.close
//    cache.clear
//    if (!tagStack.isEmpty) throw new RuntimeException("Closed XMLDataTarget with tags still open")
//  }
//}
//
//object XMLDataFactory extends DataFactory {
//  def source(input:InputStream, aliases:ClassAliases) = new XMLDataSource(Source.fromInputStream(input, "UTF-8"), aliases)
//  def target(output:OutputStream, aliases:ClassAliases) = new XMLDataTarget(new OutputStreamWriter(output, "UTF-8"), aliases)
//}
//
//object XMLIO {
//  def apply(aliases:ClassAliases = new ClassAliases) = new IO(XMLDataFactory, aliases)
//}
