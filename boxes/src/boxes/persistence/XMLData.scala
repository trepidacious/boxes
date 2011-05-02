package boxes.persistence

import scala.xml.pull._
import scala.io.Source
import collection._
import java.io.Writer
import xml.Node

class XMLAliases {
  private val aliases = mutable.Map[Class[_], String]()
  private val aliasesReverse = mutable.Map[String, Class[_]]()

  //Common default aliases

  {
    alias(classOf[java.lang.Long],    "Long")
    alias(classOf[java.lang.Integer], "Int")
    alias(classOf[java.lang.Short],   "Short")
    alias(classOf[java.lang.Byte],    "Byte")
    alias(classOf[java.lang.Boolean], "Boolean")
    alias(classOf[java.lang.Double],  "Double")
    alias(classOf[java.lang.Float],   "Float")
    alias(classOf[java.lang.String],  "String")

    alias(classOf[List[_]],           "List")
    alias(classOf[Map[_,_]],          "Map")
    alias(classOf[Set[_]],            "Set")
    alias(classOf[Option[_]],         "Option")

  }

  def alias(c:Class[_], s:String) = {
    //Note that we enforce that each string is only used for at most one class,
    //BUT we allow each class to map to multiple strings. The last-set alias
    //is the one used for encoding, but any string is valid for decoding. This
    //allows us to support legacy aliases for classes.
    aliasesReverse.get(s) match {
      case None => {}
      case Some(previousClass) => throw new RuntimeException(s + " is already an alias for " + previousClass.getCanonicalName)
    }
    aliases.put(c, s)
    aliasesReverse.put(s, c)
  }

  def forClass(c:Class[_]) = aliases.getOrElse(c, c.getCanonicalName)

  def forAlias(s:String) = aliasesReverse.getOrElse(s, Class.forName(s))

}

class XMLDataSource(s:Source, aliases:XMLAliases) extends DataSource {

  val events = new XMLEventReader(s)
  var nextEvent:Option[XMLEvent] = None
  private val cache = mutable.Map[Int, Any]()

  private def pullEvent(tag:Boolean) = {
    //Text events are only wanted if we are NOT looking for
    //a tag (and so may want whitespace text), OR the text
    //is not only whitespace
    events.find(event => {
      event match {
        case text:EvText => !tag || !text.text.trim.isEmpty
        case _ => true
      }
    }) match {
      case None => throw new RuntimeException("No more events")
      case Some(event) => event
    }
  }

  private def getNextEvent(consume:Boolean, tag:Boolean = true) = {
    if (consume) {
      val e = nextEvent.getOrElse(pullEvent(tag))
      nextEvent = None
      e
    } else {
      nextEvent match {
        case None => {
          val e = pullEvent(tag)
          nextEvent = Some(e)
          e
        }
        case Some(e) => e
      }
    }
  }

  private def getText() = {
    //Peek next event, note we are NOT looking for a tag,
    //so we will not ignore whitespace text
    //If we get a close tag, then we treat this as an empty string,
    //but don't consume the close tag, since it is meant for someone else.
    //If we get actual text, we consume and return it
    getNextEvent(consume=false, tag=false) match {
      case text:EvText => {
        getNextEvent(consume=true, tag=false)
        text.text
      }
      case end:EvElemEnd => ""
      case wrong:Any => throw new RuntimeException("Next event is not text, it is " + wrong)
    }
  }

  override def cache(id:Int, thing:Any) = {
    cache.put(id, thing).foreach(existing => {
      throw new RuntimeException("Tried to cache " + thing + " with id " + id + " but this was already used for " + existing)
    })
  }
  override def retrieveCached(ref:Int) = {
    cache.get(ref) match {
      case None => throw new RuntimeException("Nothing found in cache for id (ref) " + ref)
      case Some(thing) => thing
    }
  }

  override def getBoolean() = getText.trim.toBoolean
  override def getByte() = getText.trim.toByte
  override def getShort() = getText.trim.toShort
  override def getChar() = getText.trim.toCharArray.apply(0)
  override def getInt() = getText.trim.toInt
  override def getLong() = getText.trim.toLong
  override def getFloat() = getText.trim.toFloat
  override def getDouble() = getText.trim.toDouble
  override def getUTF():String = getText

  override def close() = {
    cache.clear
    s.close
  }

  def intAttr(e:EvElemStart, name:String) = {
    e.attrs(name) match {
      case null => None
      case id:Seq[Node] => Some(id(0).text.toInt)
    }
  }

  override def getOpenTag(consume:Boolean) = {
    getNextEvent(consume) match {
      case start:EvElemStart => Tag(start.label, intAttr(start, "id"), intAttr(start, "ref"))
      case wrong:Any => throw new RuntimeException("Next tag is not an open tag, it is " + wrong)
    }
  }

  override def getOpenClassTag(consume:Boolean):ClassTag = {
    val ot = getOpenTag(consume)
    ClassTag(aliases.forAlias(ot.text), ot.id, ot.ref)
  }

  override def assertOpenClassTag(expectedTag:ClassTag):Unit = {
    val observedTag = getOpenClassTag(consume=true)
    if (observedTag != expectedTag) {
      throw new RuntimeException("Expected tag " + expectedTag + " but found " + observedTag)
    }
  }

  override def getCloseTag() = {
    getNextEvent(consume=true) match {
      case end:EvElemEnd => {}
      case wrong:Any => throw new RuntimeException("Next tag is not a close tag, it is " + wrong)
    }
  }
  override def peekCloseTag():Boolean = {
    getNextEvent(consume=false) match {
      case end:EvElemEnd => true
      case _ => false
    }
  }

}

class XMLDataTarget(writer:Writer, aliases:XMLAliases) extends DataTarget {

  //Stack of info for open tags. String is the tag label, Boolean is whether
  //formatting should be skipped when closing the tag.
  private val tagStack = mutable.ListBuffer[(String, Boolean)]()

  private val cache = mutable.Map[Any, Int]()
  private var nextId = 0

  private val condensedClasses = Set[Class[_]](
    classOf[java.lang.Long],
    classOf[java.lang.Integer],
    classOf[java.lang.Short],
    classOf[java.lang.Byte],
    classOf[java.lang.Boolean],
    classOf[java.lang.Double],
    classOf[java.lang.Float],
    classOf[java.lang.String]
  )

  override def cache(thing:Any) = {
    cache.get(thing) match {
      case None => {
        val id = nextId
        nextId = nextId + 1
        cache.put(thing, id)
        New(id)
      }
      case Some(ref) => Cached(ref)
    }
  }

  private def printWithFormatting(s:String, newline:Boolean = true, tabs:Boolean = true) = {
    if (tabs) tagStack.foreach(_ => writer.write("  "))
    writer.write(s)
    if (newline) writer.write("\n")
  }
  private def print(s:String) = {
    writer.write(s)
  }

  //No tab or newline for primitives. This is particularly important
  //for strings, we want the tags around the string value
  //to contain only the string value itself, no extra whitespace.
  def putBoolean(b:Boolean) =   print(""+b)
  def putByte(i:Byte) =         print(""+i)
  def putShort(i:Short) =       print(""+i)
  def putChar(i:Char) =         print(""+i)
  def putInt(i:Int) =           print(""+i)
  def putLong(l:Long) =         print(""+l)
  def putFloat(f:Float) =       print(""+f)
  def putDouble(d:Double) =     print(""+d)
  def putUTF(s:String) =        print(s)

  def openTag(s:String) = {
    printWithFormatting("<" + s + ">")
    tagStack.append((s, false))
  }

  def openClassTag(c:Class[_], id:Option[Int], ref:Option[Int]) = {
    var label = aliases.forClass(c)
    var s = label

    id.foreach(id => s = s + " id='" + id + "'")
    ref.foreach(ref => s = s + " ref='" + ref + "'")

    //When opening a condensed class tag, don't print a newline
    if (condensedClasses.contains(c)) {
      printWithFormatting("<" + s + ">", false)
      tagStack.append((label, true))
    } else {
      printWithFormatting("<" + s + ">")
      tagStack.append((label, false))
    }
  }

  def closeTag() = {
    val tag = tagStack.remove(tagStack.size-1)
    val name = tag._1
    val skipTabs = tag._2

    if (skipTabs) {
      printWithFormatting("</" + name +">", true, false)
    } else {
      printWithFormatting("</" + name +">")
    }
  }

  def flush() = {
    writer.flush
  }

  def close() = {
    writer.close
    cache.clear
    if (!tagStack.isEmpty) throw new RuntimeException("Closed XMLDataTarget with tags still open")
  }
}