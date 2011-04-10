package boxes.persistence

import scala.xml.pull._
import scala.io.Source
import collection._
import java.io.Writer

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

  def forAlias(s:String) = {
    aliasesReverse.get(s) match {
      case None => Class.forName(s)
      case Some(c) => c
    }
  }
}

class XMLDataSource(s:Source, aliases:XMLAliases) extends DataSource {

  val events = new XMLEventReader(s)
  var nextEvent:Option[XMLEvent] = None

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

  private def getNextEvent(tag:Boolean = true) = {
    val e = nextEvent.getOrElse(pullEvent(tag))
    nextEvent = None
    e
  }

  private def peekNextEvent(tag:Boolean = true) = {
    nextEvent match {
      case None => {
        val e = pullEvent(tag)
        nextEvent = Some(e)
        e
      }
      case Some(e) => e
    }
  }

  private def getText() = {
    //Peek next event, note we are NOT looking for a tag,
    //so we will not ignore whitespace text
    //If we get a close tag, then we treat this as an empty string,
    //but don't consume the close tag, since it is meant for someone else.
    //If we get actual text, we consume and return it
    peekNextEvent(false) match {
      case text:EvText => {
        getNextEvent(false)
        text.text
      }
      case end:EvElemEnd => ""
      case wrong:Any => throw new RuntimeException("Next event is not text, it is " + wrong)
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

  override def close() = s.close

  //TODO reduce duplicated code - get and peek methods are essentially the same, could just take a boolean parameter.

  override def getOpenTag() = {
    getNextEvent() match {
      case start:EvElemStart => start.label
      case wrong:Any => throw new RuntimeException("Next tag is not an open tag, it is " + wrong)
    }
  }

  override def getOpenClassTag():Class[_] = {
    aliases.forAlias(getOpenTag)
  }

  override def getOpenClassTag(c:Class[_]):Unit = {
    val tagClass = getOpenClassTag
    if (tagClass != c) throw new RuntimeException("Expected tag for class " + c + " but found " + tagClass)
  }

  override def peekOpenTag():String = {
    peekNextEvent() match {
      case start:EvElemStart => start.label
      case wrong:Any => throw new RuntimeException("Next tag is not an open tag, it is " + wrong)
    }
  }

  override def peekOpenClassTag():Class[_] = {
    aliases.forAlias(peekOpenTag)
  }
  override def peekCloseTag():Boolean = {
    peekNextEvent() match {
      case end:EvElemEnd => true
      case _ => false
    }
  }

  override def getCloseTag() = {
    getNextEvent() match {
      case end:EvElemEnd => {}
      case wrong:Any => throw new RuntimeException("Next tag is not a close tag, it is " + wrong)
    }
  }
}



class XMLDataTarget(aliases:XMLAliases, writer:Writer) extends DataTarget {

  private val tagStack = mutable.ListBuffer[(String, Boolean)]()

  private def printWithTabs(s:String, newline:Boolean = true, tabs:Boolean = true) = {
    if (tabs) tagStack.foreach(_ => writer.write("  "))
    writer.write(s)
    if (newline) writer.write("\n")
  }

  def putBoolean(b:Boolean) = printWithTabs(""+b)
  def putByte(i:Byte) = printWithTabs(""+i)
  def putShort(i:Short) = printWithTabs(""+i)
  def putChar(i:Char) = printWithTabs(""+i)
  def putInt(i:Int) = printWithTabs(""+i)
  def putLong(l:Long) = printWithTabs(""+l)
  def putFloat(f:Float) = printWithTabs(""+f)
  def putDouble(d:Double) = printWithTabs(""+d)

  //No tab or newline for strings, we want the tags around the string value
  //to contain only the string value
  def putUTF(s:String) = printWithTabs(s, false, false)

  def openTag(s:String) = {
    printWithTabs("<" + s + ">")
    tagStack.append((s, false))
  }

  def openClassTag(c:Class[_]) = {
    val s = aliases.forClass(c)
    //When opening a string class tag, the contents
    //need to have no whitespace, so don't print a newline
    if (c == classOf[java.lang.String]) {
      printWithTabs("<" + s + ">", false)
      tagStack.append((s, true))
    } else {
      printWithTabs("<" + s + ">")
      tagStack.append((s, false))
    }
  }

  def closeTag() = {
    val tag = tagStack.remove(tagStack.size-1)
    val name = tag._1
    val skipTabs = tag._2

    if (skipTabs) {
      printWithTabs("</" + name +">", true, false)
    } else {
      printWithTabs("</" + name +">")
    }
  }

  def flush() = {}

  def close() = {
    if (!tagStack.isEmpty) throw new RuntimeException("Closed XMLDataTarget with tags still open")
  }
}