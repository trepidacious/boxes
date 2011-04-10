package boxes.persistence

import scala.xml._
import scala.xml.pull._
import scala.io.Source
import collection._
import java.io.Writer


trait DataSource {
//  def get():Int
//  def get(bytes:Array[Byte]):Int
//  def get(bytes:Array[Byte], offset:Int, length:Int):Int

  def getBoolean():Boolean
  def getByte():Byte
  def getShort():Short
  def getChar():Char
  def getInt():Int
  def getLong():Long
  def getFloat():Float
  def getDouble():Double
  def getUTF():String
  def close()

  /**
   * Consume the next open tag, or exception if the
   * next element is not an open tag. Returns the string
   * in the tag
   */
  def getOpenTag():String

  /**
   * Consume the next open class tag, or exception if the
   * next element is not an open class tag. Returns the class
   * in the tag
   */
  def getOpenClassTag():Class[_]

  /**
   * Consume the next open class tag, or exception if the
   * next element is not an open class tag with the specified
   * class
   */
  def getOpenClassTag(c:Class[_])

  /**
   * Get the string from next open tag, or exception if
   * next element is not an open tag
   * Does not consume the tag - leaves it in the source
   */
  def peekOpenTag():String

  /**
   * Get the class from next open class tag, or exception if
   * next element is not an open class tag
   * Does not consume the tag - leaves it in the source
   */
  def peekOpenClassTag():Class[_]

  /**
   * True if next tag is a close tag, false otherwise
   * Does not consume the tag - leaves it in the source
   */
  def peekCloseTag():Boolean

  /**
   * Consume the next close tag, or exception if the
   * next element is not a close tag
   */
  def getCloseTag()
}

/**
 * Output of the most basic data types, whose representation
 * is different between different basic file types. For example,
 * XML will use a standard string representation for Ints, whereas
 * a binary format will
 */
trait DataTarget {
//  def put(i:Int)
//  def put(bytes:Array[Byte])
//  def put(bytes:Array[Byte], offset:Int, length:Int)

  def putBoolean(b:Boolean)
  def putByte(i:Byte)
  def putShort(i:Short)
  def putChar(i:Char)
  def putInt(i:Int)
  def putLong(l:Long)
  def putFloat(f:Float)
  def putDouble(d:Double)
  def putUTF(s:String)
  def openTag(s:String)
  def openClassTag(c:Class[_])
  def closeTag()
  def flush()
  def close()
}

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

trait ValHandler[T] {
  def clazz():Class[_]
  def put(target:DataTarget, t:T)
  def get(source:DataSource):T
}

object ValHandlers {
  implicit object BooleanHandler extends ValHandler[Boolean] {
    override def clazz = classOf[java.lang.Boolean]
    override def put(target:DataTarget, t:Boolean) = target.putBoolean(t)
    override def get(source:DataSource) = source.getBoolean
  }
  implicit object IntHandler extends ValHandler[Int] {
    override def clazz = classOf[java.lang.Integer]
    override def put(target:DataTarget, t:Int) = target.putInt(t)
    override def get(source:DataSource) = source.getInt
  }
  implicit object ShortHandler extends ValHandler[Short] {
    override def clazz = classOf[java.lang.Short]
    override def put(target:DataTarget, t:Short) = target.putShort(t)
    override def get(source:DataSource) = source.getShort
  }
  implicit object ByteHandler extends ValHandler[Byte] {
    override def clazz = classOf[java.lang.Byte]
    override def put(target:DataTarget, t:Byte) = target.putByte(t)
    override def get(source:DataSource) = source.getByte
  }
  implicit object LongHandler extends ValHandler[Long] {
    override def clazz = classOf[java.lang.Long]
    override def put(target:DataTarget, t:Long) = target.putLong(t)
    override def get(source:DataSource) = source.getLong
  }
  implicit object FloatHandler extends ValHandler[Float] {
    override def clazz = classOf[java.lang.Float]
    override def put(target:DataTarget, t:Float) = target.putFloat(t)
    override def get(source:DataSource) = source.getFloat
  }
  implicit object DoubleHandler extends ValHandler[Double] {
    override def clazz = classOf[java.lang.Double]
    override def put(target:DataTarget, t:Double) = target.putDouble(t)
    override def get(source:DataSource) = source.getDouble
  }
  implicit object CharHandler extends ValHandler[Char] {
    override def clazz = classOf[java.lang.Character]
    override def put(target:DataTarget, t:Char) = target.putChar(t)
    override def get(source:DataSource) = source.getChar
  }
  implicit object StringHandler extends ValHandler[String] {
    override def clazz = classOf[java.lang.String]
    override def put(target:DataTarget, t:String) = target.putUTF(t)
    override def get(source:DataSource) = source.getUTF
  }

}